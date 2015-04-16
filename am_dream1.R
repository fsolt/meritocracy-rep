library(readr)
library(lme4)
library(arm)
library(ggplot2)
library(plyr)
library(dplyr)
library(haven)
library(magrittr)
library(maps)
library(mi)
library(mitools)

interplot <- function(m, var1, var2, xlab=NULL, ylab=NULL, 
                      seed=324, sims=1000, steps=100, plot=TRUE) {
    require(arm)
    require(ggplot2)
    set.seed(seed)
    m.sims <- arm::sim(m, sims)
    var12 <- paste0(var2,":",var1)
    if(class(m)!="lmerMod" & class(m)!="glmerMod"){
        if (!var12 %in% names(m$coef)) var12 <- paste0(var1,":",var2)
        if (!var12 %in% names(m$coef)) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
        coef <- data.frame(fake = seq(min(m$model[var2], na.rm=T), max(m$model[var2], na.rm=T), length.out=steps), coef1 = NA, ub = NA, lb = NA)
        
        for(i in 1:steps) {   
            coef$coef1[i] <- mean(m.sims@coef[,match(var1, names(m$coef))] + 
                                      coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))])
            coef$ub[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                                       coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .975)
            coef$lb[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                                       coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .025)    
        }
    }else{
        if (!var12 %in% unlist(dimnames(m@pp$X)[2])) var12 <- paste0(var1,":",var2)
        if (!var12 %in% unlist(dimnames(m@pp$X)[2])) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
        coef <- data.frame(fake = seq(min(m@frame[var2], na.rm=T), max(m@frame[var2], na.rm=T), length.out=steps), coef1 = NA, ub = NA, lb = NA)
        
        for(i in 1:steps) {   
            coef$coef1[i] <- mean(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                                      coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))])
            coef$ub[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                                       coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .975)
            coef$lb[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                                       coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .025)    
        }   
    }
    if(plot==TRUE) {
        if(steps>5) {
            coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
                geom_line() + geom_ribbon(aes(ymin=lb, ymax=ub), alpha=.5) + theme_bw()+
                ylab(ylab) + xlab(xlab)
        } else {
            coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
                geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) + 
                scale_x_continuous(breaks = 0:steps) + theme_bw()+
                ylab(ylab) + xlab(xlab)
        }
        return(coef.plot)
    } else {
        names(coef) <- c(var2, "coef", "ub", "lb")
        return(coef)
    }
}



### Read data: http://thedata.harvard.edu/dvn/dv/ajps/faces/study/StudyPage.xhtml?studyId=93632&tab=files
pew1 <- read_tsv("study_26584/Meritocracy Replication Data - Table 1.tab") # Combined Pew surveys
pew1.w <- pew1[pew1$white==1,]  # Only white respondents

pew2 <- read_tsv("study_26584/2006 Pew News Interest Survey_Table 2 Data in Stata Format.tab") # Only 2006 Pew survey
pew2.w <- pew2[pew2$white==1,] # Only white respondents

pew3 <- read_tsv("study_26584/Meritocracy Replication Data - Table 3.tab") # Only 2006 Pew survey
pew3.w <- pew3[pew3$white==1,] # Only white respondents

pew1$year <- 2005
pew1$year[pew1$survid2006==1] <- 2006
pew1$year[pew1$survid2007==1] <- 2007
pew1$year[pew1$survid2009==1] <- 2009

### Analyses
# Table 1, Model 1
t1m1 <- glmer(formula=meritocracy~ginicnty+income_i+ginicnty:income_i+income_cnty+black_cnty+
                  perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
                  ideo_i+attend_i+survid2006+survid2007+survid2009+(1+income_i|fips),
              data=pew1.w,family=binomial(link="logit"))
summary(t1m1)

interplot(t1m1, "ginicnty", "income_i")
t1m1.gc.coef <- interplot(t1m1, "ginicnty", "income_i", plot=F)

# predicted probabilities with confidence intervals (see http://glmm.wikidot.com/faq under lme4)
newdat <- pew1 %>% summarise_each(funs(mean))
newdat <- newdat[rep(1, 200), ]
newdat$ginicnty <- rep(seq(min(pew1$ginicnty, na.rm=T), max(pew1$ginicnty, na.rm=T), length.out = 100), times = 2) # Full observed range of ginicnty
newdat$income_i <- c(rep(quantile(pew1$income_i, .001), 100), rep(max(pew1$income_i), 100)) # One imputed value falls below theoretical range of variable, use actual range instead by taking 0.1 percentile value
newdat$fips <- NULL # mean FIPs meaningless (and causes error); not used anyway with re.form=NA
newdat$inc[1:100] <- "low"
newdat$inc[101:200] <- "high"

mm <- model.matrix(terms(t1m1), newdat)
newdat$pred <- predict(t1m1, newdat, re.form=NA, type="link")
pvar1 <- diag(mm %*% tcrossprod(vcov(t1m1), mm)) # FE variance only (no RE variance)
newdat <- data.frame(
    newdat,
    pp = invlogit(newdat$pred),
    pplo = invlogit(newdat$pred+qnorm(.025)*sqrt(pvar1)),
    pphi = invlogit(newdat$pred+qnorm(.975)*sqrt(pvar1))
)

t1m1.pp <- ggplot(newdat, aes(x=ginicnty, y=pp, colour=inc)) + geom_line() + 
    labs(x = "County Income Inequality", 
         y = "Probability of Rejecting Meritocracy") +
    geom_ribbon(aes(ymin = pplo, ymax = pphi, fill=inc, linetype=NA), 
                alpha = .25) +
    geom_text(aes(.5, .17, label = "Highest Income", colour="high"), size=4.5) +
    geom_text(aes(.25, .32, label = "Lowest Income", colour="low"), size=4.5) +
    scale_colour_grey(end=.6) + scale_fill_grey(end=.6) + 
    theme_bw() + theme(legend.position="none")
ggsave(file="t1m1pp.pdf", width=8, height=5.25)



# Table 2
t2m1 <- glmer(formula=divided~ginicnty05_09_01+medhinc0610cnty_01+pctblk0610cnty_01+
                  totpop0610cnty_01+pbush_01+income_i_01+Age+gender+education_01+
                  partyid_01+ideology_01+religattend_01+union+unemployed+(1|fips),
              data=pew2, family=binomial(link="logit"))
summary(t2m1)


# Table 3
t3m1 <- glmer(formula=havenot2~ginicnty+income_i+ginicnty:income_i+income_cnty+black_cnty+
                   perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
                   ideo_i+attend_i+(1+income_i|fips), 
              data=pew3, family=binomial(link="logit"))
summary(t3m1)

# Appendix B Table 1, Model 1
pew1.0506 <- pew1[pew1$year<=2006, ] 
b1m1 <- glmer(formula=meritocracy~ginicnty+income_i+ginicnty:income_i+income_cnty+black_cnty+
                  perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
                  ideo_i+attend_i+survid2006+(1+income_i|fips),
              data=pew1.0506, family=binomial(link="logit"))
summary(b1m1)



# Replicate with other Pew data
read.roper <- function(data, codebook, dir="", last.var="weight") {
    require(readr)
    if(!file.exists(data) & dir!="") data <- paste0(dir, "/", data)
    if(!file.exists(codebook) & dir!="") codebook <- paste0(dir, "/", codebook)
    system(paste0("java -jar pdfbox-app-1.8.9.jar ExtractText \"", codebook, "\""))
    cb <- readLines(gsub(".pdf", ".txt", codebook))
    start.line <- grep("Variable\\s+Rec\\s+.*", cb, ignore.case=T)
    stop.line <- grep(paste0(last.var, "\\s+[12].*"), cb, ignore.case=T)
    cb <- cb[start.line:stop.line]
    cb1 <- gsub("\\s+", ",", cb)
    t <- tempfile()
    writeLines(cb1, t)
    t1 <- read_csv(t)
    t1 <- t1[t1$Rec=="1", c(1, 3:4)]
    names(t1) <- tolower(names(t1))
    rownames(t1) <- NULL
    read_fwf(data, fwf_positions(t1$start, t1$end, t1$variable))
}



pew.data <- data.frame(name = c("pew1999t", "pew2004t", "pew2005n", "pew2006i", 
                                "pew2007r", "pew2007r", "pew2010p", "pew2011p", 
                                "pew2011s", "pew2011t", "pew2012s", "pew2014t"), 
                       year = c("1999", "2004", "2005", "2006", "2007", "2007",
                                "2010", "2011", "2011", "2011", "2012", "2014"),
                       data = c("typo99.dat", "P04TYPO.dat", 
                                "p12nii.dat", "P06IMM.dat", "p2007rel_ah.dat", 
                                "p2007rel_us.dat", "p201009pi.dat", 
                                "p201112pol.dat", "p2001sdt09.dat", 
                                "p2011typo.dat", "p2012sdt07.dat", "p2014typo.dat"),
                       cb = c("uspew1999-typo.pdf", "USPEW2004-TYPO.pdf", 
                              "uspew2005-12nii.pdf", "USPEW2006-IMM.pdf", 
                              "uspew2007-rel.pdf", "uspew2007-rel.pdf", 
                              "uspew2010-09pi.pdf", "uspew2011-12pol.pdf", 
                              "uspew2011-sdt09.pdf", "uspew2011-typo.pdf", 
                              "uspew2012-sdt07.pdf", "uspew2014-typo.pdf"), 
                       stringsAsFactors = F)

p1999 <- read.roper(pew.data$data[1], pew.data$cb[1], dir="Roper")



# County-level vars
pew1.cnty <- pew1 %>% group_by(fips) %>% summarise_each(funs(mean, n_distinct)) %>%
                select(matches("cnty|fips|bush"))

summary(pew1.cnty) # confirm one distinct value per county

pew1.cnty <- pew1 %>% group_by(fips) %>% summarise_each(funs(mean)) %>%
    select(matches("cnty|fips|bush"))


# Pew 2007
#p2007 <- read.roper(pew.data$data[6], pew.data$cb[6], dir="Roper", last.var="pvote04b")
p2007 <- read_sav("Pew/dataset_Religious_Landscape_Survey_Data/Religious Landscape Survey Data - Continental US.sav")

p2007x <- data.frame(
    resp = p2007$psraid,
#    fips = p2007$qfips,
    state = as.numeric(p2007$state),
    rej_merit = ifelse(p2007$q5c<=2, p2007$q5c-1, NA),
    income = ifelse(p2007$income<=9, p2007$income, NA), # 1 to 9
    educ = ifelse(p2007$educ<=7, p2007$educ, NA), # 1 to 7
    age = ifelse(p2007$age<99, p2007$age, NA),
    male = ifelse(p2007$sex==1, 1, 0),
    white = ifelse(p2007$race==1 & p2007$hisp!=1, 1, 0),
#    union = ifelse(p2007$labor<=3, 1, ifelse(p2007$labor==4, 0, NA)), # not asked this survey
    ideo = 6 - ifelse(p2007$ideo<=5, p2007$ideo, NA), # 1 to 5
    attend = 7 - ifelse(p2007$q20<=6, p2007$q20, NA) # 1 to 6
)
p2007x$partyid <- mapvalues(p2007$party, 
                            from = c(1:5, 9), 
                            to = c(5, 1, 3, 3, 3, NA))
p2007x$partyid[p2007$partyln==1] <- 4
p2007x$partyid[p2007$partyln==2] <- 2

# p2007x$unemp <- ifelse(p2007$employ==3 & p2007$employ2==4, 1, 0) # not asked this survey
# p2007x$unemp[p2007$employ==9 | p2007$employ2==9] <- NA

p2007x$year <- 2007
p2007x.w <- p2007x[p2007x$white==1, -c(8)]

t1m1.07.flat <- glm(formula = rej_merit~income+
                       educ+age+male+partyid+ideo+attend,
                   data=p2007x.w, family=binomial(link="logit"))

t1m1.07.x <- glmer(formula = rej_merit~income+
                  educ+age+male+partyid+ideo+attend+
                  (1+income|state),
            data=p2007x.w, family=binomial(link="logit"))

t1m1.06.x <- glmer(formula = rej_merit~income+
                       educ+age+male+partyid+ideo+attend+
                       (1|state),
                   data=p2006x.w, family=binomial(link="logit"))

# missing county info; I requested via Pew contact webpage 4/12;
#   called 4/15: Charles in Communications says he'll email the
#   form to start "the process"

# Pew 2011 Generational Change has FIPS (at least in Pew version; check Roper 2011std)



#####Starting from raw data
# Pew 2005 News Interest Index
p2005 <- read_sav("Pew/Dec05/Dec05c.sav")

p2005x <- data.frame(
    resp = p2005$resp,
    fips = as.numeric(p2005$qfips),
    state = floor(as.numeric(p2005$qfips)/1000),
    rej_merit = ifelse(p2005$q14k<=2, 0, ifelse(p2005$q14k>4, NA, 1)),
    income = ifelse(p2005$income<=9, p2005$income, NA), # 1 to 9
    educ = ifelse(p2005$educ<=7, p2005$educ, NA), # 1 to 7
    age = ifelse(p2005$age<99, p2005$age, NA),
    male = ifelse(p2005$sex==1, 1, 0),
    white = ifelse(p2005$race==1 & p2005$hisp!=1, 1, 0),
    union = ifelse(p2005$labor<=3, 1, ifelse(p2005$labor==4, 0, NA)),
    ideo = 6 - ifelse(p2005$ideo<=5, p2005$ideo, NA), # 1 to 5
    attend = 7 - ifelse(p2005$attend<=6, p2005$attend, NA) # 1 to 6
)
p2005x$partyid <- mapvalues(p2005$party, 
                            from = c(1:5, 9), 
                            to = c(5, 1, 3, 3, 3, NA))
p2005x$partyid[p2005$partyln==1] <- 4
p2005x$partyid[p2005$partyln==2] <- 2

p2005x$unemp <- ifelse((p2005$employ==3 | p2005$employ==9), NA, 0) # No employ2 in survey

# p2005x$income_b <- mapvalues(p2005$income, 
#                              from = c(1:10),
#                              to = c(5, 15, 25, 35, 45, 62.5, 87.5, 125, 175, NA))
# p2005x$educ_b <- mapvalues(p2005$educ,
#                            from = c(1:7, 9),
#                            to = c(4, 10, 12, 14, 14, 16, 18, NA))
# p2005x$attend_b <- mapvalues(p2005$attend,
#                              from = c(1:6, 9),
#                              to = c(102, 52, 18, 5, 2, 0, NA))

p2005x$year <- 2005

# Pew 2006 Immigration Survey
p2006 <- read_sav("Pew/Immigration06/Mar06 Immigrationc.sav")
p2006 <- p2006[p2006$xcode > 5, ]

p2006x <- data.frame(
               resp = p2006$resp,
               fips = p2006$qfips,
               state = floor(p2006$qfips/1000),
               rej_merit = ifelse(p2006$q8b<=2, p2006$q8b-1, NA),
               income = ifelse(p2006$income<=9, p2006$income, NA), # 1 to 9
               educ = ifelse(p2006$educ<=7, p2006$educ, NA), # 1 to 7
               age = ifelse(p2006$age<99, p2006$age, NA),
               male = ifelse(p2006$sex==1, 1, 0),
               white = ifelse(p2006$race==1 & p2006$hisp!=1, 1, 0),
               union = ifelse(p2006$labor<=3, 1, ifelse(p2006$labor==4, 0, NA)),
               ideo = 6 - ifelse(p2006$ideo<=5, p2006$ideo, NA), # 1 to 5
               attend = 7 - ifelse(p2006$attend<=6, p2006$attend, NA) # 1 to 6
               )
p2006x$partyid <- mapvalues(p2006$party, 
                            from = c(1:5, 9), 
                            to = c(5, 1, 3, 3, 3, NA))
p2006x$partyid[p2006$partyln==1] <- 4
p2006x$partyid[p2006$partyln==2] <- 2

p2006x$unemp <- ifelse(p2006$employ==3 & p2006$employ2==4, 1, 0)
p2006x$unemp[p2006$employ==9 | p2006$employ2==9] <- NA

# p2006x$income_b <- mapvalues(p2006$income, 
#                              from = c(1:10),
#                              to = c(5, 15, 25, 35, 45, 62.5, 87.5, 125, 175, NA))
# p2006x$educ_b <- mapvalues(p2006$educ,
#                            from = c(1:7, 9),
#                            to = c(4, 10, 12, 14, 14, 16, 18, NA))
# p2006x$attend_b <- mapvalues(p2006$attend,
#                              from = c(1:6, 9),
#                              to = c(102, 52, 18, 5, 2, 0, NA))

p2006x$year <- 2006
p2006x.w <- p2006x[p2006x$white==1, -c(9)]


p1x <- rbind(p2005x, p2006x)

pew1.cnty <- pew1 %>% group_by(fips) %>% summarise_each(funs(mean)) %>%
    select(matches("cnty|fips|bush"))   # I've confirmed that there's just one value per county 

acs0509 <- read_csv("b19083_001e.csv")
acs0509$fips <- as.numeric(gsub("05000US", "", acs0509$geoid))
acs0509$gini_cnty <- acs0509$b19083_001e
acs0509c <- acs0509[ , 7:8]

p1x <- left_join(p1x, pew1.cnty)
p1x$ginicnty <- NULL
p1x <- left_join(p1x, acs0509c)

p1x.w <- p1x[p1x$white==1, -c(9)]
p1x.w.sc <- p1x[p1x$white==1, c(1:3)]

# Rowwise deletion
b1m1.r <- glmer(formula = rej_merit~gini_cnty+income+gini_cnty:income+
                        income_cnty+black_cnty+perc_bush04+pop_cnty+
                        educ+age+male+unemp+union+partyid+ideo+attend+
                        (1+income|fips),
                    data=p1x.w, family=binomial(link="logit"))


# mi
p1x.w.info <- mi.info(p1x.w)
p1x.w.info <- update(p1x.w.info, "include", list(fips=F, state=F))
p1x.w.info <- update(p1x.w.info, "type", list(
    income = "ordered-categorical",
    educ = "ordered-categorical",
    attend  = "ordered-categorical"))
p1x.w.pre <- mi.preprocess(p1x.w, p1x.w.info)

p1x.w.mi <- mi(p1x.w.pre, n.imp=10, n.iter=30, seed=324, max.minutes=60)

p1x.w.mi.list <- mi.completed(p1x.w.mi)
p1x.w.mi.list2 <- imputationList(p1x.w.mi.list)
t1m1.mi.list2 <- with(p1x.w.mi.list2, 
    glmer(formula = rej_merit~gini_cnty+income+gini_cnty:income+
              income_cnty+black_cnty+perc_bush04+pop_cnty+
              educ+age+male+unemp+union+partyid+ideo+attend+
              (1+income|fips), family=binomial(link="logit")))
t1m1.mi.fe <- MIextract(t1m1.mi.list2, fun=fixef) # https://books.google.com/books?id=EbLrQrBGid8C&pg=PA384
t1m1.mi.vars <- MIextract(t1m1.mi.list2, fun=vcov)
t1m1.mi.vars2 <- list()
for(i in 1:10) {
    t1m1.mi.vars2[[i]] <- as.matrix(t1m1.mi.vars[[i]])
}
t1m1.mi.res <- MIcombine(t1m1.mi.fe, t1m1.mi.vars2)
summary(t1m1.mi.res)

t <- arm::sim(t1m1.mi.res)




library(Zelig)
library(ZeligMultilevel)
t1m1.mi.zelig <- zelig(formula = rej_merit~gini_cnty+income+gini_cnty:income+
                      income_cnty+black_cnty+perc_bush04+pop_cnty+
                      educ+age+male+unemp+union+partyid+ideo+attend+
                      tag(1+income|fips),
                  data=p1x.w.mi.list, model="logit.mixed")

interplot(t1m1.mi.list[[2]], "gini_cnty", "income")

# mice
library(mice)
p1x.w.mice <- mice(p1x.w)

t1m1x.w <- with(p1x.w, glmer(formula = rej_merit~gini_cnty+income0+gini_cnty:income0+
                     income_cnty+black_cnty+perc_bush04+pop_cnty+
                     educ0+age+male+unemp+union+partyid+ideo0+attend0+
                     (1+income0|fips),
                 data=p1x.w.mice, family=binomial(link="logit")))
summary(t1m1x.w)


#Amelia
library(Amelia)
p1x.w.t <- log1p(p1x.w) # preprocess the data
p1x.w.t.amelia <- amelia(p1x.w.t, seed=324)
p1x.w.amelia <- p1x.w.t.amelia # postprocess the imputations
p1x.w.amelia$imputations <- lapply(p1x.w.amelia$imputations, expm1) # postprocess cont.
p1x.w.amelia$imputations <- lapply(p1x.w.amelia$imputations, function(i) {
    i[4] <- p1x.w[4]
}) # postprocess cont. 


t1m1.am.zelig <- zelig(formula = rej_merit~gini_cnty+income+gini_cnty:income+
                           income_cnty+black_cnty+perc_bush04+pop_cnty+
                           educ+age+male+unemp+union+partyid+ideo+attend+
                           tag(1+income|fips),
                       data=p1x.w.amelia, model="logit.mixed")




t1m1.amelia <- lapply(p1x.w.amelia$imputations, function(i) {
    glmer(formula = rej_merit~gini_cnty+income+gini_cnty:income+
              income_cnty+black_cnty+perc_bush04+pop_cnty+
              educ+age+male+unemp+union+partyid+ideo+attend+
              (1+income|fips),
          data=i, family=binomial(link="logit"))
})



t1m1.amelia1 <- glmer(formula = rej_merit~gini_cnty+income+gini_cnty:income+
                          income_cnty+black_cnty+perc_bush04+pop_cnty+
                          educ+age+male+unemp+union+partyid+ideo+attend+
                          (1+income|fips),
                      data=p1x.w.amelia$imputations$imp1 , family=binomial(link="logit"))



t1m1.amelia <- with(p1x.w, glmer(formula = rej_merit~gini_cnty+income0+gini_cnty:income0+
                                 income_cnty+black_cnty+perc_bush04+pop_cnty+
                                 educ0+age+male+unemp+union+partyid+ideo0+attend0+
                                 (1+income0|fips),
                             data=p1x.w.mice, family=binomial(link="logit")))



pew1.cnty <- pew1 %>% group_by(fips) %>% summarise_each(funs(mean)) %>%
    select(matches("cnty|fips|bush"))

acs0509 <- read_csv("b19083_001e.csv")
acs0509$fips <- as.numeric(gsub("05000US", "", acs0509$geoid))
acs0509$gini_cnty <- acs0509$b19083_001e
acs0509c <- acs0509[ , 7:8]

p2006x <- left_join(p2006x, pew1.cnty)
p2006x <- left_join(p2006x, acs0509c)

p2006x.w <- p2006x[p2006x$white==1, ]

# Table 1, Model 1 redo
t1m1.06 <- glmer(formula = rej_merit~gini_cnty+income0+gini_cnty:income0+
                       income_cnty+black_cnty+perc_bush04+pop_cnty+
                       educ0+age+male+unemp+union+partyid+ideo0+attend0+
                       (1+income0|fips),
                   data=p2006x, family=binomial(link="logit"))
summary(t1m1.06)

t1m1.06.w <- glmer(formula = rej_merit~gini_cnty+income0+gini_cnty:income0+
                       income_cnty+black_cnty+perc_bush04+pop_cnty+
                       educ0+age+male+unemp+union+partyid+ideo0+attend0+
                       (1+income0|fips),
              data=p2006x.w,family=binomial(link="logit"))
summary(t1m1.06.w)

interplot(t1m1.06.w, "gini_cnty", "income0")

t1m1.06.w2 <- glmer(formula = rej_merit~gini_cnty+income_b+gini_cnty:income_b+
                       income_cnty+black_cnty+perc_bush04+pop_cnty+
                       educ_b+age+male+unemp+union+partyid+ideo+attend_b+
                       (1|fips),
                   data=p2006x.w,family=binomial(link="logit"))
summary(t1m1.06.w2)

interplot(t1m1.06.w2, "gini_cnty", "income_b")


summary(p2006x.w)
