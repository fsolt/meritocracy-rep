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

interplot0 <- function(m, var1, var2, xlab=NULL, ylab=NULL, 
                       seed=324, sims=1000, steps=100, xmin=NA,
                       xmax=NA, labels=NULL, plot=TRUE) {
    require(arm)
    require(ggplot2)
    require(abind)
    set.seed(seed)
    if (class(m)=="list") {
        m.list <- m
        m <- m.list[[1]]
        m.class <- class(m)
        m.sims.list <- lapply(m.list, function(i) arm::sim(i, sims))
        m.sims <- m.sims.list[[1]]
        if (m.class=="lmerMod" | m.class=="glmerMod") {
            for(i in 2:length(m.sims.list)) {
                m.sims@fixef <- rbind(m.sims@fixef, m.sims.list[[i]]@fixef)
                m.sims@ranef[[1]] <- abind(m.sims@ranef[[1]], m.sims.list[[i]]@ranef[[1]], along=1)
            }
        } else {
            stop(paste("Multiply imputed flat models not implemented yet"))
        }
    } else {
        m.class <- class(m)
        m.sims <- arm::sim(m, sims)
    }
    if(var1==var2) var12 <- paste0("I(", var1, "^2)") else var12 <- paste0(var2,":",var1)
    if(m.class!="lmerMod" & m.class!="glmerMod"){
        if (!var12 %in% names(m$coef)) var12 <- paste0(var1,":",var2)
        if (!var12 %in% names(m$coef)) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
        if (is.na(xmin)) xmin <- min(m$model[var2], na.rm=T)
        if (is.na(xmax)) xmax <- max(m$model[var2], na.rm=T)
        coef <- data.frame(fake = seq(xmin, xmax, length.out=steps), coef1 = NA, ub = NA, lb = NA)
        
        for(i in 1:steps) {    
            coef$coef1[i] <- mean(m.sims@coef[,match(var1, names(m$coef))] + 
                                      coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))])
            coef$ub[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                                       coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .975)
            coef$lb[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                                       coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .025)    
        }
    } else {
        if (!var12 %in% unlist(dimnames(m@pp$X)[2])) var12 <- paste0(var1,":",var2)
        if (!var12 %in% unlist(dimnames(m@pp$X)[2])) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
        if (is.na(xmin)) xmin <- min(m@frame[var2], na.rm=T)
        if (is.na(xmax)) xmax <- max(m@frame[var2], na.rm=T)        
        coef <- data.frame(fake = seq(xmin, xmax, length.out=steps), coef1 = NA, ub = NA, lb = NA)
        
        for(i in 1:steps) {   
            coef$coef1[i] <- mean(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                                      coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))])
            coef$ub[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                                       coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .975)
            coef$lb[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
                                       coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .025)    
        }   
    }
    if (plot==TRUE) {
        if(steps>10) {
            coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) + 
                geom_line() + geom_ribbon(aes(ymin=lb, ymax=ub), alpha=.5) +
                theme_bw() + ylab(ylab) + xlab(xlab)
        } else {
            if (is.null(labels)) {
                coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) + 
                    geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) +
                    scale_x_continuous(breaks = seq(min(coef$fake), max(coef$fake), length.out=steps)) +
                    theme_bw() + ylab(ylab) + xlab(xlab)
            } else {
                coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) + 
                    geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) +
                    scale_x_continuous(breaks = seq(min(coef$fake), max(coef$fake), length.out=steps),
                                       labels = labels) +
                    theme_bw() + ylab(ylab) + xlab(xlab)
            } 
        }
        return(coef.plot)
    } else {
        names(coef) <- c(var2, "coef", "ub", "lb")
        return(coef)
    }
}

##### Starting fresh 
# county data
fips_cnty <- read_csv("https://raw.githubusercontent.com/raypereda/fips-county-codes/master/lib/national.txt", 
                      col_types="ccccc") 
names(fips_cnty) <- tolower(gsub(" ", "_", names(fips_cnty)))
fips_cnty$fips <- as.numeric(do.call(paste0, c(fips_cnty[, c(2,3)])))
fips_cnty$county <- tolower(gsub(" County| Parish", "", fips_cnty$county_name))
fips_cnty$county <- gsub(" ", "", fips_cnty$county)

bush04 <- read_tsv("http://bactra.org/election/vote-counts-with-NE-aggregated")
bush04$perc_bush04 <- with(bush04, Bush/(Bush+Kerry+Nader))
names(bush04) <- tolower(names(bush04))
bush04$county <- tolower(gsub(" County| Parish", "", bush04$county))
bush04$county <- gsub("saint", "st.", bush04$county)
bush04$county <- gsub(" ", "", bush04$county)
bush04$county[(bush04$state=="LA"|bush04$state=="MS") & bush04$county=="jeffdavis"] <- "jeffersondavis"
bush04$county[(bush04$state=="ME") & bush04$county=="linc"] <- "lincoln"
bush04$county[(bush04$state=="ME") & bush04$county=="andr"] <- "androscoggin"
bush04$county[(bush04$state=="ME") & bush04$county=="pen-s"] <- "penobscot"
bush04$county[(bush04$state=="MA") & bush04$county=="hamd"] <- "hamden"
bush04$county[(bush04$state=="MA") & bush04$county=="esse"] <- "essex"
bush04$county[(bush04$state=="MA") & bush04$county=="hams"] <- "hampshire"
bush04$county[(bush04$state=="NH") & bush04$county=="graf"] <- "grafton"
bush04$county[(bush04$state=="NY") & bush04$county=="manhattan"] <- "newyork"
bush04$county[(bush04$state=="NY") & bush04$county=="statenisland"] <- "richmond"
bush04$county[(bush04$state=="NY") & bush04$county=="brooklyn"] <- "kings"
bush04$county[(bush04$state=="VT") & bush04$county=="fran"] <- "franklin"
bush04$county[(bush04$state=="VT") & bush04$county=="wins"] <- "windsor"
bush04$county[(bush04$state=="VT") & bush04$county=="addi"] <- "addison"
bush04$county[(bush04$state=="VT") & bush04$county=="gris"] <- "grandisle"
bush04$county[(bush04$state=="VT") & bush04$county=="oran"] <- "orange"
bush04$county[(bush04$state=="VA") & bush04$county=="manassas"] <- "manassascity"
bush04$county[(bush04$state=="VA") & bush04$county=="norton"] <- "nortoncity"


bush04_cnty <- left_join(bush04, fips_cnty)
missing <- bush04_cnty[is.na(bush04_cnty$fips), 1:8] # election results still without fips due to county name inconsistencies
bush04_cnty <- bush04_cnty[!is.na(bush04_cnty$fips), ] # keep (for the moment) only results with fips
remaining <- anti_join(fips_cnty, bush04) %>% arrange(state) # fips without election results

missing$county0 <- missing$county # move county names to a tempvar
missing$county <- NA

states <- unique(missing$state)
states <- states[states != "AK"] # nothing to be done with Alaska election results--no breakdown in data
for(i in 1:length(states)) {
    t.rem <- remaining$county[remaining$state==states[i]] # fips without election results, one state at a time
    missing$county[missing$state==states[i]] <- lapply(missing$county0[missing$state==states[i]], function (ii) agrep(ii, t.rem, value=T, max.distance=.2)) # find matches to county name by state
}
missing$county <- unlist(lapply(missing$county, function(ii) ii[1])) # use closest match to county name
missing <- left_join(missing, fips_cnty) # now merge; some results still without fips in Maine, otherwise good
missing$county0 <- NULL # drop tempvar

bush04_cnty %<>% rbind(missing) %>% select(fips, perc_bush04)

acs0509 <- read_csv("data/acs0509-counties.csv")
names(acs0509) <- tolower(names(acs0509))
acs0509 <- mutate(acs0509,
                  fips = as.numeric(gsub("05000US", "", geoid)),
                  gini_cnty = b19083_001e,
                  income_cnty = b19013_001e/10000,
                  black_cnty = b02001_003e/b02001_001e,
                  pop_cnty = b02001_001e/10000)
cnty_data <- select(acs0509, fips:pop_cnty)
cnty_data %<>% left_join(bush04_cnty)


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
p2005x.w <- p2005x[p2005x$white==1, -c(9)]


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



# Combine 2005 and 2006
p1x <- rbind(p2005x, p2006x)

pew1.cnty <- pew1 %>% group_by(fips) %>% summarise_each(funs(mean)) %>%
    select(matches("cnty|fips|bush"))   # I've confirmed that there's just one value per county 

acs0509 <- read_csv("b19083_001e.csv") # income distribution data from five-year ACS
acs0509$fips <- as.numeric(gsub("05000US", "", acs0509$geoid))
acs0509$gini_cnty <- acs0509$b19083_001e
acs0509c <- acs0509[ , 7:8]

p1x <- left_join(p1x, pew1.cnty)
p1x$ginicnty <- NULL
p1x <- left_join(p1x, acs0509c)

p1x.w <- p1x[p1x$white==1, -c(9)]
p1x.w.sc <- p1x[p1x$white==1, c(1:3)]


# Rowwise deletion
# t1m1.05 <- glmer(formula = rej_merit~gini_cnty+income+gini_cnty:income+
#                      income_cnty+black_cnty+perc_bush04+pop_cnty+
#                      educ+age+male+unemp+union+partyid+ideo+attend+
#                      (1+income|fips),
#                  data=p2005x.w,family=binomial(link="logit"))
# 
# t1m1.06 <- glmer(formula = rej_merit~gini_cnty+income+gini_cnty:income+
#                      income_cnty+black_cnty+perc_bush04+pop_cnty+
#                      educ+age+male+unemp+union+partyid+ideo+attend+
#                      (1+income|fips),
#                  data=p2006x.w,family=binomial(link="logit"))

b1m1.r <- glmer(formula = rej_merit~gini_cnty+income+gini_cnty:income+
                    income_cnty+black_cnty+perc_bush04+pop_cnty+
                    educ+age+male+unemp+union+partyid+ideo+attend+
                    (1+income|fips),
                data=p1x.w, family=binomial(link="logit"))


# Multiply impute missing values with mi
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
b1m1.mi <- with(p1x.w.mi.list2, 
                glmer(formula = rej_merit~gini_cnty+income+gini_cnty:income+
                          income_cnty+black_cnty+perc_bush04+pop_cnty+
                          educ+age+male+unemp+union+partyid+ideo+attend+
                          (1+income|fips), family=binomial(link="logit")))
b1m1.mi.fe <- MIextract(b1m1.mi, fun=fixef) # https://books.google.com/books?id=EbLrQrBGid8C&pg=PA384
b1m1.mi.vars <- MIextract(b1m1.mi, fun=vcov)
b1m1.mi.vars2 <- list()
b1m1.mi.vars2 <- lapply(b1m1.mi.vars, as.matrix)
b1m1.mi.res <- MIcombine(b1m1.mi.fe, b1m1.mi.vars2)
summary(b1m1.mi.res)

b1m1.mi.plot <- interplot(b1m1.mi, "gini_cnty", "income", steps=9,
                          labels = t$inc_labels,
                          xlab = "Family Income",
                          ylab = "County Income Inequality") 
b1m1.mi.plot <- b1m1.mi.plot + 
    geom_hline(yintercept=0, colour="grey80", linetype="dashed")
ggsave(file="b1m1_mi_plot.pdf", plot=b1m1.mi.plot, width=8, height=5.25)

b1m1.mi.coef <- interplot(b1m1.mi, "gini_cnty", "income", steps=9, plot=F)

# Pew 2007 (to evaluate mixing DVs)
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


# Pew 2011 Generational Change has FIPS (at least in Pew version; check Roper 2011std)



