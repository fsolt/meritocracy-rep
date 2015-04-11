library(readr)
library(lme4)

### Read data
pew1 <- read_tsv("study_26584/Meritocracy Replication Data - Table 1.tab") # Combined Pew surveys
pew1 <- pew1[pew1$white==1,]  # Only white respondents

pew2 <- read_tsv("study_26584/2006 Pew News Interest Survey_Table 2 Data in Stata Format.tab") # Only 2006 Pew survey
pew2 <- pew2[pew2$white==1,] # Only white respondents

pew3 <- read_tsv("study_26584/Meritocracy Replication Data - Table 3.tab") # Only 2006 Pew survey
pew3 <- pew3[pew3$white==1,] # Only white respondents

pew1$year <- 2005
pew1$year[pew1$survid2006==1] <- 2006
pew1$year[pew1$survid2007==1] <- 2007
pew1$year[pew1$survid2009==1] <- 2009

### Analyses
# Table 1, Model 1
t1m1 <- glmer(formula=meritocracy~ginicnty+income_i+ginicnty:income_i+income_cnty+black_cnty+
                  perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
                  ideo_i+attend_i+survid2006+survid2007+survid2009+(1+income_i|fips),
              data=pew1,family=binomial(link="logit"))
summary(t1m1)

interplot(t1m1, "ginicnty", "income_i")

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

roper.codebook <- function(file) {
    require(readr)
    system(paste0("java -jar pdfbox-app-1.8.9.jar ExtractText \"", file, "\""))
    cb <- readLines(gsub(".pdf", ".txt", file))
    start.line <- grep("Variable\\s+Rec.*", cb, ignore.case=T)
    stop.line <- grep("weight\\s+[12].*", cb, ignore.case=T)
    cb <- cb[start.line:stop.line]
    cb1 <- gsub("\\s+", ",", cb)
    t <- tempfile()
    writeLines(cb1, t)
    t1 <- read_csv(t)
    t1 <- t1[t1$Rec=="1", c(1, 3:4)]
    names(t1) <- tolower(names(t1))
    rownames(t1) <- NULL
    t1
}
cb2005 <- roper.codebook("Roper/uspew2005-12nii.pdf")                 
cb1999 <- roper.codebook("Roper/uspew1999-typo.pdf")
p2005 <- read_fwf("Roper/p12nii.dat", fwf_positions(cb2005$start, cb2005$end, cb2005$variable))

datasets <- list.files("Roper/", ".pdf")
datasets <- data.frame(cb = c(datasets[1:5], datasets[5:11]), stringsAsFactors = F)
t <- data.frame(data = list.files("Roper/", ".dat"), stringsAsFactors = F)
datasets <- cbind(datasets, t)

pew.data <- data.frame(name = c("pew1999t", "pew2004t", "pew2005n", "pew2006i", 
                                "pew2007r", "pew2007r", "pew2010p", "pew2011p", 
                                "pew2011s", "pew2011t", "pew2012s", "pew2014t"), 
                       year = c("1999", "2004", "2005", "2006", "2007", "2007",
                                "2010", "2011", "2011", "2011", "2012", "2014"),
                       data = c("typo99.dat", "P04TYPO.dat", "p201009pi.dat",
                                "p201112pol.dat", "p2001sdt09.dat", "p2011typo.dat", 
                                "p2012sdt07.dat", "p2014typo.dat"), 
                       cb = c("uspew1999-typo.pdf", "USPEW2004-TYPO.pdf", 
                              "uspew2005-12nii.pdf", "USPEW2006-IMM.pdf", 
                              "uspew2007-rel.pdf", "uspew2007-rel.pdf", 
                              "uspew2010-09pi.pdf", "uspew2011-12pol.pdf", 
                              "uspew2011-sdt09.pdf", "uspew2011-typo.pdf", 
                              "uspew2012-sdt07.pdf", "uspew2014-typo.pdf"), 
                       stringsAsFactors = F)

