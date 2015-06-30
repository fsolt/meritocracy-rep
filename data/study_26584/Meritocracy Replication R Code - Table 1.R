##===========================================##
##   False Consciousness or Class Awareness  ##
##   Newman, Johnston, and Lown              ##
##                                           ##
##   Replication R File (Table 1)            ##
##   Version:  6/23/2014                     ##
##   Prepared by:  Patrick Lown              ##
##===========================================##

##  IMPORTANT NOTE:  
##  Due to a change to the Zelig package, the method of model approximation has changed.
##  The models presented in Table 1 in the article were fit with the Laplace approximation, 
##  whereas the models found in this replication file are estimated with a maximum likelihood
##  estimator.  The coefficients will therefore be slightly different, but the signs, significance, 
##  and effect sizes remain the same.

library(Zelig)
library(lme4)
library(ZeligMultilevel)
library(foreign)

##-------------##
##  Read Data  ##
##-------------##
    #Set working directory to file location
    setwd("C:/Users/Patrick/Desktop")
    
    pew_data<-read.dta("Meritocracy Replication Data - Table 1.dta")

##-------------##
##  Analyses   ##
##-------------##

    ##  Model 1 (Whites ONLY) -- Not converging
        pew_data1<-pew_data[pew_data$white==1,]  #Subset out nonwhites
        
        z.out <- glmer(formula=meritocracy~ginicnty+income_i+ginicnty*income_i+income_cnty+black_cnty+
                               perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
                               ideo_i+attend_i+survid2006+survid2007+survid2009+(1+income_i|fips),
                               data=pew_data1,family=binomial(link="logit"))
        summary(z.out)

    ##  Model 2 (Nonwhites ONLY)
        pew_data2<-pew_data[pew_data$white==0,]  #Subset out whites

        z.out <- glmer(formula=meritocracy~ginicnty+income_i+ginicnty*income_i+income_cnty+black_cnty+
                               perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
                               ideo_i+attend_i+survid2006+survid2007+survid2009+(1+income_i|fips),
                               data=pew_data2,family=binomial(link="logit"))
        summary(z.out)


