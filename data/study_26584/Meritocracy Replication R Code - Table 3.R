##===========================================##
##   False Consciousness or Class Awareness  ##
##   Newman, Johnston, and Lown              ##
##                                           ##
##   Replication R File (Table 3)            ##
##   Version:  6/23/2014                     ##
##   Prepared by:  Patrick Lown              ##
##===========================================##

##  IMPORTANT NOTE:  
##  Due to a change to the Zelig package, the method of model approximation has changed.
##  The models presented in Table 3 in the article were fit with the Laplace approximation, 
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
    
    pew_data3<-read.dta("Meritocracy Replication Data - Table 3.dta")

##-------------##
##  Analyses   ##
##-------------##

    ##  Model 1 (Whites ONLY)
        pew_data3<-pew_data3[pew_data3$white==1,]  #Subset out nonwhites
        z.out <- zelig(formula=havenot2~ginicnty+income_i+ginicnty*income_i+income_cnty+black_cnty+
                               perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
                               ideo_i+attend_i+tag(1+income_i|fips), data=pew_data3,model="logit.mixed")
        summary(z.out)









