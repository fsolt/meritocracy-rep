library(readr)
library(haven)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(mi)
library(betareg)
library(truncnorm)
library(mitools)
library(lme4)
library(beepr)

### Have vs. have-nots 
# questions asked, with fips, in *every year* from 2005-2009, though NJL uses only 2006

hhn_format <- function(df, cfips, dh, hn) {
    surv <- str_replace(deparse(substitute(df)), "hhn", "20")
    all_vars <- c("income", "educ", "age", "sex", "racethn", "race", "hisp", 
                  "labor", "ideo", "attend", "employ", "party", "partyln")
    vars <- c(cfips, dh, hn, all_vars[all_vars %in% names(df)])
    df %<>% select(one_of(vars))
    names(df)[1:3] <- c("cfips", "dh", "hn")
    df %<>% mutate(
        fips = as.integer(cfips),
        state = floor(fips/1000),
        div_hhn = ifelse(dh==1, 1, ifelse(dh==2, 0, NA)),
        have_not = ifelse(hn==2, 1, ifelse(hn==1, 0, NA)),
        income = as.integer(ifelse(income<=9, income, NA)), # 1 to 9
        educ = as.integer(ifelse(educ<=7, educ, NA)), # 1 to 7
        age = ifelse(age<99, age, NA),
        male = ifelse(sex==1, 1, 0),
        ideo = as.integer(6 - ifelse(ideo<=5, ideo, NA)), # 1 to 5
        partyid = ifelse(party==1, 5, ifelse(party==2, 1, ifelse(partyln==1, 4, ifelse(partyln==2, 2, NA))))
    )
    if (names(df) %>% str_detect("racethn") %>% any()) {
        df %<>% mutate(white = ifelse(racethn==1, 1, 0))
    } else df %<>% mutate(white = ifelse(race==1 & hisp!=1, 1, 0))
    if (names(df) %>% str_detect("labor") %>% any()) {
        df %<>% mutate(union = ifelse(labor<=3, 1, ifelse(labor==4, 0, NA)))
    } else df %<>% mutate(union = NA)
    if (names(df) %>% str_detect("attend") %>% any()) {
        df %<>% mutate(attend = 7 - ifelse(attend<=6, attend, NA))
    } else df %<>% mutate(attend = NA)
    if (names(df) %>% str_detect("employ") %>% any()) {
        df %<>% mutate(emp = ifelse(employ<3, 1, ifelse(employ==3, 0, NA)))
    } else df %<>% mutate(emp = NA)

    df$survey <- surv
    
    vars2 <- c("fips", "state", "div_hhn", "have_not", "income", "educ", "age", "male", "white", 
               "union", "emp", "partyid", "ideo", "attend", "survey")
    df %<>% select(one_of(vars2)) %>% left_join(cnty_data) %>% filter(white==1)
} 

hhn_mi <- function(df, seed=324) {
    mdf <- missing_data.frame(as.data.frame(df))
    mdf <- change(mdf, y = c("fips", "state"), what = "type", to = "irrelevant")
    mdf <- change(mdf, y = c("income", "educ", "attend"), what = "type", to = "ordered-categorical")
    mdf <- change(mdf, y = "age", what = "type", to = "bounded-continuous", lower=18, upper=97)
    mdf_mi <- mi(mdf, seed=seed) 
    
    # switch to mitools format (no support for glmer in mi::pool)
    mdf_mi_list <- complete(mdf_mi, m=10) 
    mdf_mi_list <- lapply(mdf_mi_list, function(df) 
        sapply(df, function(v) 
            if(any(class(v)=="factor")) v <- as.numeric(levels(v))[v] else v <- v) %>% data.frame) # get rid of %&*(&^ factors
    imputationList(mdf_mi_list)
}

format_mi_results <- function(m) {
    m_fe <- MIextract(m, fun=fixef) # see https://books.google.com/books?id=EbLrQrBGid8C&pg=PA384
    m_vars <- MIextract(m, fun=vcov)
    m_vars2 <- list()
    m_vars2 <- lapply(m_vars, as.matrix)
    m_res <- MIcombine(m_fe, m_vars2)
    b <- m_res$coefficients
    se <- diag(m_res$variance^.5)
    data.frame(Value = b,
               Coefficient = names(b),
               HighInner = b, # no inner CI, but variable is still needed by coefplot
               LowInner = b,
               HighOuter = b+qnorm(.975)*se,
               LowOuter = b-qnorm(.975)*se,
               Model = deparse(substitute(m)), 
               stringsAsFactors = FALSE)
}

cnty_data <- read_csv("data/cnty_data.csv")

### 2006 (Table 2)
hhn06 <- read_dta("data/pew/haves_havenots/Sept06/Sept06NIIc.dta") # Converted first with StatTransfer as read_sav didn't work
hhn06x <- hhn_format(hhn06, cfips="fips", dh="q52", hn="q53")

hhn06x_mi <- hhn_mi(hhn06x) # multiply impute missing data

t2 <- with(hhn06x_mi, 
              glmer(formula = div_hhn~gini_cnty+
                        income_cnty+black_cnty+perc_bush04+pop_cnty+
                        income+educ+age+male+union+emp+partyid+ideo+attend+
                        (1|fips), family=binomial(link="logit")))
t2_res <- format_mi_results(t2)
summary(t2_res)


### additional data
# load all datasets
hhn05 <- read_sav("data/pew/haves_havenots/Oct05NII/Oct05NIIc.sav") # Oct05NII (all controls)
hhn06 <- read_dta("data/pew/haves_havenots/Sept06/Sept06NIIc.dta") # used in Table 2
hhn07 <- read_sav("data/pew/haves_havenots/July07/July07c.sav") # July07 (missing labor, employ)
hhn0801 <- read_dta("data/pew/haves_havenots/Jan08/Jan08c.dta") # Jan08 (all controls)
hhn0810 <- read_sav("data/pew/haves_havenots/EarlyOct08/EarlyOct08c.sav") # EarlyOct2008 (missing labor, attend)
hhn09 <- read_sav("data/pew/haves_havenots/Values09/Values09c.sav") %>% # Values09 (all controls, but only Survey B asked hhn)
    filter(values==2) # keep only Survey B, which asked hhn questions

# no fips in Apr10-political-future;
#               Sept 22-25 2011 omnibus public; or 
#               Dec11 Political (they're too late for the acs0509 contextual data anyway)

# format all datasets
hhn05x <- hhn_format(hhn05, cfips="qfips", dh="q52", hn="q53")
hhn06x <- hhn_format(hhn06, cfips="fips", dh="q52", hn="q53")
hhn07x <- hhn_format(hhn07, cfips="qfips", dh="q40", hn="q41")
hhn0801x <- hhn_format(hhn0801, cfips="fips", dh="q33", hn="q34")
hhn0810x <- hhn_format(hhn0810, cfips="zfips", dh="q56", hn="q57")
hhn09x <- hhn_format(hhn09, cfips="fips", dh="qb28", hn="qb29")

# combine data
hhn <- rbind(hhn05x, hhn06x, hhn07x, hhn0801x, hhn0810x, hhn09x)
rm(list = ls(pattern="hhn0.*")) # free memory

hhn_mi <- hhn_mi(hhn)

t2_all <- with(hhn_mi, 
              glmer(formula = div_hhn~gini_cnty+
                        income_cnty+black_cnty+perc_bush04+pop_cnty+
                        income+educ+age+male+union+emp+partyid+ideo+attend+
                        (1|fips), family=binomial(link="logit")))
t2_all_res <- format_mi_results(t2_all)
summary(t2_all_res)
# beep()

# Cross-classified model with level for survey makes no difference
# t2_all2 <- with(hhn_mi, 
#                glmer(formula = div_hhn~gini_cnty+
#                          income_cnty+black_cnty+perc_bush04+pop_cnty+
#                          income+educ+age+male+union+emp+partyid+ideo+attend+
#                          (1|fips) + (1|survey), family=binomial(link="logit")))
# t2_all2_res <- format_mi_results(t2_all2)
# summary(t2_all2_res)
# beep()

# No sign of interaction either: gini_cnty not significant at any income
# t2_all3 <- with(hhn_mi, 
#                 glmer(formula = div_hhn~gini_cnty+
#                           income_cnty+black_cnty+perc_bush04+pop_cnty+
#                           income+educ+age+male+union+emp+partyid+ideo+attend+gini_cnty:income +
#                           (1|fips) + (1|survey), family=binomial(link="logit")))
# t2_all3_res <- format_mi_results(t2_all3)
# summary(t2_all3_res)
# interplot(t2_all3, "gini_cnty", "income")
# beep()


# Do any other datasets yield 2006 result? no
yrs <- c(2005, 2006, 2007, 200801, 200810, 2009)
t2_by_surv <- list()

for (i in 1:length(yrs)) {
    ds <- lapply(hhn_mi$imputations, function(x) x[x$survey==yrs[i],, drop=F]) %>% imputationList()
    res <- with(ds, 
              glmer(formula = div_hhn~gini_cnty+
                        income_cnty+black_cnty+perc_bush04+pop_cnty+
                        income+educ+age+male+union+emp+partyid+ideo+attend+
                        (1|fips), family=binomial(link="logit")))
    t2_by_surv[[i]] <- format_mi_results(res)
}
lapply(t2_by_surv, print)
beep()

coef_plot <- function(df, coef, lower, upper, varnames, groupname = NULL, xlab, label.pos = c(0, 1, 2), fontsize = NULL, monochrome = NULL)
{
    # This function creates a dot-and-whisker plot for regression coefficients, first differences, etc.
    # It requires the following arguments:
    # coef: a (numeric) vector of coefficients/first differences/...
    ## If using groups of variables: enter NA for the empty coefficient designating a group
    # se: a vector of standard errors
    ## If using groups of variables: enter NA for the empty SE designating a group
    # varnames: a (character) vector of variable names
    ## If using groups of variables: enter the group name where you want it to appear
    # groupname: an optional (character) vector of variable groups with the same length as varnames
    # xlab: a character vector of length 1 with the label for the x-axis
    # label.pos: Position of the x-axis tick labels; 0 (default) for left, 0.5 for center, 1 for right
    # monochrome: 0 for colors, 1 for black dots and whiskers
    # Optional arguments:
    # fontsize: an optional numeric vector of varying font sizes (if using groups)
    
    var <- seq(length(varnames), 1, by = -1)
    group <- as.numeric(as.factor(groupname))
    results <- data.frame(var=var, varnames=varnames, coef=coef, lower = lower, upper = upper, group=group, groupname=groupname)
  
    ggplot(results, aes(x = coef, y = as.factor(var))) +
        geom_point(colour = "black") + 
        geom_segment(aes(x = lower, xend = upper, y = var, yend = var), colour = "black") +
        theme_bw() + xlab(paste(xlab)) + ylab("") + 
        scale_y_discrete(breaks=results$var, labels=results$varnames) + 
        geom_vline(xintercept = 0, colour = "blue", linetype = 2) + 
        theme(legend.position = "none", 
              axis.text.y = element_text(colour = "black", hjust = label.pos),
              axis.text.x = element_text(colour = "black"))

}