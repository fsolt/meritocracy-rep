library(haven)
library(dplyr)
library(magrittr)
library(lme4)
library(mi)
library(mitools)

# Have vs. have-nots 
# questions asked, with fips, in *every year* from 2005-2009, though NJL uses only 2006

hhn_format <- function(df, cfips, dh, hn) {
    surv <- deparse(substitute(df))
    all_vars <- c("income", "educ", "age", "sex", "racethn", "race", "hisp", 
                  "labor", "ideo", "attend", "employ", "party", "partyln")
    vars <- c(cfips, dh, hn, all_vars[all_vars %in% names(df)])
    df %<>% select(one_of(vars))
    names(df)[1:3] <- c("cfips", "dh", "hn")
    df %<>% mutate(
        fips = cfips,
        state = floor(cfips/1000),
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
    df %<>% select(one_of(vars2))
} 


# 2006
hhn06 <- read_dta("data/pew/haves_havenots/Sept06/Sept06NIIc.dta") # Converted first with StatTransfer as read_sav didn't work
hhn06x <- hhn_format(hhn06, cfips="fips", dh="q52", hn="q53")

hhn06x2 <- left_join(hhn06x, cnty_data)
hhn06x2.w <- hhn06x2 %>% filter(white==1)

    # row-wise deletion of missing data
t2.rwd <- glmer(formula = div_hhn~gini_cnty+ 
                    income_cnty+black_cnty+perc_bush04+pop_cnty+
                    income+educ+age+male+union+emp+partyid+ideo+attend+
                    (1|fips),
                data=hhn06x2.w, family=binomial(link="logit"))
summary(t2.rwd)

    # Multiply impute missing values with mi
hhn06x2.w.info <- mi.info(hhn06x2.w)
hhn06x2.w.info <- update(hhn06x2.w.info, "include", list(resp=F, fips=F, state=F))
hhn06x2.w.info <- update(hhn06x2.w.info, "type", list(
    income = "ordered-categorical",
    educ = "ordered-categorical",
    attend  = "ordered-categorical"))
hhn06x2.w.pre <- mi.preprocess(hhn06x2.w, hhn06x2.w.info)

hhn06x2.w.mi <- mi(hhn06x2.w.pre, n.imp=10, n.iter=30, seed=324, max.minutes=60)

hhn06x2.w.mi.list <- mi.completed(hhn06x2.w.mi)
hhn06x2.w.mi.list2 <- imputationList(hhn06x2.w.mi.list)
t2.mi <- with(hhn06x2.w.mi.list2, 
              glmer(formula = div_hhn~gini_cnty+
                        income_cnty+black_cnty+perc_bush04+pop_cnty+
                        income+educ+age+male+union+emp+partyid+ideo+attend+
                        (1|fips), family=binomial(link="logit")))
t2.mi.fe <- MIextract(t2.mi, fun=fixef) # https://books.google.com/books?id=EbLrQrBGid8C&pg=PA384
t2.mi.vars <- MIextract(t2.mi, fun=vcov)
t2.mi.vars2 <- list()
t2.mi.vars2 <- lapply(t2.mi.vars, as.matrix)
t2.mi.res <- MIcombine(t2.mi.fe, t2.mi.vars2)
summary(t2.mi.res)




# additional data
# hhn05 available with fips (Oct05NII)
hhn05 <- read_sav("data/pew/haves_havenots/Oct05NII/Oct05NIIc.sav")

# hhn07 available with fips (July07; missing labor, employ)
hhn07 <- read_sav("data/pew/haves_havenots/July07/July07c.sav")

# hhn08 available with fips (Jan08 has all controls; EarlyOct2008 missing labor, attend)
hhn0801 <- read_dta("data/pew/haves_havenots/Jan08/Jan08c.dta")
hhn0810 <- read_sav("data/pew/haves_havenots/EarlyOct08/EarlyOct08c.sav")

# hhn09 available with fips (Values09 has all controls but hhn asked only of half--survey used in Table 1!)
hhn09 <- read_sav("data/pew/haves_havenots/Values09/Values09c.sav")

# no fips in Apr10-political-future;
#               Sept 22-25 2011 omnibus public; or 
#               Dec11 Political (they're too late for the acs0509 contextual data anyway)

# format all datasets
hhn05x <- hhn_format(hhn05, cfips="qfips", dh="q52", hn="q53")
hhn06x <- hhn_format(hhn06, cfips="fips", dh="q52", hn="q53")
hhn07x <- hhn_format(hhn07, cfips="qfips", dh="qb28", hn="qb29")
hhn0801x <- hhn_format(hhn0801, cfips="fips", dh="q33", hn="q34")
hhn0810x <- hhn_format(hhn0810, cfips="zfips", dh="q56", hn="q57")
hhn09x <- hhn_format(hhn09, cfips="fips", dh="qb28", hn="qb29")




# combined data
hhn0506 <- rbind(hhn05x, hhn06x)

hhn0506 <- left_join(hhn0506, cnty_data)
hhn0506.w <- hhn0506[hhn0506$white==1, ]

t2_0506.rwd <- glmer(formula = div_hhn~gini_cnty+
                         income_cnty+black_cnty+perc_bush04+pop_cnty+
                         income+educ+age+male+union+emp+partyid+ideo+attend+
                         (1|fips),
                     data=hhn0506.w, family=binomial(link="logit"))
summary(t2_0506.rwd)

# t2_0506.plot <- interplot(t2_0506.rwd, "gini_cnty", "income",
#                        xmin = min(t$income_i), xmax = max(t$income_i),
#                        steps = 9, labels = t$inc_labels,
#                        xlab = "Family Income",
#                        ylab = "County Income Inequality")
# t2_0506.plot <- t2_0506.plot + geom_hline(yintercept=0, colour="grey80", linetype="dashed")
#ggsave(file="t2_0506_plot.pdf", plot=t1m1.plot, width=8, height=5.25)







# combined data
hhn0506.w.info <- mi.info(hhn0506.w)
hhn0506.w.info <- update(hhn0506.w.info, "include", list(resp=F, fips=F, state=F))
hhn0506.w.info <- update(hhn0506.w.info, "type", list(
    income = "ordered-categorical",
    educ = "ordered-categorical",
    attend  = "ordered-categorical"))
hhn0506.w.pre <- mi.preprocess(hhn0506.w, hhn0506.w.info)

hhn0506.w.mi <- mi(hhn0506.w.pre, n.imp=10, n.iter=30, seed=324, max.minutes=60)

hhn0506.w.mi.list <- mi.completed(hhn0506.w.mi)
hhn0506.w.mi.list2 <- imputationList(hhn0506.w.mi.list)
t2.c.mi <- with(hhn0506.w.mi.list2, 
                glmer(formula = div_hhn~gini_cnty+
                          income_cnty+black_cnty+perc_bush04+pop_cnty+
                          income+educ+age+male+union+emp+partyid+ideo+attend+
                          (1|fips), family=binomial(link="logit")))
t2.c.mi.fe <- MIextract(t2.c.mi, fun=fixef) # https://books.google.com/books?id=EbLrQrBGid8C&pg=PA384
t2.c.mi.vars <- MIextract(t2.c.mi, fun=vcov)
t2.c.mi.vars2 <- list()
t2.c.mi.vars2 <- lapply(t2.c.mi.vars, as.matrix)
t2.c.mi.res <- MIcombine(t2.c.mi.fe, t2.c.mi.vars2)
summary(t2.c.mi.res)

b1m1.mi.plot <- interplot(b1m1.mi, "gini_cnty", "income", steps=9,
                          labels = t$inc_labels,
                          xlab = "Family Income",
                          ylab = "County Income Inequality") 
b1m1.mi.plot <- b1m1.mi.plot + 
    geom_hline(yintercept=0, colour="grey80", linetype="dashed")
ggsave(file="b1m1_mi_plot.pdf", plot=b1m1.mi.plot, width=8, height=5.25)

b1m1.mi.coef <- interplot(b1m1.mi, "gini_cnty", "income", steps=9, plot=F)