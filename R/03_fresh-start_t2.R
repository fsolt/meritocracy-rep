# Alternate item
# 2007 Values Survey (also asked 2009 (with fips), 2012 (no fips), many earlier years before 2005)
val2007 <- read_sav("Pew/Values07/Values07c.sav")

val2007$rej_merit <- with(val2007, as.numeric((q13e<=2) & (q13f<=2)))
val2007$white <- ifelse(val2007$race==1 & val2007$hisp!=1, 1, 0)
prop.table(table(val2007$rej_merit[val2007$white==1]))

val2012 <- read_sav("Pew/august_12_middle_class/Aug12 Middle Class_cleaned.sav")


# Have vs. have-nots 
# questions asked often; have to look for fips




# Haves and have-nots
hhn06 <- read_dta("Pew/Haves_HaveNots/Sept06/Sept06NIIc.dta") #Data converted with StatTransfer as read_sav didn't work
hhn06x <- data.frame(
    resp = hhn06$psraid,
    fips = as.numeric(hhn06$fips),
    state = as.numeric(hhn06$state),
    div_hhn = ifelse(hhn06$q52==1, 1, ifelse(hhn06$q52==2, 0, NA)),
    have_not = ifelse(hhn06$q53==2, 1, ifelse(hhn06$q53==1, 0, NA)),
    income = ifelse(hhn06$income<=9, hhn06$income, NA), # 1 to 9
    educ = ifelse(hhn06$educ<=7, hhn06$educ, NA), # 1 to 7
    age = ifelse(hhn06$age<99, hhn06$age, NA),
    male = ifelse(hhn06$sex==1, 1, 0),
    white = ifelse(hhn06$race==1 & hhn06$hisp!=1, 1, 0),
    union = ifelse(hhn06$labor<=3, 1, ifelse(hhn06$labor==4, 0, NA)),
    ideo = 6 - ifelse(hhn06$ideo<=5, hhn06$ideo, NA), # 1 to 5
    attend = 7 - ifelse(hhn06$attend<=6, hhn06$attend, NA), # 1 to 6
    employ = as.numeric(hhn06$employ),
    employ2 = NA
)
hhn06x$partyid <- mapvalues(as.numeric(hhn06$party), 
                            from = c(1:5, 9), 
                            to = c(5, 1, 3, 3, 3, NA))
hhn06x$partyid[hhn06$partyln==1] <- 4
hhn06x$partyid[hhn06$partyln==2] <- 2

hhn06x$unemp <- ifelse((hhn06$employ==3 | hhn06$employ==9), NA, 0) # No employ2 in survey
hhn06x$emp <- ifelse((hhn06$employ<=2), 1, ifelse(hhn06$employ==3, 0, NA)) # employed vs. not employed

hhn06x2 <- left_join(hhn06x, cnty_data)
hhn06x2.w <- hhn06x2 %>% filter(white==1)

t2.rwd <- glmer(formula = div_hhn~gini_cnty+
                    income_cnty+black_cnty+perc_bush04+pop_cnty+
                    income+educ+age+male+union+emp+partyid+ideo+attend+
                    (1|fips),
                data=hhn06x2.w, family=binomial(link="logit"))
summary(t2.rwd)

# additional data
hhn05 <- read_sav("data/pew/haves_havenots/Oct05NII/Oct05NIIc.sav")
hhn05x <- data.frame(
    resp = hhn05$resp,
    fips = as.numeric(hhn05$qfips),
    state = as.numeric(hhn05$qstcd),
    div_hhn = ifelse(hhn05$q52==1, 1, ifelse(hhn05$q52==2, 0, NA)),
    have_not = ifelse(hhn05$q53==2, 1, ifelse(hhn05$q53==1, 0, NA)),
    income = ifelse(hhn05$income<=9, hhn05$income, NA), # 1 to 9
    educ = ifelse(hhn05$educ<=7, hhn05$educ, NA), # 1 to 7
    age = ifelse(hhn05$age<99, hhn05$age, NA),
    male = ifelse(hhn05$sex==1, 1, 0),
    white = ifelse(hhn05$race==1 & hhn05$hisp!=1, 1, 0),
    union = ifelse(hhn05$labor<=3, 1, ifelse(hhn05$labor==4, 0, NA)),
    ideo = 6 - ifelse(hhn05$ideo<=5, hhn05$ideo, NA), # 1 to 5
    attend = 7 - ifelse(hhn05$attend<=6, hhn05$attend, NA), # 1 to 6
    employ = as.numeric(hhn05$employ),
    employ2 = NA
)
hhn05x$partyid <- mapvalues(hhn05$party, 
                            from = c(1:5, 9), 
                            to = c(5, 1, 3, 3, 3, NA))
hhn05x$partyid[hhn05$partyln==1] <- 4
hhn05x$partyid[hhn05$partyln==2] <- 2

hhn05x$unemp <- ifelse((hhn05$employ==3 | hhn05$employ==9), NA, 0) # No employ2 in survey
hhn05x$emp <- ifelse((hhn05$employ<=2), 1, ifelse(hhn05$employ==3, 0, NA)) # employed vs. not employed

hhn05x2 <- left_join(hhn05x, cnty_data) 
hhn05x2.w <- hhn05x2[hhn05x2$white==1, ]

t2_05.rwd <- glmer(formula = div_hhn~gini_cnty+
                       income_cnty+black_cnty+perc_bush04+pop_cnty+
                       income+educ+age+male+union+emp+partyid+ideo+attend+
                       (1|fips),
                   data=hhn05x2.w, family=binomial(link="logit"))
summary(t2_05.rwd)

# hhn07 available with fips (July07; may not have all controls)
p07 <- read_sav("~/Documents/Projects/Meritocracy_Rep0/Pew/Haves_HaveNots/July07/July07c.sav")
# hhn08 available with fips (Jan08 *and* EarlyOct08; may not have all controls)
p08jan <- read_dta("~/Documents/Projects/Meritocracy_Rep0/Pew/Haves_HaveNots/Jan08/Jan08c.dta")
p08oct <- read_sav("~/Documents/Projects/Meritocracy_Rep0/Pew/Haves_HaveNots/EarlyOct08/EarlyOct08c.sav")
# hhn09 available in Values09! which they used in Table 1!!

# no fips in Apr10-political-future; Sept 22-25 2011 omnibus public; or Dec11 Political (they're too late for the contextual data anyway)

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





# Multiply impute missing values with mi
# 2006 only
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