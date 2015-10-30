library(readr)
library(haven)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(lme4)
library(mi)
library(betareg)
library(truncnorm)
library(mitools)
library(beepr)
library(dotwhisker)

### Have vs. have-nots 
# questions asked, with fips, in *every year* from 2005-2009, though NJL uses only 2006

gen_partyid <- function(df) {
	pid <- rep(NA, length(df$party))
	pid[df$party==1 & df$partystr==1] <- 5
	pid[df$party==1 & df$partystr==2] <- 4
	pid[df$partyln==1] <- 4
	pid[(df$party==3 | df$party==4) & df$partyln==9] <- 3
	pid[df$partyln==2] <- 2
	pid[df$party==2 & df$partystr==2] <- 2
	pid[df$party==2 & df$partystr==1] <- 1
	return(pid)
}

gen_partyid2 <- function(df) {
	pid <- rep(NA, length(df$party))
	pid[df$party==1 & df$partystr==1] <- 7
	pid[df$party==1 & df$partystr==2] <- 6
	pid[df$partyln==1] <- 5
	pid[(df$party==3 | df$party==4) & df$partyln==9] <- 4
	pid[df$partyln==2] <- 3
	pid[df$party==2 & df$partystr==2] <- 2
	pid[df$party==2 & df$partystr==1] <- 1
	return(pid)
}

hhn_format <- function(df, cfips, dh, hn) {
	# clean a Pew have/have-not survey dataset and merge it with county-level data
	# df: data; cfips: var of county fips; dh: var of U.S. divided; hn: var of self-id as have-not
    surv <- str_replace(deparse(substitute(df)), "hhn", "20")
    all_vars <- c("income", "educ", "age", "sex", "racethn", "race", "hisp", 
                  "labor", "ideo", "attend", "employ",
    			  "party", "partystr", "partyln")
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
        ideo = as.integer(6 - ifelse(ideo<=5, ideo, NA)) # 1 to 5
    )
    df$partyid <- gen_partyid(df)
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
               "union", "emp", "partyid", "partyid2", "ideo", "attend", "survey")
    df %<>% select(one_of(vars2)) %>% left_join(cnty_data) %>% filter(white==1)
} 

hhn_mi <- function(df, seed=324) {
	# multiply impute missing data in a cleaned and merged Pew dataset
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
	# format results of analysis of multiply imputed Pew dataset
    m_fe <- MIextract(m, fun=fixef) # see https://books.google.com/books?id=EbLrQrBGid8C&pg=PA384
    m_vars <- MIextract(m, fun=vcov)
    m_vars2 <- list()
    m_vars2 <- lapply(m_vars, as.matrix)
    m_res <- MIcombine(m_fe, m_vars2)
    b <- m_res$coefficients
    se <- diag(m_res$variance^.5)
    df <- data.frame(term = names(b),
               estimate = b,
               std.error = se,
               model = deparse(substitute(m)), 
               stringsAsFactors = FALSE)
    df %>% filter(term!="(Intercept)")
}

## Replicate county-level data from sources
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
bush04$county[(bush04$state=="ME") & bush04$county=="som-s"] <- "somerset"
bush04$county[(bush04$state=="ME") & bush04$county=="oxf-s"] <- "oxford"
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
bush04_cnty <- bush04_cnty[!is.na(bush04_cnty$fips), ] # keep only results that already have fips
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

acs0509 <- read_csv("data/acs0509-counties.csv") # this throws warnings; they are irrelevant
names(acs0509) <- tolower(names(acs0509))
acs0509 <- mutate(acs0509,
				  fips = as.numeric(gsub("05000US", "", geoid)),
				  gini_cnty = b19083_001e,
				  income_cnty = b19013_001e/10000,
				  black_cnty = b02001_003e/b02001_001e,
				  pop_cnty = b02001_001e/10000)
cnty_data <- select(acs0509, fips:pop_cnty) %>% left_join(bush04_cnty)
write_csv(cnty_data, "data/cnty_data.csv")

cnty_data <- read_csv("data/cnty_data.csv")
vars_list <- c("gini_cnty", "income_cnty", "black_cnty", "perc_bush04", "pop_cnty", "income",
              "educ", "age", "male", "union", "emp", "partyid", "ideo", "attend", "(Intercept)")
vars_proper <- c("Gini Index", "Median Household Income", "Percent Black", "Bush Vote", 
                "Total Population", "Income", "Education", "Age", "Male", "Union Membership",
                "Employed", "Republican Party ID", "Conservative Ideology",
                "Religious Attendance")

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
dwplot(t2_res)


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
rm(list = ls(pattern="hhn0[5789].*")) # free up memory

hhn_mi <- hhn_mi(hhn)

t2_all <- with(hhn_mi, 
              glmer(formula = div_hhn~gini_cnty+
                        income_cnty+black_cnty+perc_bush04+pop_cnty+
                        income+educ+age+male+union+emp+partyid+ideo+attend+
                        (1|fips), family=binomial(link="logit")))
t2_all_res <- format_mi_results(t2_all)

level_brackets <- list(c("County-Level", "gini_cnty", "pop_cnty"),
                       c("Individual-Level", "income", "attend"))

p <- t2_res %>% by_2sd(hhn06x) %>%
    rbind(t2_all_res %>% by_2sd(hhn)) %>% dwplot +
    relabel_y_axis(vars_proper) +
    theme_bw() + xlab("Coefficient Estimate") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    theme(legend.justification=c(0, 1), legend.position=c(0, 1),
          legend.background = element_rect(colour="grey80"),
          legend.title.align = .5) +
    scale_colour_grey(start = .5, end = .7,
                      name = "Dataset",
                      breaks = c("t2", "t2_all"),
                      labels = c("Pew 2006", "Pew 2005-2009"))
g <- p %>% add_brackets(level_brackets)
grid.draw(g)
pdf("doc/figures/03_examine_all_available_data_t2.pdf")  
grid.draw(g)
dev.off()


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

# Does any other single dataset yield same result as 2006? no
yrs <- c(2005, 2007, 200801, 200810, 2009)

for (i in 1:length(yrs)) {
    ds <- lapply(hhn_mi$imputations, function(x) x[x$survey==yrs[i],, drop=F]) %>% imputationList()
    res <- with(ds, 
              glmer(formula = div_hhn~gini_cnty+
                        income_cnty+black_cnty+perc_bush04+pop_cnty+
                        income+educ+age+male+union+emp+partyid+ideo+attend+
                        (1|fips), family=binomial(link="logit")))
    tidy_res <- format_mi_results(res)
    tidy_res$model <- paste("Pew", yrs[i])
    if (i==1) t2_by_survey <- tidy_res else t2_by_survey <- rbind(t2_by_survey, tidy_res)
}

t2_by_survey %<>% rbind(t2_res %>% mutate(model = "Pew 2006")) %>% 
	arrange(model) # use 2006 data mi'd separately for consistency w first plot

secret_weapon(t2_by_survey, "gini_cnty") +
    theme_bw() + xlab("Coefficient Estimate, County Gini Index") + ylab("") + 
    relabel_y_axis(c("Oct 2005", "Sept 2006", "July 2007", 
                                            "Jan 2008", "Oct 2008", "Apr 2009")) +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + 
    theme(legend.position = "none")
ggsave("doc/figures/03_examine_all_available_data_t2_by_survey.pdf", width = 6, height = 3)
