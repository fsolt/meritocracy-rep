ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("foreign", "readstata13","ggplot2", "tidyr","dplyr", "dotwhisker")
ipak(packages)


# Version a
# “Most people who want to get ahead can make it if they’re willing to work hard” or “Hard work and determination are no guarantee of success for most people.” From this item, we constructed a dichotomous variable, labeled Meritocracy, which was coded 1 for respondents reporting agreement with the latter statement and 0 for those agreeing with the former statement.

# Loading data

dt99 <- read.dta("./data/merit/version_a/1999 typo/typo99.dta", convert.factors = F) # Q.18(T).k
# CATEGORIES:
#   
# 1	Statement #1 — strongly
# 2	Statement #1 — not strongly
# 3	Statement #2 — strongly
# 4	Statement #2 — not strongly
# 5	Neither/both equally (VOLUNTEERED)
# 9	Don't know/Refused

dt00 <- read.spss("./data/merit/version_a/2000 typo/Typol00c.sav", to.data.frame = T) # Q.18.k
# CATEGORIES:
# 1	Statement #1 —  strongly
# 2	Statement #1 — not strongly
# 3	Statement #2 —  strongly
# 4	Statement #2 —  not strongly
# 5	Neither/both equally (VOLUNTEERED)
# 9	Don't know/Refused


dt04 <- read.dta("./data/merit/version_a/2004 Typology/typology04c.dta", convert.factors = F) # q11.k
# RESPONSE CATEGORIES:
# 1	Statement #1 — strongly
# 2	Statement #1 — not strongly
# 3	Statement #2 — strongly
# 4	Statement #2 — not strongly
# 5	Neither/both equally (VOLUNTEERED)
# 9	Don't know/Refused


dt05 <- read.spss("./data/merit/version_a/2005 Dec/Dec05c.sav", to.data.frame = T) # q14.k
# RESPONSE CATEGORIES:
# 1	Statement #1 — strongly
# 2	Statement #1 — not strongly
# 3	Statement #2 — strongly
# 4	Statement #2 — not strongly
# 5	Neither/both equally (VOLUNTEERED)
# 9	Don't know/Refused



dt06 <- read.spss("./data/merit/version_a/2006 Immigration/Mar06 Immigrationc.sav", to.data.frame = T) # q8.b

# RESPONSE CATEGORIES:
# 1	Statement #1 
# 2	Statement #2
# 3	Neither/both equally (VOLUNTEERED)
# 9	Don't know /Refused


dt10 <- read.spss("./data/merit/version_a/2010 Sept-Political-Independents/Sept10 public.sav", to.data.frame = T) # q30.d

# RESPONSE CATEGORIES:
# 1	Statement #1
# 2	Statement #2 
# 3	Neither/both equally (VOL.)
# 9	Don't know/Refused (VOL.)


dt11 <- read.spss("./data/merit/version_a/2011 Dec Political/Dec11 public.sav", to.data.frame = T) #Q44.a
# RESPONSE CATEGORIES:
# 1	Statement #1
# 2	Statement #2
# 3	[VOL. DO NOT READ] Neither/both equally
# 9	[VOL. DO NOT READ] Don't know/Refused


dt11_2 <- read.spss("./data/merit/version_a/2011 Political Typology Survey/2011 Political Typology public.sav", to.data.frame = T) #Q17.k
# RESPONSE CATEGORIES:
# 1	Statement #1 — strongly
# 2	Statement #1 — not strongly
# 3	Statement #2 — strongly
# 4	Statement #2 — not strongly
# 5	Neither/both equally (VOL.)
# 9	Don't know/Refused (VOL.)


dt12 <- read.spss("./data/merit/version_a/2012 august_middle_class/Aug12 Middle Class_cleaned.sav", to.data.frame = T) #Q11
# 1          Most people who want to get ahead can make it if they're willing to work hard [OR]
# 2          Hard work and determination are no guarantee of success for most people
# 3          [VOL. DO NOT READ]   Neither/Both equally 
# 9          [VOL. DO NOT READ]   Don’t know/Refused


dt14 <- read.spss("./data/merit/version_a/2014 Jan/Jan14 public.sav", to.data.frame = T) #Q44.a
# RESPONSE CATEGORIES:
# 1	Statement #1
# 2	Statement #2
# 3	[VOL. DO NOT READ] Neither/both equally
# 9	[VOL. DO NOT READ] Don't know/Refused


dt14_2 <- read.spss("./data/merit/version_a/2014 Polarization/Polarization 2014 public.sav", to.data.frame = T) # q25.k
# RESPONSE CATEGORIES:
# 1	Statement #1 
# 2	Statement #2 
# 5	Neither/Both equally (VOL.)
# 9	Don't know/Refused (VOL.)


# dt15 <- read.spss("./data/merit/version_a/2015 Jan/Jan15 public.sav", to.data.frame = T) no such question

# Data selection
library(car)

dt_va <- list()
dt_va[["1999"]] <- data.frame(value = recode(dt99$q18k, "c(1,2) = 0; c(3,4) = 1; else = NA"), weight = dt99$weight)

dt_va[["2000"]] <- data.frame(value = recode(dt00$Q18K, "c('Statement #1 -  strongly','Statement #1 - not strongly') = 0; c('Statement #2 -  strongly', 'Statement #2 -  not strongly') = 1; else = NA") %>% as.numeric() - 1, weight = dt00$WEIGHT)

dt_va[["2004"]] <- data.frame(value = recode(dt04$q11k, "c(1,2) = 0; c(3,4) = 1; else = NA"), weight = dt04$weight)

dt_va[["2005"]] <- data.frame(value = as.numeric(dt05$q14k) %>% recode("c(1,2) = 0; c(3,4) = 1; else = NA"), weight = dt05$weight)
  
dt_va[["2006"]] <- data.frame(value = as.numeric(dt06$q8b) %>% recode("1 = 0; 2 = 1; else = NA"), weight = dt06$weight)

dt_va[["2010"]] <- data.frame(value = as.numeric(dt10$q30d) %>% recode("1 = 0; 2 = 1; else = NA"), weight = dt10$weight)

dt_va[["2011"]] <- data.frame(value = as.numeric(dt11$q44a) %>% recode("1 = 0; 2 = 1; else = NA"), weight = dt11$weight)

dt_va[["2011_2"]] <- data.frame(value = as.numeric(dt11_2$q17k) %>% recode("c(1,2) = 0; c(3,4) = 1; else = NA"), weight = dt11_2$weight)

dt_va[["2012"]] <- data.frame(value = as.numeric(dt12$q11) %>% recode("1 = 0; 2 = 1; else = NA"), weight = dt12$weight)

dt_va[["2014"]] <- data.frame(value = as.numeric(dt14$q44a) %>% recode("1 = 0; 2 = 1; else = NA"), weight = dt14$weight)

dt_va[["2014_2"]] <- data.frame(value = as.numeric(dt14_2$q25k) %>% recode("1 = 0; 2 = 1; else = NA"), weight = dt14_2$weight)


# Mean data
library(Hmisc)
dt_mean <- data.frame(year = 1999, mean = with(dt_va[["1999"]], wtd.mean(value, weight, normwt = T)), sd = with(dt_va[["1999"]], wtd.var(value, weight, normwt = T)%>% sqrt()))

dt_mean <- rbind(dt_mean, 
                 c(year = 2000, mean = with(dt_va[["2000"]], wtd.mean(value, weight, normwt = T)), sd = with(dt_va[["2000"]], wtd.var(value, weight, normwt = T)%>% sqrt())),
                 c(year = 2004, mean = with(dt_va[["2004"]], wtd.mean(value, weight, normwt = T)), sd = with(dt_va[["2004"]], wtd.var(value, weight, normwt = T)%>% sqrt())),
                 c(year = 2005, mean = with(dt_va[["2005"]], wtd.mean(value, weight, normwt = T)), sd = with(dt_va[["2005"]], wtd.var(value, weight, normwt = T)%>% sqrt())),
                 c(year = 2006, mean = with(dt_va[["2006"]], wtd.mean(value, weight, normwt = T)), sd = with(dt_va[["2006"]], wtd.var(value, weight, normwt = T)%>% sqrt())),
                 c(year = 2010, mean = with(dt_va[["2010"]], wtd.mean(value, weight, normwt = T)), sd = with(dt_va[["2010"]], wtd.var(value, weight, normwt = T)%>% sqrt())),
                 c(year = 2011, mean = with(dt_va[["2011"]], wtd.mean(value, weight, normwt = T)), sd = with(dt_va[["2011"]], wtd.var(value, weight, normwt = T)%>% sqrt())),
                 c(year = 2012, mean = with(dt_va[["2012"]], wtd.mean(value, weight, normwt = T)), sd = with(dt_va[["2012"]], wtd.var(value, weight, normwt = T)%>% sqrt())),
                 c(year = 2014, mean = with(dt_va[["2014"]], wtd.mean(value, weight, normwt = T)), sd = with(dt_va[["2014"]], wtd.var(value, weight, normwt = T)%>% sqrt()))
                 )

  
  
