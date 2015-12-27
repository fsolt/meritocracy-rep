ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("foreign","stringr", "ggplot2", "tidyr","dplyr", "dotwhisker")
ipak(packages)



# Version a ####
# “Most people who want to get ahead can make it if they’re willing to work hard” or “Hard work and determination are no guarantee of success for most people.” From this item, we constructed a dichotomous variable, labeled Meritocracy, which was coded 1 for respondents reporting agreement with the latter statement and 0 for those agreeing with the former statement.

# Loading data

dt99 <- read.dta("./data/merit/version_a/1999 typo/typo99.dta", convert.factors = F) # Q.18(T).j
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
dt_va[["1999"]] <- data.frame(value = recode(dt99$q18j, "c(1,2) = 1; c(3,4) = 0; else = NA"), weight = dt99$weight) # 1999 survey may have the wrong coding, 3, 4 = strongly agree

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
dt_meana <- data.frame(year = 1999, mean = with(dt_va[["1999"]], wtd.mean(value, weight, normwt = T)), se = with(dt_va[["1999"]], wtd.var(value, weight, normwt = T)/nrow(dt_va[["1999"]]) %>% sqrt()))

dt_meana <- rbind(dt_meana, 
                 c(year = 2000, mean = with(dt_va[["2000"]], wtd.mean(value, weight, normwt = T)), se = with(dt_va[["2000"]], wtd.var(value, weight, normwt = T) / nrow(dt_va[["2000"]]) %>% sqrt())),
                 c(year = 2004, mean = with(dt_va[["2004"]], wtd.mean(value, weight, normwt = T)), se = with(dt_va[["2004"]], wtd.var(value, weight, normwt = T) / nrow(dt_va[["2004"]]) %>% sqrt())),
                 c(year = 2005, mean = with(dt_va[["2005"]], wtd.mean(value, weight, normwt = T)), se = with(dt_va[["2005"]], wtd.var(value, weight, normwt = T) / nrow(dt_va[["2005"]]) %>% sqrt())),
                 c(year = 2006, mean = with(dt_va[["2006"]], wtd.mean(value, weight, normwt = T)), se = with(dt_va[["2006"]], wtd.var(value, weight, normwt = T) / nrow(dt_va[["2006"]]) %>% sqrt())),
                 c(year = 2010, mean = with(dt_va[["2010"]], wtd.mean(value, weight, normwt = T)), se = with(dt_va[["2010"]], wtd.var(value, weight, normwt = T) / nrow(dt_va[["2010"]]) %>% sqrt())),
                 c(year = 2011, mean = with(dt_va[["2011"]], wtd.mean(value, weight, normwt = T)), se = with(dt_va[["2011"]], wtd.var(value, weight, normwt = T) / nrow(dt_va[["2011"]]) %>% sqrt())),
                 c(year = 2011.1, mean = with(dt_va[["2011_2"]], wtd.mean(value, weight, normwt = T)), se = with(dt_va[["2011"]], wtd.var(value, weight, normwt = T) / nrow(dt_va[["2011_2"]]) %>% sqrt())),
                 c(year = 2012, mean = with(dt_va[["2012"]], wtd.mean(value, weight, normwt = T)), se = with(dt_va[["2012"]], wtd.var(value, weight, normwt = T) / nrow(dt_va[["2012"]]) %>% sqrt())),
                 c(year = 2014, mean = with(dt_va[["2014"]], wtd.mean(value, weight, normwt = T)), se = with(dt_va[["2014"]], wtd.var(value, weight, normwt = T) / nrow(dt_va[["2014"]]) %>% sqrt())),
                 c(year = 2014.1, mean = with(dt_va[["2014_2"]], wtd.mean(value, weight, normwt = T)), se = with(dt_va[["2014"]], wtd.var(value, weight, normwt = T) / nrow(dt_va[["2014_2"]]) %>% sqrt()))
)

dt_meana$version <- "a"



# Version b ####
dt8712 <- read.spss("./data/merit/version_b/1987-2012 Values Merge/1987-2012 Values Merge public.sav", to.data.frame = T) # q2.f q2.e
# RESPONSE CATEGORIES:
# 1	Completely agree
# 2	Mostly agree
# 3	Mostly disagree
# 4	Completely disagree
# 9	Don't know/Refused

dt8712$vab <- ifelse(as.numeric(dt8712$q2f) < 3 & as.numeric(dt8712$q2e) < 3, 1, 0)
  
  
dt_meanb <- group_by(dt8712, year) %>% summarise(mean = wtd.mean(vab, weight, normwt = T), sd = sqrt(wtd.var(vab, weight, normwt = T)), version = "b") 
dt_meanb$mean[dt_meanb$mean == 0] <- NA
dt_meanb$sd[dt_meanb$sd == 0] <- NA
dt_meanb <- left_join(dt_meanb, count(dt8712, year)) %>% mutate(se = sd/sqrt(n), sd = NULL, n = NULL)



# Version c ####
dt_vac <- data.frame(value = as.numeric(dt8712$q2f) %>% recode("c(1, 2) = 1; c(3, 4, 5) = 0"), year = dt8712$year, weight = dt8712$weight)


dt_meanc <- group_by(dt_vac, year) %>% summarise(mean = wtd.mean(value, weight, normwt = T), sd = sqrt(wtd.var(value, weight, normwt = T)), version = "c")  
dt_meanc$mean[is.nan(dt_meanc$mean)] <- NA
dt_meanc$sd[is.nan(dt_meanc$sd)] <- NA
dt_meanc <- left_join(dt_meanc, count(dt8712, year)) %>% mutate(se = sd/sqrt(n), sd = NULL, n = NULL)



# Combine ####
diff <- setdiff(unique(dt_meanb$year), unique(dt_meana$year))
dt_meana <- rbind(dt_meana, data.frame(year = diff, mean = NA, se = NA, version = "a"))
dt_meana <- dt_meana[order(dt_meana$year),]

diff <- setdiff(unique(dt_meana$year), unique(dt_meanb$year))
dt_meanb <- rbind(dt_meanb, data.frame(year = diff, mean = NA, se = NA, version = "b"))
dt_meanb <- dt_meanb[order(dt_meanb$year),]

dt_meanc <- rbind(dt_meanc, data.frame(year = diff, mean = NA, se = NA, version = "c"))
dt_meanc <- dt_meanc[order(dt_meanc$year),]

dt_mean <- bind_rows(dt_meana, dt_meanb, dt_meanc) %>% filter(year != 1993)

njl <- data.frame(
  year = c(2005:2007, 2009),  
  njl = c("a", "a", "b", "c"),
  stringsAsFactors = FALSE)

dt_mean <- left_join(dt_mean, njl)
dt_mean$njl[is.na(dt_mean$njl)] <- "d"


# ggplot(ver_all2, aes(x = year, y = value, color = version)) +
#   scale_color_manual(values = c(A = "red", B = "darkgreen", C = "navy")) +
#   scale_fill_manual(values = c(A = "red", B = "darkgreen", C = "navy", D = "white")) +
#   geom_line() +
#   geom_point(aes(fill = njl), shape = 21, na.rm = TRUE, size = 3) +
#   scale_x_continuous(breaks=1999:2012, labels=1999:2012) +
#   theme_bw() +
#   theme(axis.text.x  = element_text(angle=80, vjust=0.6),
#         legend.position = "none") +
#   ylab("Percentage Rejecting Meritocracy") + xlab("") +
#   geom_text(aes(label = "Measure 3", x = 2000, y = 31, color = "C")) +
#   geom_text(aes(label = "Measure 1", x = 2003, y = 26, color = "A")) +
#   geom_text(aes(label = "Measure 2", x = 2010, y = 19, color = "B"))
# ggsave("./doc/figures/04_three_measures_dv.pdf")
# 
# 
# dt_meanShare <- filter(dt_mean, year >= 1999 & year <= 2012 & year %% 1 == 0)




# plot ####
ggplot(dt_meanShare, aes(x = year, y = mean, color = version)) +
  scale_color_manual(values = c(a = "red", b = "darkgreen", c = "navy")) +
  scale_fill_manual(values = c(a = "red", b = "darkgreen", c = "navy", d = "white")) +
  geom_line() +
  geom_point(aes(fill = njl), shape = 21, na.rm = TRUE, size = 3) +
  scale_x_continuous(breaks=1999:2012, labels=1999:2012) +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=80, vjust=0.6),
        legend.position = "none") +
  ylab("Percentage Rejecting Meritocracy") + xlab("") 
# + geom_text(aes(label = "Measure 3", x = 2000, y = 31, color = "c")) +
# geom_text(aes(label = "Measure 1", x = 2003, y = 26, color = "a")) +
# geom_text(aes(label = "Measure 2", x = 2010, y = 19, color = "b"))


library(dotwhisker)
dt_mean$year <- as.character(dt_mean$year)
names(dt_mean) <- c("term", "estimate", "std.error", "model", "njl")
dt_mean <- mutate(dt_mean, estimate = estimate * 100, std.error = std.error * 100)


dwplot(dt_mean, dodge_size = 0.3) +
  scale_color_manual(values = c(a = "red", b = "darkgreen", c = "navy")) +
  scale_fill_manual(values = c(a = "red", b = "darkgreen", c = "navy", d = "white")) +
  geom_point(aes(fill = njl), shape = 21, na.rm = TRUE, size = 3) +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=80, vjust=0.6), legend.position = "none") + 
  xlab("Percentage Rejecting Meritocracy") +　
  coord_flip() 
ggsave("./doc/figures/04_dotwhisker_draft.pdf")

