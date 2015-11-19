library(stringr)
library(rio)

data_path <- "data/pew/merit/"
ver_a_files <- list.files(data_path, recursive = TRUE) %>% 
	str_subset("version_a.*sav|dta")	#misses typo1999c which is .dat in fwf
ver_a <- data.frame(
	file_name = paste0(data_path, ver_a_files[c(1:2, 5:9, 11:13, 16)]),
	year = c(2011, 2012, 2007, 2005, 2011.1, 2006, 2014, 2014.1,
			 2010, 2000, 2004), stringsAsFactors = FALSE) %>% arrange(year)
rc2 <- "1 = 0; 2 = 1; else = NA"
rc4 <- "c(1,2) = 0; c(3,4) = 1; else = NA"

ver_a <- ver_a %>% mutate(
	var_name = c("Q18K", "q11k", "q14k", "q8b", "q5c", "q30d", "q44a",
					"q17k", "q11", "q44a", "q25k"),
	weight = c("WEIGHT", rep("weight", 10)),
	rc = c(rep(rc4, 3), rep(rc2, 4), rc4, rep(rc2, 3)),
	version = "Version A",
	value = NA)

get_mean <- function(x) {
	df <- import(x$file_name)
	df$rej_merit <- car::recode(as.numeric(df[[x$var_name]]), x$rc)
	weighted.mean(df$rej_merit, df[[x$weight]], na.rm = TRUE)
}
	
for (i in 1:nrow(ver_a)) {
	ver_a$value[i] <- get_mean(ver_a[i, ])
# 	x <- ver_a[i, ]
# 	df <- import(x$file_name)
# 	df$rej_merit <- car::recode(as.numeric(df[[x$var_name]]), x$rc)
# 	ver_a$value[i] <- Hmisc::wtd.mean(df$rej_merit, df[[x$weight]]) * 100 %>% round()
#	ver_a$se[i] <- (Hmisc::wtd.var(df$rej_merit, df[[x$weight]], normwt = TRUE) %>% sqrt()) / sqrt(length(length(df$rej_merit[!is.na(df$rej_merit)])))
}

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


version_a <- list()

for (i in 1:length)