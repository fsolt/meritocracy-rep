library(rio)
library(stringr)
library(dplyr)
library(ggplot2)

data_path <- "data/pew/merit/"
ver_a_files <- list.files(data_path, recursive = TRUE) %>% 
	str_subset("version_a.*sav|dta")
ver_a <- data.frame(
	file_name = paste0(data_path, ver_a_files[c(1:2, 5:8, 12:14, 17)]),
	year = c(2011, 2012, 2007, 2005, 2011, 2006,
			 2010, 2000, 1999, 2004), stringsAsFactors = FALSE) %>% arrange(year)
rc2 <- "1 = 0; 2 = 1; else = NA"
rc4 <- "c(1,2) = 0; c(3,4) = 1; else = NA"

ver_a <- ver_a %>% mutate(
	var_name = c("q18j", "Q18K", "q11k", "q14k", "q8b", "q5c", "q30d", "q17k", 
				 "q44a", "q11"), # does q44a correspond to 1999 data?
	weight = c("weight", "WEIGHT", rep("weight", 8)),
	rc = c(rep(rc4, 4), rep(rc2, 3), rc4, rep(rc2, 2)),
	version = "A",
	value = NA)

ver_a1 <- ver_a %>% filter(year!=2011)
ver_a2 <- ver_a %>% filter(year==2011)

setup <- function(x) {
	df <- import(x$file_name)
	df$rej_merit <- car::recode(as.numeric(df[[x$var_name]]), x$rc)
	df$weight <- df[[x$weight]]
	df <- df %>% select(rej_merit, weight) %>% mutate(year = x$year)
	return(df)
}
	
for (i in 1:nrow(ver_a1)) {
	df <- setup(ver_a1[i, ])
	ver_a1$value[i] <- weighted.mean(df$rej_merit, df$weight, na.rm = TRUE) *100
}

df <- rbind(setup(ver_a2[1, ]), setup(ver_a2[2, ]))
ver_a2 <- data.frame(
	year = 2011, 
	version = "A", 
	value = weighted.mean(df$rej_merit, df$weight, na.rm = TRUE) *100,
	stringsAsFactors = FALSE)

ver_b_file <- import(paste0(data_path, "version_b/1987-2012 Values Merge/1987-2012 Values Merge public.sav")) 
ver_b_file$rej_merit <- with(ver_b_file, ifelse(q2f < 3 & q2e < 3, 1, 0))
ver_b_file$rej_merit[ver_b_file$q2f == 9 | ver_b_file$q2e == 9] <- NA
ver_b <- ver_b_file %>% group_by(year) %>% 
	summarize(version = "B",
			  value = weighted.mean(rej_merit, weight, na.rm = TRUE) * 100) %>% 
	filter(year>=1999)

ver_c_file <- ver_b_file 
ver_c_file$rej_merit <- with(ver_c_file, ifelse(q2f < 3, 1, 0))
ver_c_file$rej_merit[ver_c_file$q2f == 9] <- NA
ver_c <- ver_c_file %>% group_by(year) %>% 
	summarize(version = "C",
			  value = weighted.mean(rej_merit, weight, na.rm = TRUE) * 100) %>% 
	filter(year>=1999)

ver_all <- ver_a1 %>% select(year, version, value) %>% rbind(ver_a2, ver_b, ver_c)
ver_all_wide <- ver_all %>% tidyr::spread(key = version, value = value) # just for reference

njl <- data.frame(
	year = c(2005:2007, 2009), 
	version = c("A", "A", "B", "C"), 
	njl = c("A", "A", "B", "C"),
	stringsAsFactors = FALSE)

ver_all2 <- left_join(ver_all, njl)
ver_all2$njl[is.na(ver_all2$njl)] <- "D"

ggplot(ver_all2, aes(x = year, y = value, color = version)) +
 	scale_color_manual(values = c(A = "red", B = "darkgreen", C = "navy")) +
 	scale_fill_manual(values = c(A = "red", B = "darkgreen", C = "navy", D = "white")) +
	geom_line() +
	geom_point(aes(fill = njl), shape = 21, na.rm = TRUE, size = 3) +
	scale_x_continuous(breaks=1999:2012, labels=1999:2012) +
	theme_bw() +
	theme(axis.text.x  = element_text(angle=80, vjust=0.6),
		  legend.position = "none") +
	ylab("Percentage Rejecting Meritocracy") + xlab("") +
	geom_text(aes(label = "Measure 3", x = 2000, y = 31, color = "C")) +
	geom_text(aes(label = "Measure 1", x = 2003, y = 26, color = "A")) +
	geom_text(aes(label = "Measure 2", x = 2010, y = 19, color = "B"))
ggsave("./doc/figures/04_three_measures_dv.pdf")
