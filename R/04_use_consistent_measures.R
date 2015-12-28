#Package load ####

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("rio", "stringr", "dplyr", "Hmisc", "ggplot2")
ipak(packages)



# Data setting ####

data_path <- "data/pew/merit/"
ver_a_files <- list.files(data_path, recursive = TRUE) %>% 
	str_subset("version_a.*sav|dta")

# [1] "version_a/2011 Political Typology Survey/2011 Political Typology public.sav"                              
# [2] "version_a/august_12_middle_class/Aug12 Middle Class_cleaned.sav"                                          
# [3] "version_a/dataset_Religious_Landscape_Survey_Data/Religious Landscape Survey Data - Alaska and Hawaii.sav"
# [4] "version_a/dataset_Religious_Landscape_Survey_Data/Religious Landscape Survey Data - Continental US.sav"   
# [5] "version_a/Dec05/Dec05c.sav"                                                                               
# [6] "version_a/Dec11 Political/Dec11 public.sav"                                                               
# [7] "version_a/Immigration06/Mar06 Immigrationc.sav"                                                           
# [8] "version_a/Jan14/Jan14 public.sav"                                                                         
# [9] "version_a/Jan15/Jan15 public.sav"                                                                         
# [10] "version_a/Polarization 2014/Polarization 2014 public.sav"                                                 
# [11] "version_a/Sept-10-Political-Independents/Sept10 public.sav"                                               
# [12] "version_a/typo00/Typol00c.sav"                                                                            
# [13] "version_a/typo99/typo99.dta"                                                                              
# [14] "version_a/Typology04/TypoCallback05/typology callbackc.dta"                                               
# [15] "version_a/Typology04/TypoCallback05/Typology Callbackc.sav"                                               
# [16] "version_a/Typology04/typology04c.dta"                                                                     
# [17] "version_a/Typology04/Typology04c.sav"   

ver_a <- data.frame(
	file_name = paste0(data_path, ver_a_files[c(1:2, 4:7, 11:13, 16)]),
	year = c(2011, 2012, 2007, 2005, 2011, 2006, 2010, 2000, 1999, 2004), 
	stringsAsFactors = FALSE) %>% arrange(year)

rc2 <- "1 = 0; 2 = 1; else = NA" # coding rule 1
rc4 <- "c(1,2) = 0; c(3,4) = 1; else = NA" # coding rule 2

ver_a <- ver_a %>% mutate(
	var_name = c("q18j", "Q18K", "q11k", "q14k", "q8b", "q5c", "q30d", "q17k", 
				 "q44a", "q11"),  # question numbers 
	weight = c("weight", "WEIGHT", rep("weight", 8)),
	rc = c(rep(rc4, 4), rep(rc2, 3), rc4, rep(rc2, 2)),
	version = "A",
	value = NA, 
	se = NA)

ver_a1 <- ver_a %>% filter(year!=2011)
ver_a2 <- ver_a %>% filter(year==2011)

setup <- function(x) {
	df <- import(x$file_name)
	df$rej_merit <- car::recode(as.numeric(df[[x$var_name]]), x$rc) # using the coding rule to recode
	df$weight <- df[[x$weight]]
	df <- df %>% select(rej_merit, weight) %>% mutate(year = x$year)
	return(df)
} # function to read data and select variables


# Calculating means and variances ####
	
for (i in 1:nrow(ver_a1)) {
	df <- setup(ver_a1[i, ])
	df$n <- length(df$rej_merit)
	ver_a1$value[i] <- wtd.mean(df$rej_merit, df$weight, normwt = T) *100
	ver_a1$se[i] <- wtd.var(df$rej_merit, df$weight, normwt = T) / df$n %>% sqrt() * 100
}


df <- rbind(setup(ver_a2[1, ]), setup(ver_a2[2, ]))

ver_a2 <- summarise(df, year = 2011, 
                    version = "A", 
                    n = length(rej_merit),
                    value = wtd.mean(rej_merit, weight, normwt = T) *100,
                    se = wtd.var(rej_merit, weight, normwt = T) / n %>% sqrt() * 100)


ver_b_file <- import(paste0(data_path, "version_b/1987-2012 Values Merge/1987-2012 Values Merge public.sav")) 
ver_b_file$rej_merit <- with(ver_b_file, ifelse(q2f < 3 & q2e < 3, 1, 0))
ver_b_file$rej_merit[ver_b_file$q2f == 9 | ver_b_file$q2e == 9] <- NA
ver_b <- ver_b_file %>% group_by(year) %>% 
	summarise(version = "B",
			  value = wtd.mean(rej_merit, weight, normwt = T) * 100,
			  n = length(rej_merit),
			  se = wtd.var(rej_merit, weight, normwt = T) / n %>% sqrt() * 100) %>% 
	filter(year>=1999)


ver_c_file <- ver_b_file 
ver_c_file$rej_merit <- with(ver_c_file, ifelse(q2f < 3, 1, 0))
ver_c_file$rej_merit[ver_c_file$q2f == 9] <- NA
ver_c <- ver_c_file %>% group_by(year) %>% 
	summarise(version = "C",
	          value = wtd.mean(rej_merit, weight, normwt = T) * 100,
	          n = length(rej_merit),
	          se = wtd.var(rej_merit, weight, normwt = T) / n %>% sqrt() * 100) %>% 
	filter(year>=1999)

ver_all <- bind_rows(ver_a1, ver_a2, ver_b, ver_c)
ver_all <- ver_all[, colSums(is.na(ver_all)) == 0]
ver_all_wide <- ver_all %>% tidyr::spread(key = version, value = value) # just for reference



njl <- data.frame(
	year = c(2005:2007, 2009), 
	version = c("A", "A", "B", "C"), 
	njl = c("A", "A", "B", "C"),
	stringsAsFactors = FALSE) # data used in NJL paper

ver_all2 <- left_join(ver_all, njl)
ver_all2$njl[is.na(ver_all2$njl)] <- "D"

ggplot(ver_all2, aes(x = year, y = value, color = version)) +
 	scale_color_manual(values = c(A = "red", B = "darkgreen", C = "navy")) +
 	scale_fill_manual(values = c(A = "red", B = "darkgreen", C = "navy", D = "white")) +
  geom_errorbar(aes(ymin=value - 1.96 * se, ymax=value + 1.96 * se), width=.1, position = position_dodge(width = 0.08)) +
	geom_line(position = position_dodge(width = 0.08)) +
	geom_point(aes(fill = njl), shape = 21, na.rm = TRUE, size = 3, position = position_dodge(width = 0.08)) +
	scale_x_continuous(breaks=1999:2012, labels=1999:2012) +
	theme_bw() +
	theme(axis.text.x  = element_text(angle=80, vjust=0.6),
		  legend.position = "none") +
	ylab("Percentage Rejecting Meritocracy") + xlab("") +
	geom_text(aes(label = "Measure 3", x = 2000, y = 31, color = "C")) +
	geom_text(aes(label = "Measure 1", x = 2003, y = 26, color = "A")) +
	geom_text(aes(label = "Measure 2", x = 2010, y = 19, color = "B"))
ggsave("./doc/figures/04_three_measures_whisker.pdf")
