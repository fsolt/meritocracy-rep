library(readr)
library(haven)
library(dplyr)
library(broom)
library(stargazer)
library(stringr)

t1_bush <- read_tsv("data/study_26584/Meritocracy Replication Data - Table 1.tab") %>% 
	select(fips, perc_bush04) %>% unique()

t2_bush <- read_tsv("data/study_26584/2006 Pew News Interest Survey_Table 2 Data in Stata Format.tab") %>% 
	select(fips, pbush_01) %>% unique()

bush <- left_join(t1_bush, t2_bush) 

fips_cnty <- read_csv("https://raw.githubusercontent.com/raypereda/fips-county-codes/master/lib/national.txt", 
					  col_types="ccccc") 
names(fips_cnty) <- tolower(gsub(" ", "_", names(fips_cnty)))
fips_cnty$fips <- as.numeric(do.call(paste0, c(fips_cnty[, c(2,3)])))
fips_cnty$county <- gsub(" County| Parish", "", fips_cnty$county_name)

bush_al <- left_join(bush, fips_cnty) %>% 
	filter(state == "AL" & !is.na(pbush_01)) %>% 
	transmute("County" = county, 
		   "Table 1 Data" = round(perc_bush04, digits = 2), 
		   "Table 2 Data" = round(pbush_01, digits = 2)) %>% 
	head()
rownames(bush_al) <- NULL

stargazer(bush_al, summary = FALSE, rownames = FALSE,
		  title = "Mismatched Data on Bush Vote, from Replication Data",
		  label = "T:bush_data",
		  out = "doc/figures/05_wrangle_with_care_bush_al.tex")

# Alternate table methods
# Hmisc::latex(bush_al,
# 			 file="doc/figures/05_handle_data_with_care_bush_al.tex",
# 			 cgroup=c("", "Data Used", "Data Used"),
# 			 booktabs=TRUE, dcolumns=FALSE,
# 			 rowname = NULL
# )
# 
# library(pixiedust)
# custom_head <- rbind(c("", "Table 1", "Table 2 "), 
# 					 c("County", "Data", "Data")) %>%
# 	as.data.frame(stringsAsFactors = FALSE)
# dust(bush_al) %>% 
# 	redust(custom_head, part = "head") %>% 
# 	sprinkle_print_method("html")
