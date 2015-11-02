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
	transmute("County" = paste0(county, ", AL"),
		   "Table 1 Data" = round(perc_bush04, digits = 2), 
		   "Table 2 Data" = round(pbush_01, digits = 2)) %>% 
	head()

stargazer(bush_al, summary = FALSE, rownames = FALSE,
		  title = "Mismatched Data on Bush Vote, from Replication Data",
		  label = "T:bush_al",
		  notes="\\parbox[t]{10cm}{\\emph{Notes}: \\citet{Newman2015a} replication data on
		  the share of the vote won by Bush in the 2004 presidential election.  
		  The first six counties, when listed alphabetically by state and 
		  county, are shown; they reveal that the data employed in Table 2
		  only occasionally matches that employed in Table 1, even when
		  rounded to two digits.  Overall, these data match for fewer than
		  10\\% of all counties.}",
		  out = "doc/figures/05_wrangle_with_care_bush_al.tex")

# Alternate table methods
# Hmisc::latex(bush_al,
# 			 file="doc/figures/05_wrangle_with_care_bush_al.tex",
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
