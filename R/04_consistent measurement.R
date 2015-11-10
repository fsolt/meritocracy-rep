ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2", "tidyr","dplyr", "dotwhisker")
ipak(packages)

# Loading data
data <- read.table("./data/study_26584/Meritocracy Replication Data - Table 1.tab", sep="\t", header=TRUE)

data$year <- ifelse(data$survid2006 == 1, 2006, ifelse(data$survid2007 == 1, 2007, ifelse(data$survid2009 == 1, 2009, 2005)))


# Descriptive alaysis
ggplot(data, aes(x = meritocracy, colour = year)) + geom_bar()
ggplot(data, aes(x = meritocracy)) + geom_density()

count <- group_by(data, year) %>% summarise(merit = sum(meritocracy), nomerit = length(meritocracy) - sum(meritocracy), total = length(meritocracy))
