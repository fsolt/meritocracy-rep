### Replicate Table 1 of Newman, Johnston, and Lown (2015)
### using their own replication data, and plot coefficient
### estimates for ginicnty by income_i

library(readr)
library(lme4)

interplot0 <- function(m, var1, var2, xlab=NULL, ylab=NULL, 
					   seed=324, sims=1000, steps=100, xmin=NA,
					   xmax=NA, labels=NULL, plot=TRUE) {
	require(arm)
	require(ggplot2)
	require(abind)
	set.seed(seed)
	if (class(m)=="list") {
		m.list <- m
		m <- m.list[[1]]
		m.class <- class(m)
		m.sims.list <- lapply(m.list, function(i) arm::sim(i, sims))
		m.sims <- m.sims.list[[1]]
		if (m.class=="lmerMod" | m.class=="glmerMod") {
			for(i in 2:length(m.sims.list)) {
				m.sims@fixef <- rbind(m.sims@fixef, m.sims.list[[i]]@fixef)
				m.sims@ranef[[1]] <- abind(m.sims@ranef[[1]], m.sims.list[[i]]@ranef[[1]], along=1)
			}
		} else {
			stop(paste("Multiply imputed flat models not implemented yet"))
		}
	} else {
		m.class <- class(m)
		m.sims <- arm::sim(m, sims)
	}
	if(var1==var2) var12 <- paste0("I(", var1, "^2)") else var12 <- paste0(var2,":",var1)
	if(m.class!="lmerMod" & m.class!="glmerMod"){
		if (!var12 %in% names(m$coef)) var12 <- paste0(var1,":",var2)
		if (!var12 %in% names(m$coef)) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
		if (is.na(xmin)) xmin <- min(m$model[var2], na.rm=T)
		if (is.na(xmax)) xmax <- max(m$model[var2], na.rm=T)
		coef <- data.frame(fake = seq(xmin, xmax, length.out=steps), coef1 = NA, ub = NA, lb = NA)
		
		for(i in 1:steps) {    
			coef$coef1[i] <- mean(m.sims@coef[,match(var1, names(m$coef))] + 
								  	coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))])
			coef$ub[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
								   	coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .975)
			coef$lb[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
								   	coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .025)    
		}
	} else {
		if (!var12 %in% unlist(dimnames(m@pp$X)[2])) var12 <- paste0(var1,":",var2)
		if (!var12 %in% unlist(dimnames(m@pp$X)[2])) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
		if (is.na(xmin)) xmin <- min(m@frame[var2], na.rm=T)
		if (is.na(xmax)) xmax <- max(m@frame[var2], na.rm=T)        
		coef <- data.frame(fake = seq(xmin, xmax, length.out=steps), coef1 = NA, ub = NA, lb = NA)
		
		for(i in 1:steps) {   
			coef$coef1[i] <- mean(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
								  	coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))])
			coef$ub[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
								   	coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .975)
			coef$lb[i] <- quantile(m.sims@fixef[,match(var1, unlist(dimnames(m@pp$X)[2]))] + 
								   	coef$fake[i]*m.sims@fixef[,match(var12, unlist(dimnames(m@pp$X)[2]))], .025)    
		}   
	}
	if (plot==TRUE) {
		if(steps>10) {
			coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) + 
				geom_line() + geom_ribbon(aes(ymin=lb, ymax=ub), alpha=.5) +
				theme_bw() + ylab(ylab) + xlab(xlab)
		} else {
			if (is.null(labels)) {
				coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) + 
					geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) +
					scale_x_continuous(breaks = seq(min(coef$fake), max(coef$fake), length.out=steps)) +
					theme_bw() + ylab(ylab) + xlab(xlab)
			} else {
				coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) + 
					geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) +
					scale_x_continuous(breaks = seq(min(coef$fake), max(coef$fake), length.out=steps),
									   labels = labels) +
					theme_bw() + ylab(ylab) + xlab(xlab)
			} 
		}
		# print(coef)
		return(coef.plot)
	} else {
		names(coef) <- c(var2, "coef", "ub", "lb")
		return(coef)
	}
}

### Read data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/26584
pew1 <- read_tsv("data/study_26584/Meritocracy Replication Data - Table 1.tab") # Combined Pew surveys
pew1$year <- 2005
pew1$year[pew1$survid2006==1] <- 2006
pew1$year[pew1$survid2007==1] <- 2007
pew1$year[pew1$survid2009==1] <- 2009
pew1_w <- pew1[pew1$white==1,]  # Only white respondents

### Analyses
# Table 1, Model 1
t1m1 <- glmer(formula=meritocracy~ginicnty+income_i+ginicnty:income_i+income_cnty+black_cnty+
			  	perc_bush04+pop_cnty+educ_i+age_i+gender_i+unemp_i+union_i+partyid_i+
			  	ideo_i+attend_i+survid2006+survid2007+survid2009+(1+income_i|fips),
			  data=pew1_w,family=binomial(link="logit"))
summary(t1m1)

# plot interaction
t <- data.frame(table(pew1_w$income_i))
names(t) <- c("income_i", "freq")
t <- t[t$freq>10, ] # include only observed values in plot (exclude imputed values)
t$income_i <- as.numeric(levels(t$income_i))[t$income_i]
t$inc_labels <- c("<$10k", "$10-20k", "$20-30k", "$30-40k", "$40-50k",
				  "$50-75k", "$75-100k", "$100-150k", ">$150k")

t1m1_plot <- interplot0(t1m1, "ginicnty", "income_i", sims=5000,
						xmin = min(t$income_i), xmax = max(t$income_i),
						steps = 9, labels = t$inc_labels,
						xlab = "Family Income",
						ylab = "County Income Inequality")
t1m1_plot <- t1m1_plot + geom_hline(yintercept=0, colour="grey80", linetype="dashed")
ggsave(file="doc/figures/07_plot_interaction_terms_t1m1.pdf", plot=t1m1_plot, width=8, height=5.25)


