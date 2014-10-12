# R code for Politics and Rulemaking at the Copyright Office
# 
# (C) 2013 by Gabriel J. Michael
# This work is licensed under CC BY-NC-SA
# For the details of the license, see http://creativecommons.org/licenses/by-nc-sa/3.0/
#
# Requires packages: xtable, Hmisc, MIfuns, pscl, texreg, ggplot2 and any dependencies
# Developed on R version 2.15.2 running on x86_64-apple-darwin9.8.0/x86_64 (64-bit) 
#
# NB: this code will not run as is. You need to have the data files, specify
# the paths to them, and uncomment the read.csv lines.
###############################################################################

# you will need to load these libraries and dependencies
library(xtable)
library(Hmisc)
library(MIfuns)
library(pscl)
library(texreg)
library(ggplot2)

# Set your working directory here
setwd("~")

# read in file
dmca <- read.csv("dmca_rulemaking_comments_public.csv", header=TRUE)

# ensure stuff are factors
dmca$cycle <- as.factor(dmca$cycle)
dmca$represented <- as.factor(dmca$represented)
dmca$type <- relevel(dmca$type, "individual")

# MODEL 1 PRIMARY MODEL WITHOUT CYCLE VARIABLE
model1 <- glm(granted ~ type + represented + valid + pages,
		family=binomial(link="probit"), data=dmca)
summary(model1)

# MODEL 2 LIMITED to 2003-2012 TO TEST RENEW VARIABLE
# testing renewal only makes sense in cycles 2 through 5
dmca25 <- subset(dmca, dmca$cycle!=2000)
model2 <- glm(granted ~ type + represented + valid + pages + renew,
		family=binomial(link="probit"), data=dmca25)
summary(model2)

# MODEL 3 ALL VARS INCLUDING CYCLE (EXCEPT RENEW)
model3 <- glm(granted ~ type + represented + valid + pages + cycle,
		family=binomial(link="probit"), data=dmca)
summary(model3)

# MODEL 4 LIMITING TO 2000-2010 TO GENERATE 2012 PREDICTIONS
# used data only from cycles 1 through 4
# excludes comment validity because all 2012 comments are valid, so not useful for prediction
dmca14 <- subset(dmca, dmca$cycle!=2012)
model4 <- glm(granted ~ type + represented + pages,
		family=binomial(link="probit"), data=dmca14)
summary(model4)

# PRINT TABLES
# will generate the latex and html code that can be pasted into other files
# html code can be used as is, latex code will need a preamble, etc, and a latex compiler

tbl <- ftable2data.frame(ftable(addmargins(table(dmca$type, dmca$valid, dmca$cycle))))
tbl[tbl[,2] %in% "0  ",2] <- "Invalid"
tbl[tbl[,2] %in% "1  ",2] <- "Valid  "
tbl[tbl[,2] %in% "Sum",2] <- "Total  "
colnames(tbl)[9] <- "Total"
tbl[c(2, 8, 11, 14),1] <- c("Individual", "Business  ", "Education ", "Total     ")
tbl <- xtable(tbl, caption="Table 1: Commenter Types and Comment Validity in Five Rulemaking Cycles")
tbl
print(tbl, caption.placement="top", include.rownames=FALSE)
sink("table1.html")
print(tbl, caption.placement="top", type="html", include.rownames=FALSE)
sink()

tbl <- ftable2data.frame(ftable(addmargins(table(dmca$type, dmca$represented, dmca$cycle))))
tbl[tbl[,2] %in% "0  ",2] <- "Not Represented"
tbl[tbl[,2] %in% "1  ",2] <- "Represented    "
tbl[tbl[,2] %in% "Sum",2] <- "Total          "
colnames(tbl)[9] <- "Total"
tbl[c(2, 8, 11, 14),1] <- c("Individual", "Business  ", "Education ", "Total     ")
tbl <- xtable(tbl, caption="Table 2: Commenter Types and Legal Representation in Five Rulemaking Cycles")
tbl
print(tbl, caption.placement="top", include.rownames=FALSE)
sink("table2.html")
print(tbl, caption.placement="top", type="html", include.rownames=FALSE)
sink()

tbl <- ftable2data.frame(ftable(addmargins(table(dmca$type, dmca$granted, dmca$cycle))))
tbl[tbl[,2] %in% "0  ",2] <- "Not Granted"
tbl[tbl[,2] %in% "1  ",2] <- "Granted    "
tbl[tbl[,2] %in% "Sum",2] <- "Total      "
colnames(tbl)[9] <- "Total"
tbl[c(2, 8, 11, 14),1] <- c("Individual", "Business  ", "Education ", "Total     ")
tbl <- xtable(tbl, caption="Table 3: Commenter Types and Granted Exemptions in Five Rulemaking Cycles")
tbl
print(tbl, caption.placement="top", include.rownames=FALSE)
sink("table3.html")
print(tbl, caption.placement="top", type="html", include.rownames=FALSE)
sink()

# get some pseudo-R2 figures
# These have to be inserted manually into the texreg table

lapply(list(model1, model2, model3, model4), pR2)

# will generate the latex and html code that can be pasted into other files
# html code can be used as is, latex code will need a preamble, etc, and a latex compiler

texreg(list(model1, model2, model3, model4), caption="Table 4: Four Models of Commenter Influence", caption.above=TRUE,
		custom.coef.names=c("(Intercept)", "NGO", "Business", "Education", "Represented", "Valid", "Pages", "Renew", 
				"2003", "2006", "2010", "2012"),
		model.names=c("1 Primary", "2 Renewal", "3 Cycle", "4 2012 Predictions"))
sink("table4.html")
htmlreg(list(model1, model2, model3, model4), caption="Table 4: Four Models of Commenter Influence", caption.above=TRUE,
		custom.coef.names=c("(Intercept)", "NGO", "Business", "Education", "Represented", "Valid", "Pages", "Renew", 
				"2003", "2006", "2010", "2012"),
		model.names=c("Model 1:<br>Primary", "Model 2:<br>With Renewal Variable", "Model 3:<br>With Cycle Variable", "Model 4:<br>2012 Predictions"))
sink()

# POST ESTIMATION FOR MODEL 1

# you will have to change this line to reflect the path on your machine
# this file refers to a file of test data with 1600 rows corresponding to various combinations of the variables
test_data <- read.csv("test_data.csv", header=TRUE)
test_data$represented <- as.factor(test_data$represented)
#test_data$valid <- as.factor(test_data$valid)
predictions <- predict(model1, newdata=test_data, type="response", se.fit=TRUE)
test_data$p1 <- predictions$fit

# labelling function for facets
rep_labeller <- function(var, value){
	value <- as.character(value)
	if (var=="represented") { 
		value[value==0] <- "Not Represented"
		value[value==1]   <- "Represented"
	}
	if (var=="valid") { 
		value[value==0] <- "Invalid"
		value[value==1]   <- "Valid"
	} 
	return(value)
}
postscript("figure2.eps", horizontal=FALSE, paper="letter", width=7, height=7)
ggplot(test_data, aes(pages, p1, linetype=type)) + geom_line() + facet_grid(valid~represented, labeller=rep_labeller) +
		ggtitle("Figure 2: Model 1 Predicted Probabilities") +
		scale_linetype_discrete(name="Commenter Type") +
		xlab("Pages") + ylab("Predicted Probability")
dev.off()

# POST ESTIMATION PREDICTIONS (MODEL 4)
# you will have to change this line to reflect the path on your machine
# this file refers to a file of test data with 1600 rows corresponding to various combinations of the variables
test_data <- read.csv("test_data.csv", header=TRUE)
test_data$represented <- as.factor(test_data$represented)
test_data <- test_data[test_data$valid==1,]
#test_data$valid <- as.factor(test_data$valid)
predictions <- predict(model4, newdata=test_data, type="response", se.fit=TRUE)
test_data$p4 <- predictions$fit

# labelling function for facets
rep_labeller <- function(var, value){
	value <- as.character(value)
	if (var=="represented") { 
		value[value==0] <- "Not Represented"
		value[value==1]   <- "Represented"
	}
	return(value)
}
postscript("figure1.eps", horizontal=FALSE, paper="letter", width=7, height=3.5)
ggplot(test_data, aes(pages, p4, linetype=type)) + geom_line() + facet_grid(~represented, labeller=rep_labeller) +
		ggtitle("Figure 1: Model 4 Predicted Probabilities") +
		scale_linetype_discrete(name="Commenter Type") +
		xlab("Pages") + ylab("Predicted Probability")
dev.off()
