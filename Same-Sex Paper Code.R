rm(list = ls())

library(rms)
library(googleVis)
library(pROC)
library(gmodels)
library(scales)
library(Amelia)
library(ggplot2)
library(caret)
library(rattle)
library(plyr)
library(texreg)
library(MASS)
library(blme)
library(party)

setwd("C://Users//Sheryl//Documents//Housing Segregation")
CityData <- read.csv("Kentucky.csv")

CountyData <- read.csv("KentuckyCountiesSS.csv")

CityData <- join(CityData, CountyData, by = c("county_name", "as_of_year"))
                  

## Dichotomizing Action Taken and Loan Type
CityData$di_action <- ifelse(CityData$action_taken == 1, 1, 0) 
CityData$di_action_name <- ifelse(CityData$di_action == 1, "Originated", "Not Originated")

CityData$di_loan <- ifelse(CityData$loan_type == 1, 1, 0)
CityData$di_loan_name <- ifelse(CityData$loan_type == 1, "Conventional", "Government Backed")

# Renaming applicant_sex_name

levels(CityData$applicant_sex_name)[2] <- "Not Provided"

# Same Sex Applications

CityData$couple[CityData$applicant_sex == 1 & CityData$co_applicant_sex == 5] <- 1 # Single Male
CityData$couple[CityData$applicant_sex == 2 & CityData$co_applicant_sex == 5] <- 2 # Single Female
CityData$couple[CityData$applicant_sex == 1 & CityData$co_applicant_sex == 2] <- 3 # Male Lead
CityData$couple[CityData$applicant_sex == 2 & CityData$co_applicant_sex == 1] <- 4 # Female Lead
CityData$couple[CityData$applicant_sex == 1 & CityData$co_applicant_sex == 1] <- 5 # Male Couple
CityData$couple[CityData$applicant_sex == 2 & CityData$co_applicant_sex == 2] <- 6 # Female Couple
CityData$couple[CityData$applicant_sex == 3 | CityData$applicant_sex == 4] <- 7 # Other

CityData$couple_name[CityData$couple == 1] <- "Single Male"
CityData$couple_name[CityData$couple == 2] <- "Single Female"
CityData$couple_name[CityData$couple == 3] <- "Male Lead"
CityData$couple_name[CityData$couple == 4] <- "Female Lead"
CityData$couple_name[CityData$couple == 5] <- "Male Couple"
CityData$couple_name[CityData$couple == 6] <- "Female Couple"
CityData$couple_name[CityData$couple == 7] <- "Other"

# Four Couple Groups
CityData$four_couple[CityData$applicant_sex == 1 & CityData$co_applicant_sex == 5] <- 1 # Single 
CityData$four_couple[CityData$applicant_sex == 2 & CityData$co_applicant_sex == 5] <- 1 # Single 
CityData$four_couple[CityData$applicant_sex == 1 & CityData$co_applicant_sex == 2] <- 2 # Hetero Couple
CityData$four_couple[CityData$applicant_sex == 2 & CityData$co_applicant_sex == 1] <- 2 # Hetero Couple
CityData$four_couple[CityData$applicant_sex == 1 & CityData$co_applicant_sex == 1] <- 3 # Same Sex Couple
CityData$four_couple[CityData$applicant_sex == 2 & CityData$co_applicant_sex == 2] <- 3 # Same Sex Couple
CityData$four_couple[CityData$applicant_sex == 3 | CityData$applicant_sex == 4] <- 4 # Other

CityData$four_couple_name[CityData$four_couple == 1] <- "Single"
CityData$four_couple_name[CityData$four_couple == 1] <- "Single"
CityData$four_couple_name[CityData$four_couple == 2] <- "Hetero Couple"
CityData$four_couple_name[CityData$four_couple == 2] <- "Hetero Couple"
CityData$four_couple_name[CityData$four_couple == 3] <- "Same Sex Couple"
CityData$four_couple_name[CityData$four_couple == 3] <- "Same Sex Couple"
CityData$four_couple_name[CityData$four_couple == 4] <- "Other"

## Subsetting Data by Action
X1 <- CityData[CityData$action_taken == 1 | CityData$action_taken == 2 | 
                 CityData$action_taken == 3 | CityData$action_taken == 4 | 
                 CityData$action_taken == 5,]

# Omit NA couple_name
X1 <- X1[complete.cases(X1[,c("couple", "four_couple")]),]


# Fixing couple levels
X1$couple_name <- as.factor(X1$couple_name)
X1 <- within(X1, couple_name <- relevel(couple_name, ref = "Other"))

X1$four_couple_name <- as.factor(X1$four_couple_name)
X1 <- within(X1, four_couple_name <- relevel(four_couple_name, ref = "Other"))

# Dropping Variables
drop.vars <- names(X1) %in% c("agency_abbr", "agency_code", "agency_name", "applicant_race_3",
                              "applicant_race_4", "applicant_race_5", "applicant_race_name 3",
                              "applicant_race_name_4", "applicant_race_name_5", "co_applicant_race_3",
                              "co_applicant_race_4", "co_applicant_race_5", "co_applicant_race_name_3",
                              "co_applicant_race_name_4", "co_applicant_race_name_5","edit_status",
                              "edit_status_name", "hoepa_status", "hoepa_status_name",
                              "lien_status", "lien_status_name", "preapproval", 
                              "preapproval_name", "purchaser_type", "purchaser_type_name",
                              "rate_spread")
X1 <- X1[!drop.vars]

#Normalize Income and Loan Amount

X1$log_income <- log(X1$applicant_income_000s)
X1$log_amount <- log(X1$loan_amount_000s)

# Dummy variable for anti-discrimination law present in Washington D.C.

X1$fair_law <- 0
X1$fair_law[X1$state_abbr == "DC"] <- 1
X1$fair_law[X1$state_abbr == "MD"] <- 1
X1$fair_law[X1$county_name == "Alexandria city"] <- 1

# Dummy variable for anti-discrimination law present in Kentucky

X1$fair_law <- 0
X1$fair_law[X1$county_name == "Jefferson County"] <- 1
X1$fair_law[X1$county_name == "Fayette County"] <- 1
# Covington
X1$fair_law[X1$census_tract_number %in% c(603, 607, 609, 610, 611, 612, 613, 614, 616, 
                                          636.06, 638, 648, 649, 650, 651, 652, 653, 
                                          655.02, 668, 669, 670, 671)] <- 1 
# Danville
X1$fair_law[X1$census_tract_number %in% c(9301, 9302, 9303, 9304, 9305, 9306, 9307,
                                          9201.01, 9201.02, 9201.03, 9202, 9203, 9204)] <- 1
# Frankfort
X1$fair_law[X1$census_tract_number %in% c(701, 704.01, 704.02, 705, 706, 707.01, 707.02,
                                          708, 711, 712)] <- 1
# Morehead
X1$fair_law[X1$census_tract_number %in% c(9501, 9502, 9503)] <- 1
# Vicco
X1$fair_law[X1$census_tract_number %in% c(9605, 9707)] <- 1

# Descriptive Stats

X.ss <- X1[X1$four_couple_name == "Same Sex Couple",]
X.hetero <- X1[X1$four_couple_name == "Hetero Couple",]
X.single <- X1[X1$four_couple_name == "Single",]
X.other <- X1[X1$four_couple_name == "Other",]

summary(X.ss$log_income)
summary(X.hetero$log_income)
summary(X.single$log_income)
summary(X.other$log_income)

table(X.ss$as_of_year) # same sex applications mostly decreased during time frame
hist(X.ss$as_of_year)
hist(X1$as_of_year)

summary(X.ss$log_amount)
summary(X.hetero$log_amount)
summary(X.single$log_amount)
summary(X.other$log_amount)

table(X.ss$di_loan_name)
table(X.hetero$di_loan_name)
table(X.single$di_loan_name)
table(X.other$di_loan_name)

table(X.ss$di_action_name)
table(X.hetero$di_action_name)
table(X.single$di_action_name)
table(X.other$di_action_name)


# Plots


#Interactive Plots

int.plot <- gvisLineChart(X2, "tract_to_msamd_income", "minority_population",
                          options = list(width = 1000, height = 400, isStacked=TRUE,
                                         gvis.editor = "Edit me!"))
plot(int.plot)

## Couple Regression

couple.amt.dep <- lm(log_amount ~ log_income + minority_population +
                       tract_to_msamd_income + four_couple_name, data = X1)

# Randomly sample from data
r.X1<- X1[sample(nrow(X1), 75000, replace = F),]

# Main model
couple.orig.dep <- glm(di_action ~ log_income + log_amount + minority_population +
                         tract_to_msamd_income + four_couple_name + fair_law +
                         di_loan + as.factor(county_name) + as.factor(as_of_year), 
                       data = r.X1, family = "binomial")

htmlreg(couple.orig.dep, file = "MainModel.doc")

# More couple level model
more.couple.orig.dep <- glm(di_action ~ log_income + log_amount + minority_population +
                         tract_to_msamd_income + couple_name + fair_law +
                         di_loan + as.factor(county_name) + as.factor(as_of_year), 
                       data = r.X1, family = "binomial")

htmlreg(more.couple.orig.dep, file = "MoreCouple.doc")

# Hierarchical Model
#hier.orig.dep <- bglmer(di_action ~ log_income + log_amount + minority_population +
#                          tract_to_msamd_income + four_couple_name + fair_law +
#                          di_loan + (1 | county_name) + (1 | as_of_year), family = binomial,
#                        data = X1)
# This breaks R

# Predicted Probability

X.c.ss <- cbind(1, median(X1$log_income, na.rm = TRUE), median(X1$log_amount, na.rm = TRUE), 
                median(X1$minority_population, na.rm = TRUE), 
                median(X1$tract_to_msamd_income, na.rm = TRUE), 0, 1, 0, 1, 1)
                
X.c.single <- cbind(1, median(X1$log_income, na.rm = TRUE), median(X1$log_amount, na.rm = TRUE), 
                    median(X1$minority_population, na.rm = TRUE),
                    median(X1$tract_to_msamd_income, na.rm = TRUE), 0, 0, 1, 1, 1)

X.c.hetero <- cbind(1, median(X1$log_income, na.rm = TRUE), median(X1$log_amount, na.rm = TRUE),
                    median(X1$minority_population, na.rm = TRUE),
                    median(X1$tract_to_msamd_income, na.rm = TRUE), 1, 0, 0, 1, 1)

ss.prob <- plogis(X.c.ss%*%couple.orig.dep$coefficients[1:10])
single.prob <- plogis(X.c.single%*%couple.orig.dep$coefficients[1:10])
hetero.prob <- plogis(X.c.hetero%*%couple.orig.dep$coefficients[1:10])


beta.tilde <- mvrnorm(1000, couple.orig.dep$coefficients[c(1:28,30:35)], vcov(couple.orig.dep))
beta.tilde <- beta.tilde[,1:10]

ci.ss <- numeric(1000)
for(i in 1:1000) {
  ci.ss[i] <- plogis(X.c.ss%*%beta.tilde[i,])
}
quantile(ci.ss, c(.05, .95))

ci.single <- numeric(1000)
for(i in 1:1000) {
  ci.single[i] <- plogis(X.c.single%*%beta.tilde[i,])
}
quantile(ci.single, c(.05, .95))

ci.hetero <- numeric(1000)
for(i in 1:1000) {
  ci.hetero[i] <- plogis(X.c.hetero%*%beta.tilde[i,])
}
quantile(ci.hetero, c(.05, .95))


# Fairness Difference
X.c.nofair <- cbind(1, median(X1$log_income, na.rm = TRUE), median(X1$log_amount, na.rm = TRUE), 
                    median(X1$minority_population, na.rm = TRUE),
                    median(X1$tract_to_msamd_income, na.rm = TRUE), 0, 1, 0, 0, 1)

fair <- plogis(X.c.ss%*%couple.orig.dep$coefficients[1:10])
nofair <- plogis(X.c.nofair%*%couple.orig.dep$coefficients[1:10])
fd <- fair - nofair

ci.fair <- ci.nofair <- ci.fd.fair <- numeric(1000)
for(i in 1:1000) {
  ci.fair[i] <- plogis(X.c.ss%*%beta.tilde[i,])
  ci.nofair[i] <- plogis(X.c.nofair%*%beta.tilde[i,])
  ci.fd.fair[i] <- ci.fair[i]-ci.nofair[i]
}
quantile(ci.fd.fair, c(.05, .95))

# Other model

couple.type.dep <- glm(di_loan ~ log_income + minority_population +
                         tract_to_msamd_income + four_couple_name, 
                       data = X1, family = "binomial")

# Checking model fit
couple.predict <- ifelse(predict(couple.orig.dep, type = "response") >= .5, 1, 0)

xtabs(~couple.predict + X1$di_action) # not good at predicting rejections at all

theROC <- roc(couple.predict, X3$di_action)
plot(theROC)

XSS <- X2[X2$four_couple_name == "Same Sex Couple",]
XHC <- X2[X2$four_couple_name == "Hetero Couple",]

CrossTable(X2$four_couple_name, X2$di_action_name)
CrossTable(XSS$di_loan_name, XSS$di_action_name)
CrossTable(XHC$di_loan_name, XHC$di_action_name)


# Impute Missing Data
# Reduce Data set
reduced.X1 <- X1[,c(1, 19, 20, 23, 38, 39, 75, 78, 79, 80, 81, 82, 83, 84, 85, 86)]
a.out <- amelia(reduced.X1, m = 5, idvars = c("applicant_sex_name", "county_name",
                                                    "di_action_name", "di_loan_name",
                                                    "race"))
imp1 <- a.out$imputations[[1]]

# Matching
diff.1 <- mean(training$di_action[training$di_action == 1]) - mean(training$action_taken[training$di_action != 1])
se.1 <- wilcox.exact(training$di_action[training$di_action == 1], training$action_taken[training$di_action != 1], conf.int = TRUE)

m.out <- matchit(di_action ~ race + log_income + log_amount + tract_to_msamd_income + minority_population, method = "genetic", data = imp1)
m.data <- match.data(m.out)

diff.2 <- mean(m.data$di_action[m.data$di_action == 1]) - mean(m.data$di_action[m.data$di_action != 1])
se.2 <- wilcox.exact(m.data$di_action[m.data$di_action == 1], m.data$di_action[m.data$di_action != 1], conf.int = TRUE)

# Will have to multiply impute applicant incomes to get complete data first