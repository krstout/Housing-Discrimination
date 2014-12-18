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
library(survey)
library(maps)
library(ggmap)
library(pcse)

#setwd("C://Users//Sheryl//Documents//Housing Segregation")
setwd("C://Users//Kevin//Documents//Housing Segregation")

CityData <- read.csv("St.Louis.csv")

CountyData <- read.csv("STLSameSexPopulation.csv")

CityData <- join(CityData, CountyData, by = "county_name")


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
CityData$couple[CityData$applicant_sex == 3] <- 7 # Other
CityData$couple[CityData$applicant_sex == 4] <- 8 # Not Applicable

CityData$couple_name[CityData$couple == 1] <- "Single Male"
CityData$couple_name[CityData$couple == 2] <- "Single Female"
CityData$couple_name[CityData$couple == 3] <- "Male Lead"
CityData$couple_name[CityData$couple == 4] <- "Female Lead"
CityData$couple_name[CityData$couple == 5] <- "Male Couple"
CityData$couple_name[CityData$couple == 6] <- "Female Couple"
CityData$couple_name[CityData$couple == 7] <- "Other"
CityData$couple_name[CityData$couple == 8] <- "Not Applicable"

# Four Couple Groups
CityData$four_couple[CityData$applicant_sex == 1 & CityData$co_applicant_sex == 5] <- 1 # Single 
CityData$four_couple[CityData$applicant_sex == 2 & CityData$co_applicant_sex == 5] <- 1 # Single 
CityData$four_couple[CityData$applicant_sex == 1 & CityData$co_applicant_sex == 2] <- 2 # Hetero Couple
CityData$four_couple[CityData$applicant_sex == 2 & CityData$co_applicant_sex == 1] <- 2 # Hetero Couple
CityData$four_couple[CityData$applicant_sex == 1 & CityData$co_applicant_sex == 1] <- 3 # Same Sex Couple
CityData$four_couple[CityData$applicant_sex == 2 & CityData$co_applicant_sex == 2] <- 3 # Same Sex Couple
CityData$four_couple[CityData$applicant_sex == 3] <- 4 # Other
CityData$four_couple[CityData$applicant_sex == 4] <- 5 # Not Applicable

CityData$four_couple_name[CityData$four_couple == 1] <- "Single"
CityData$four_couple_name[CityData$four_couple == 1] <- "Single"
CityData$four_couple_name[CityData$four_couple == 2] <- "Hetero Couple"
CityData$four_couple_name[CityData$four_couple == 2] <- "Hetero Couple"
CityData$four_couple_name[CityData$four_couple == 3] <- "Same Sex Couple"
CityData$four_couple_name[CityData$four_couple == 3] <- "Same Sex Couple"
CityData$four_couple_name[CityData$four_couple == 4] <- "Other"
CityData$four_couple_name[CityData$four_couple == 5] <- "Not Applicable"

## Subsetting Data by Action
X1 <- CityData[CityData$action_taken == 1 | CityData$action_taken == 2 | 
                 CityData$action_taken == 3 | CityData$action_taken == 4 | 
                 CityData$action_taken == 5,]

## Throw out Not Applicable couples, which aren't actual people or are loan purchases by bank
X1 <- X1[X1$couple == 1 | X1$couple == 2 | X1$couple == 3 | X1$couple == 4 | X1$couple == 5 | 
           X1$couple == 6 | X1$couple == 7,]

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

# Dummy variable for anti-discrimination law present in St. Louis

X1$fair_law <- 0
X1$fair_law[X1$state_abbr == "IL"] <- 1
X1$fair_law[X1$county_name == "St. Louis city"] <- 1
X1$fair_law[X1$county_name == "St. Louis County"] <- 1

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


### REGRESSIONS


# Proportionally Sample

prop.sample <- function(stratum, n) {
  proportions <- table(stratum)/length(stratum)
  nstrat <- as.vector(round(n*proportions))
  nstrat[nstrat==0] <- 1
  names(nstrat) <- names(proportions)
  stratsample(stratum, nstrat)
}

weighted <- prop.sample(stratum = interaction(X1$four_couple, drop = TRUE), n = 50000)
X1.sample <- X1[weighted, ] # Produces sample which has same couple proportions as data

# Main model
couple.orig.dep <- glm(di_action ~ minority_population + tract_to_msamd_income + 
                         Percent.Same.Sex + fair_law + log_income + log_amount +
                         di_loan + four_couple_name,
                       data = X1, family = "binomial")

couple.int <- glm(di_action ~ minority_population + tract_to_msamd_income + 
                    Percent.Same.Sex + fair_law + log_income + log_amount +
                    di_loan + four_couple_name + four_couple_name*fair_law,
                  data = X1, family = "binomial")

more.couple.orig.dep <- glm(di_action ~ minority_population + tract_to_msamd_income + 
                              Percent.Same.Sex + fair_law + log_income + log_amount +
                              di_loan + couple_name,
                            data = X1, family = "binomial")

#pcse.couple <- pcse(couple.orig.dep, groupN = X1$county_name, groupT = X1$as_of_year)

htmlreg(l = list(couple.orig.dep, couple.int, more.couple.orig.dep), file = "MainModelSTL.doc", digits = 3)

# Get coefficient for 'Other' category
X1$four_couple_name <- as.factor(X1$four_couple_name)
X1 <- within(X1, four_couple_name <- relevel(four_couple_name, ref = "Hetero Couple"))

other.couple.int <- glm(di_action ~ minority_population + tract_to_msamd_income + 
                          Percent.Same.Sex + fair_law + log_income + log_amount +
                          di_loan + four_couple_name + four_couple_name*fair_law,
                        data = X1, family = "binomial")
summary(other.couple.int)

X1$four_couple_name <- as.factor(X1$four_couple_name)
X1 <- within(X1, four_couple_name <- relevel(four_couple_name, ref = "Other"))

# Hierarchical Model
#hier.orig.dep <- bglmer(di_action ~ log_income + log_amount + minority_population +
#                          tract_to_msamd_income + four_couple_name + di_loan + fair_law +
#                          Percent.Same.Sex + fair_law*four_couple_name +
#                          (1 | county_name) + (1 | as_of_year), 
#                        family = binomial, data = X1)

# Predicted Probability

X.c.ss <- cbind(1, median(X1$minority_population, na.rm = TRUE), 
                median(X1$tract_to_msamd_income, na.rm = TRUE), 
                median(X1$Percent.Same.Sex, na.rm = TRUE), 
                1, # Fair Law
                median(X1$log_income, na.rm = TRUE), 
                median(X1$log_amount, na.rm = TRUE), 
                1, # Coventional vs. Gov-backed
                0, 1, 0, 0, 1, 0) # Hetero, SS, Single

X.c.single <- cbind(1, median(X1$minority_population, na.rm = TRUE), 
                    median(X1$tract_to_msamd_income, na.rm = TRUE), 
                    median(X1$Percent.Same.Sex, na.rm = TRUE), 
                    1, # Fair Law
                    median(X1$log_income, na.rm = TRUE), 
                    median(X1$log_amount, na.rm = TRUE), 
                    1, # Coventional vs. Gov-backed
                    0, 0, 1, 0, 0, 1) # Hetero, SS, Single

X.c.hetero <- cbind(1, median(X1$minority_population, na.rm = TRUE), 
                    median(X1$tract_to_msamd_income, na.rm = TRUE), 
                    median(X1$Percent.Same.Sex, na.rm = TRUE), 
                    1, # Fair Law
                    median(X1$log_income, na.rm = TRUE), 
                    median(X1$log_amount, na.rm = TRUE), 
                    1, # Coventional vs. Gov-backed
                    1, 0, 0, 1, 0, 0) # Hetero, SS, Single

X.c.other <- cbind(1, median(X1$minority_population, na.rm = TRUE), 
                   median(X1$tract_to_msamd_income, na.rm = TRUE), 
                   median(X1$Percent.Same.Sex, na.rm = TRUE), 
                   1, # Fair Law
                   median(X1$log_income, na.rm = TRUE), 
                   median(X1$log_amount, na.rm = TRUE), 
                   1, # Coventional vs. Gov-backed
                   1, 0, 0, 1, 0, 0) # Other, SS, Single

ss.prob <- plogis(X.c.ss%*%couple.int$coefficients)
single.prob <- plogis(X.c.single%*%couple.int$coefficients)
hetero.prob <- plogis(X.c.hetero%*%couple.int$coefficients)
other.prob <- plogis(X.c.other%*%other.couple.int$coefficients)

# For simulating confidence intervals 
beta.tilde <- mvrnorm(1000, couple.int$coefficients, vcov(couple.int))
#beta.tilde <- beta.tilde[,1:10]

ci.ss <- numeric(1000)
for(i in 1:1000) {
  ci.ss[i] <- plogis(X.c.ss%*%beta.tilde[i,])
}
ss.int <- quantile(ci.ss, c(.05, .95))

ci.single <- numeric(1000)
for(i in 1:1000) {
  ci.single[i] <- plogis(X.c.single%*%beta.tilde[i,])
}
single.int <- quantile(ci.single, c(.05, .95))

ci.hetero <- numeric(1000)
for(i in 1:1000) {
  ci.hetero[i] <- plogis(X.c.hetero%*%beta.tilde[i,])
}
hetero.int <- quantile(ci.hetero, c(.05, .95))

# For simulating 'other' probability specifically
other.beta.tilde <- mvrnorm(1000, other.couple.int$coefficients, 
                            vcov(other.couple.int))
ci.other <- numeric(1000)
for(i in 1:1000) {
  ci.other[i] <- plogis(X.c.other%*%other.beta.tilde[i,])
}
other.int <- quantile(ci.other, c(.05, .95))

# First Difference for Opposite Sex vs. Same Sex

opp.ss.fd <- hetero.prob-ss.prob
opp.ss.ci <- opp.ci <- ss.ci <- numeric(1000)
for(i in 1:1000) {
  ss.ci[i] <- plogis(X.c.ss%*%beta.tilde[i,])
  opp.ci[i] <- plogis(X.c.hetero%*%beta.tilde[i,])
  opp.ss.ci[i] <- opp.ci[i]-ss.ci[i]
}
opp.ss.fd.ci <- quantile(opp.ss.ci, c(.05, .95))

# Fairness Difference

X.c.ss.nofair <- cbind(1, median(X1$minority_population, na.rm = TRUE), 
                       median(X1$tract_to_msamd_income, na.rm = TRUE), 
                       median(X1$Percent.Same.Sex, na.rm = TRUE), 
                       0, # Fair Law
                       median(X1$log_income, na.rm = TRUE), 
                       median(X1$log_amount, na.rm = TRUE), 
                       1, # Coventional vs. Gov-backed
                       0, 1, 0, 0, 0, 0) # Hetero, SS, Single

X.c.single.nofair <- cbind(1, median(X1$minority_population, na.rm = TRUE), 
                           median(X1$tract_to_msamd_income, na.rm = TRUE), 
                           median(X1$Percent.Same.Sex, na.rm = TRUE), 
                           0, # Fair Law
                           median(X1$log_income, na.rm = TRUE), 
                           median(X1$log_amount, na.rm = TRUE), 
                           1, # Coventional vs. Gov-backed
                           0, 0, 1, 0, 0, 0) # Hetero, SS, Single

X.c.hetero.nofair <- cbind(1, median(X1$minority_population, na.rm = TRUE), 
                           median(X1$tract_to_msamd_income, na.rm = TRUE), 
                           median(X1$Percent.Same.Sex, na.rm = TRUE), 
                           0, # Fair Law
                           median(X1$log_income, na.rm = TRUE), 
                           median(X1$log_amount, na.rm = TRUE), 
                           1, # Coventional vs. Gov-backed
                           1, 0, 0, 0, 0, 0) # Hetero, SS, Single

# Opposite vs. Same Sex in No Fair Law Area
nf.opp.ss.fd <- plogis(X.c.hetero.nofair%*%couple.int$coefficients)-
  plogis(X.c.ss.nofair%*%couple.int$coefficients)
nf.opp.ss.ci <- nf.opp.ci <- nf.ss.ci <- numeric(1000)

for(i in 1:1000) {
  nf.ss.ci[i] <- plogis(X.c.ss.nofair%*%beta.tilde[i,])
  nf.opp.ci[i] <- plogis(X.c.hetero.nofair%*%beta.tilde[i,])
  nf.opp.ss.ci[i] <- nf.opp.ci[i]-nf.ss.ci[i]
}
nf.opp.ss.fd.ci <- quantile(nf.opp.ss.ci, c(.05, .95))

# Second Difference
sd <- nf.opp.ss.fd - opp.ss.fd
sd.ci <- ss.fair <- opp.fair <- ss.nofair <- opp.nofair <- numeric(1000)
for(i in 1:1000) {
  opp.nofair <- plogis(X.c.hetero.nofair%*%beta.tilde[i,])
  ss.nofair <- plogis(X.c.ss.nofair%*%beta.tilde[i,])
  opp.fair <- plogis(X.c.hetero%*%beta.tilde[i,])
  ss.fair <- plogis(X.c.ss%*%beta.tilde[i,])
  sd.ci[i] <- (opp.nofair-ss.nofair) - (opp.fair-ss.fair)
}
second.diff.ci <- quantile(sd.ci, c(.05, .95))

# Same Sex Fair vs. No Fair Law
fair <- plogis(X.c.ss%*%couple.int$coefficients)
nofair <- plogis(X.c.ss.nofair%*%couple.int$coefficients)
fd <- fair - nofair

ci.fair <- ci.nofair <- ci.fd.fair <- numeric(1000)
for(i in 1:1000) {
  ci.fair[i] <- plogis(X.c.ss%*%beta.tilde[i,])
  ci.nofair[i] <- plogis(X.c.ss.nofair%*%beta.tilde[i,])
  ci.fd.fair[i] <- ci.fair[i]-ci.nofair[i]
}
fair.diff <- quantile(ci.fd.fair, c(.05, .95))

### End of Useful Code ###






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

### Maps ###

DCarea <- get_map("Washington D.C.", zoom = 10, maptype = "roadmap")
ggmap(DCarea)
