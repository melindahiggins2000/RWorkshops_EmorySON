#' =======================================
#' Regression - fitting models
#' and exploring the results
#' 
#' Melinda Higgins, PhD
#' dated 08/17/2018
#' =======================================
#' 
#' PACKAGES NEEDED:
#' - tidyverse
#' - car
#' - olsrr
#' - ROCR
#' - Rcmdr
#' 
#' =======================================
#' Create a New Project in a 
#' New Folder on your computer
#' This will be your "working directory"
#' for this session/workshop
#' =======================================

# The *.Rdata file can be downloaded from the SASR website
# https://nhorton.people.amherst.edu/sasr2/datasets.php 
#
# download the HELP dataset and 
# put it in your working directory

# check your working directory using getwd()
getwd()

# load the dataset help.Rdata
# Rdata is proprietary to R
# but it efficient for memory and loads fast
load("help.Rdata")

# this loads the data frame "helpdata"
class(helpdata)
str(helpdata)
attributes(helpdata)
names(helpdata)

# This dataset has 453 observations and 88 vars
# various r functions for getting data.frame
# dimensions. 
dim(helpdata)
nrow(helpdata)
ncol(helpdata)

# load tidyverse package
# this loads dplyr and ggplot2
library(tidyverse)

# create a quick subset
# let's look at age, female, racegrp, cesd, pcs and mcs.
helpset1 <- helpdata %>%
  select(age, female, homeless, racegrp, cesd, pcs, mcs)

# get summary stats
summary(helpset1)

# last time we saw sort of model via the t.test
# compare to running a t-test
# NOTE: R by DEFAULT runs an unpooled t-test
options(digits=8)
t.test(age ~ female, helpset1)
t.test(cesd ~ female, helpset1)

# if you want to run a pooled t-test
t.test(formula = cesd ~ female, 
       data = helpset1,
       var.equal = TRUE)

# let's save the results and inspect the output object
t1 <- t.test(formula = cesd ~ female, 
             data = helpset1,
             var.equal = TRUE)

# you can perform a t-test for a dichotomous group variable
# like female by settin female as a predictor and setting
# cesd as the outcome in a regression model
# let's use the lm() linear model function and the same
# formula syntax cesd ~ female which you can read as
# "model the cesd as a function of the female variable predictor"
#: NOTICE that the options are very similar
lm(formula = cesd ~ female, 
   data = helpset1)

# let's save the results
lm1 <- lm(formula = cesd ~ female, 
          data = helpset1)

# summary of this lm1 object
class(lm1)
summary(lm1)

# coeficients
coef(lm1)

# ANOVA table of model fit
anova(lm1)

# built-in plot diagnostics for an lm class object
plot(lm1)

# get a plot window with 4 plots
# 2 columns 2 rows
par(mfrow=c(2,2))
plot(lm1)
# reset par
par(mfrow=c(1,1))

# get a histogram of the residuals
# use freq = FALSE to get probaiblities and not counts
# overlay a density curve
hist(lm1$residuals,
     freq = FALSE)
lines(density(lm1$residuals))

# look at a QQ plot of the residuals
library(car)
car::qqPlot(lm1$residuals)

# on your own
# look at cesd by homelessness (yes/no)
# use the homeless variable
t2 <- t.test(formula = cesd ~ homeless, 
             data = helpset1,
             var.equal = TRUE)
t2

# let's save the results
lm2 <- lm(formula = cesd ~ homeless, 
          data = helpset1)

summary(lm2)
car::qqPlot(lm2$residuals)

par(mfrow=c(2,2))
plot(lm2)
# reset par
par(mfrow=c(1,1))

# look at more than 1 predictor
# let's look at age, female, homeless
# and pcs as possible predictors for cesd
lm3 <- lm(formula = cesd ~ age + female + homeless + pcs, 
          data = helpset1)

# coefficients tests - based on TYPE III SS
summary(lm3)

# ANOVA of model effects show TYPE I SS
anova(lm3)

# NOTE: R by default displays the TYPE I Sums of Squares (SS)
# so the p-values shown DEPEND on the ORDER of the variables
# the p-value for age is for age as a predictor 
# with nothing else in the model
# the p-value for female is adjusted for age
# the p-value for homeless is adjusted for age and female
# the p-value for pcs is adjusted for age, female and homeless
# to get TYPE III SS which is what SAS and SPSS do be default
# we need the car package and the car::Anova() function
car::Anova(mod = lm3,
           type = "III")

# compare these to what we saw for the summary(lm3)
summary(lm3)

# let's investigate multicollinearity
car::vif(lm3)
sqrt(car::vif(lm3)) > 2 # problem?

# another helpful package for OLS
# model diagnostics - olsrr package
library(olsrr)

# get a more complete model fit summary
# includes standardized betas
olsrr::ols_regress(lm3)

# get DfBeta plots for each variable in model
olsrr::ols_plot_dfbetas(lm3)

# residual QQ plot
olsrr::ols_plot_resid_qq(lm3)

# get multicollinearity stats
olsrr::ols_coll_diag(lm3)

# normality tests for residuals
# take with a grain of salt
# these are sensitive to large n
olsrr::ols_test_normality(lm3)

# corelation between observed
# and expected residuals
olsrr::ols_test_correlation(lm3)

# observed versus predicted plots
olsrr::ols_plot_obs_fit(lm3)

# diagnostics plots
olsrr::ols_plot_diagnostics(lm3)

# stepwise variable selection
k <- olsrr::ols_step_both_p(lm3)
k
plot(k)

# backwards variable selection
k <- ols_step_backward_aic(lm3)
k
plot(k)

# forwards variable selection
k <- ols_step_forward_aic(lm3)
k
plot(k)

# both variable selection
k <- ols_step_both_aic(lm3)
k
plot(k)

# all possible subsets
k <- ols_step_all_possible(lm3)
k
plot(k)

# best subset
k <- ols_best_subset(lm3)
k
plot(k)

# let's see what R does with a categorical predictor
# like racegrp
# table of the racegrp
table(helpset1$racegrp)

# note: racegrp is a factor type variable
class(helpset1$racegrp)

# run a lm for cesd by racegrp
lm4 <- lm(cesd ~ racegrp, helpset1)

# the model summary gives coefficients
# for automatically created dummy variables
# each category is compared to the 1st category
# which was black
# the automatic recoding occurs since
# racegrp is a factor type variable
summary(lm4)

# the anova table gives the overall effect test
# of race as a predictor
anova(lm4)

# let's try a logistic regression
# compute cesd => 16 to indicate
# severe depressive symptoms
# create a new variable and add it to dataset
helpset1$cesdgte16 <- helpset1$cesd >= 16

glm1 <- glm(formula = cesdgte16 ~ age + female + homeless + pcs, 
            data = helpset1,
            family = binomial)
glm1
summary(glm1)

# NOTE: These coefficients are betas - you have
# to take the exp() to get the odds ratios
exp(coef(glm1))

# other model summary stats
anova(glm1)
anova(glm1, test = "Chisq")
car::Anova(glm1,
           type = 3)

# see https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

# make an ROC curve

library(ROCR)
p <- predict(glm1, newdata = helpset1, 
             type="response")
pr <- prediction(p, as.numeric(helpset1$cesdgte16))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(0,1, col="red")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# let's explore Rcmdr
# be sure you have Rcmdr package installed
# run library() to load the package and start
# the GUI interface
library(Rcmdr)
