#' =======================================
#' Univariate and Bivariate Stats and
#' Graphics for Visualizing data
#' 
#' Melinda Higgins, PhD
#' dated 07/23/2018
#' =======================================
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

# Note: length() only does the number of columns
# for a data.frame. However, you can use length()
# to get the number of elements in a given column.
# say the pcs1 variable.

# get the number of variables, number of columns
length(helpdata)

# get the number of rows, observations
length(helpdata$pcs1)

# simple summary stats
summary(helpdata$pcs1)

# notice that this doesn't give us the
# standard deviation
# other stats specific functions
mean(helpdata$pcs1)
sd(helpdata$pcs1)

# why did we get NA's - look back at the summary stats
# output - notice there were 207 NA's or missing values
# we have to add an option to handle the missing data
mean(helpdata$pcs1, na.rm = TRUE)
sd(helpdata$pcs1, na.rm = TRUE)

# to see amount of missing data in
# a given variable - try pcs1
summary(helpdata$pcs1)

# is.na() returns a logic statement
# of TRUE/FALSE which can be summed 
# to count the number of TRUE's as 1's
sum(is.na(helpdata$pcs1))

# can also use mean() to find the percentage
# of missing TRUE's - there are 207 missing
# out of 453 = 0.45695 (45.7% missing)
mean(is.na(helpdata$pcs1))

# suppose we wanted the 2.5th and 97.5th
# percentiles to find the middle 95% of the data
quantile(helpdata$pcs1, probs = c(0.025, 0.975),
         na.rm = TRUE)

# more ways to summarize data

# using "tidyverse" approach with pipes %>%
# load the dplyr package
library(dplyr)

# run summary stats for pcs1
helpdata %>%
  select(pcs1) %>%
  summary()

# run a basic summary of
# age, pcs, mcs and cesd
helpdata %>%
  select(age,pcs,mcs,cesd) %>%
  summary()

# try the Hmisc package - use describe() function
library(Hmisc)

# get descriptive stats for 1 variable
# at a time
Hmisc::describe(helpdata$age)
Hmisc::describe(helpdata$racegrp)
Hmisc::describe(helpdata$pcs)
Hmisc::describe(helpdata$pcs1)

# get descriptive stats for a selection of vars
helpdata %>%
  select(age,pcs,mcs,cesd) %>%
  Hmisc::describe()

# try pastecs package - stat.desc() function
library(pastecs)

helpdata %>%
  select(pcs1) %>%
  pastecs::stat.desc()

helpdata %>%
  select(age,pcs,mcs,cesd) %>%
  pastecs::stat.desc()

# also try the psych package
# describe() function
# ** IMPORTANT ** same function name
# as Hmisc - so you need to use
# package::function() format to keep
# things straight due to function masking

library(psych)

helpdata %>%
  select(age,pcs,mcs,cesd) %>%
  psych::describe()

# ways to get frequency tables
library(gmodels)
gmodels::CrossTable(helpdata$racegrp) # SAS format default
gmodels::CrossTable(helpdata$racegrp, format="SPSS")

# univariate graphics
# histograms

# r base functions
hist(helpdata$age)

# make a kernel density plot
plot(density(helpdata$age))

# put histogram and density plots together
# set freq=FALSE to get probabilities instead
# of counts/frequencies - that way the Y-axis
# is on the same scale for the density plot overlay
hist(helpdata$age,
     freq=FALSE,
     main="Histogram and Density of Ages in HELP dataset",
     xlab="Age")
lines(density(helpdata$age), col="blue", lwd=2)

# optional - add rug plot under x-axis
rug(jitter(helpdata$age))

# ggplot2() tidyverse approach
# see http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/ 
library(ggplot2)

ggplot(helpdata, aes(x=age)) + 
  geom_histogram()

# Histogram overlaid with kernel density curve
# Histogram with density instead of count on y-axis
# Overlay with transparent density plot
ggplot(helpdata, aes(x=age)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="my title", subtitle="my subtitle")

# save plot object
p <- ggplot(helpdata, aes(x=age)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") 

# add layers to plot object
p + labs(title="my title", subtitle="my subtitle")

# create a quick subset
# let's look at age, female, racegrp, cesd, pcs and mcs.
helpset1 <- helpdata %>%
  select(age, female, racegrp, cesd, pcs, mcs)

# investigate normality
qqnorm(helpset1$age)
qqline(helpset1$age)

library(car)
car::qqPlot(helpset1$age)

library(ggpubr)
ggqqplot(helpset1$age)

# Shapiro-Wilk's test of normality - better test
shapiro.test(helpset1$age)

# Kolmogorov_Smirnov test of normality
ks.test(helpset1$age, "pnorm")

# get skewness and kurtosis
library(e1071)
e1071::skewness(helpset1$age)
e1071::kurtosis(helpset1$age)

library(psych)
psych::skew(helpset1$age)
psych::kurtosi(helpset1$age)
psych::mardia(helpset1$age)
psych::describe(helpset1$age)

# ========================================
# look at cesd, pcs and mcs - work on your own
ggplot(helpset1, aes(x=cesd)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram of CESD")

qqnorm(helpset1$cesd,
       main="QQ Plot of CESD")
qqline(helpset1$cesd)

psych::describe(helpset1$cesd)
shapiro.test(helpset1$cesd)

ggplot(helpset1, aes(x=pcs)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram of PCS")

qqnorm(helpset1$pcs,
       main="QQ Plot of PCS")
qqline(helpset1$pcs)

psych::describe(helpset1$pcs)
shapiro.test(helpset1$pcs)

ggplot(helpset1, aes(x=mcs)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram of MCS")

qqnorm(helpset1$mcs,
       main="QQ Plot of MCS")
qqline(helpset1$mcs)

psych::describe(helpset1$mcs)
shapiro.test(helpset1$mcs)

library(pastecs)
vars <- c("age","cesd","pcs","mcs")
options(scipen=100)
options(digits=3)
stat.desc(helpset1[,vars], 
          basic=TRUE,
          norm=TRUE)
# ========================================

# boxplots
boxplot(helpdata$age)

# simple boxplot - get stats
b1 <- boxplot(helpdata$age)
b1$stats
# these 5 numbers are the lower whisker value,
# lower hinge (25th percentile), median,
# upper hinge (75th percentile), upper whisker

# compare to summary and fivenum
summary(helpdata$age)

# another way to get the stats for a boxplot
boxplot.stats(helpdata$age)

# Tukey's five number summary
# min, lower-hinge, median, upper-hinge, max
fivenum(helpdata$age)
help("fivenum")

# there are 9 different types of
# quantile algorithms, type=7 is the default
# see help(quantile)

# default, returns min, 25th, median, 75th, max
quantile(helpdata$age)

# change the probs to get other percentiles
quantile(helpdata$age, probs = c(0.025, 0.975))

# boxplots by groups
# use formula approach using ~
boxplot(age ~ racegrp, data=helpdata,
        main="Age Distributions by Race",
        xlab="Race",
        ylab="Age")

# ggplot2() approach for 1 variable
# put var of interest as y and leave x blank
ggplot(helpdata, aes(x="", y=age)) + 
  geom_boxplot() +
  xlab("") +
  ylab("Age (in years)")

# boxplots of ages by race
ggplot(helpdata, aes(x=racegrp, y=age, fill=racegrp)) + 
  geom_boxplot() +
  xlab("Racial Group") +
  ylab("Age (in years)")

# an alternative to boxes
# ages by race - violin plots
ggplot(helpdata, aes(x=racegrp, y=age, fill=racegrp)) + 
  geom_violin() +
  xlab("Racial Group") +
  ylab("Age (in years)")

# add overlaid dots with jitter
# see examples at http://ggplot2.tidyverse.org/reference/geom_violin.html
# and add title to legend, see http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
ggplot(helpdata, aes(x=racegrp, y=age, fill=racegrp)) + 
  geom_violin() +
  xlab("Racial Group") +
  ylab("Age (in years)") + 
  geom_jitter(height = 0, width = 0.1) +
  labs(title="Violin Plots of Age by Race") +
  guides(fill=guide_legend(title="Racial Group"))

# optional
# violin plots - by race
# using the violinmplot package
library(violinmplot)
violinmplot(age ~ racegrp, 
            data = helpdata)

# categorical data - frequency plots
helpdata %>% 
  ggplot(aes(x=racegrp)) +
    geom_bar(stat="count")

# =======================================
# continuous - continuous
# =======================================

# scatterplot mcs and cesd
plot(helpset1$cesd, helpset1$mcs)

# scatterplot with smoothed fit line added
scatter.smooth(helpset1$cesd, helpset1$mcs)

# smoothed fit line with confidence intervals
library(ggplot2)
qplot(helpset1$cesd, helpset1$mcs, geom='smooth')

# parametric Pearson's R correlation
# non-parametric Spearman's rho
# non-parametric Kendall's tau - good for rank ties

# create variable subset
vars <- c("age","cesd","pcs","mcs")

cor(helpset1[,vars],
    method="pearson")
cor(helpset1[,vars],
    method="spearman")
cor(helpset1[,vars],
    method="kendall")

# can also use the corr.test() in the psych
# package to get the p-values, t-tests,
# and confidence intervals, in addition to
# the correlations
psych::corr.test(helpset1[,vars],
                 method="pearson")

# save the results
pc <- psych::corr.test(helpset1[,vars],
                       method="pearson")
# look at correlations
pc$r

# t-test for each correlation
pc$t

# p-value of each t-test for correlation
pc$p

# confidence intervals for each pair
# removes the diagonal values and tests
pc$ci

# also look at spearmans rho
pc <- psych::corr.test(helpset1[,vars],
                       method="spearman")
pc$ci

# =======================================
# continuous with 2-group categorical
# =======================================
# this is basically a t-test
vars <- c("female","age","cesd")
pc <- psych::corr.test(helpset1[,vars],
                       method="pearson")
pc$ci

pc$t

# compare to running a t-test
options(digits=8)
t.test(age ~ female, helpset1)
t.test(cesd ~ female, helpset1)

# non-parametric 2-group tests
# Mann Whitney U test
wilcox.test(age ~ female, helpset1)
wilcox.test(cesd ~ female, helpset1)

# =======================================
# categorical - categorical
# =======================================

# let's make female a factor
# with appropriate labels and levels defined
helpset1$femaleF <- factor(helpset1$female,
                           levels=c(0,1),
                           labels=c("male","female"))

# check
table(helpset1$female)
table(helpset1$femaleF)

# let's merge hispanic/other together for race
helpset1$race3[helpset1$racegrp == "black"] <- "Black"
helpset1$race3[helpset1$racegrp == "white"] <- "White"
helpset1$race3[helpset1$racegrp == "hispanic"] <- "Hisp/Other"
helpset1$race3[helpset1$racegrp == "other"] <- "Hisp/Other"

# check recoding
table(helpset1$racegrp)
table(helpset1$race3)

library(gmodels)

# basic table - formatted like SAS
CrossTable(helpset1$race3, helpset1$female)

# remove row%, total%, chisq contribution
CrossTable(helpset1$race3, helpset1$female,
           expected=TRUE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE)

# add chi-square test of independence
# and Fisher's Exact Test
# learn more help(fisher.test)
CrossTable(helpset1$race3, helpset1$female,
           expected=TRUE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE,
           chisq=TRUE,
           fisher=TRUE)
