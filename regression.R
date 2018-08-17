#' =======================================
#' Regression - fitting models
#' and exploring the results
#' 
#' Melinda Higgins, PhD
#' dated 08/17/2018
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

# get summary stats
summary(helpdata)

