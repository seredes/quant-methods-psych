################################################################################
##################          REPLICATION R CODES               ##################
################################################################################
#
##@@ INSTRUCTIONS @@##
# Please do not remove any comments from this file, but feel free to add as many
# comments to R code as you like!
#
# Replace all ---USER INPUT--- sections with relevant input (including the - 
# symbols).
#
# When you are done, save this file as RPP_YYYY.R, where YYYY is the OSF code of 
# the project.
# e.g., The first study, Tracing attention and the activation...., should be
# called RPP_qwkum.R (since https://osf.io/qwkum/ is the project site)
#
# For any questions, please contact me at sacha.epskamp@gmail.com
#
##@@ GENERAL INFORMATION @@##
#@ Study Title: Social identity contingencies: How diversity cues signal threat or safety for african americans in mainstream institutions.
#@ Coder name: Dan Martin
#@ Coder e-mail: dpmartin42@gmail.com
#@ Type of statistic: F
#@ Type of effect-size: eta^2
#@ OSF link project: https://osf.io/etg7c/
#@ OSF link replication report: https://osf.io/ymh82/
#
##@@ REQUIRED PACKAGES @@##
# install packages
install.packages("httr")
install.packages("RCurl")
install.packages("sas7bdat")
install.packages("dplyr")
# load corresponding libraries
library("httr")
library("RCurl")
library(sas7bdat)
library(dplyr)

source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
theFile <- getOSFfile("https://osf.io/chq6z/")
myData <- read.sas7bdat(theFile)
head(myData)  

##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

# Get only necessary columns and filter race (should be only 1 or 2)
myDataSub <- dplyr::select(myData, racecode, fair, trust) %>%
  filter(racecode %in% c(1, 2))
head(myDataSub)
# Recode condition and race as a factor
myDataSub$racecode <- as.factor(myDataSub$racecode)
myData$fair <- as.factor(myData$fair)

##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object
the_mod <- lm(trust ~ racecode * fair, data = myDataSub)
anova(the_mod)

##@@ STATISTIC @@##
anova(the_mod)[[4]][3]

##@@ P-VALUE @@##
anova(the_mod)[[5]][3]

##@@ SAMPLE SIZE @@##
nrow(na.omit(myDataSub))

##@@ EFFECT SIZE @@##
anova(the_mod)[[2]][3]/sum(anova(the_mod)[[2]])

##@@ AGREEMENT WITH AUTHORS @@##
TRUE

#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---