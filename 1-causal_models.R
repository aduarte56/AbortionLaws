################################################################################
##
## Title: Causal Models: the effect of abortion law flexibility on maternal mortality
################################################################################

# Required libraries 
library(tidyverse)
library(tidyr)
library(janitor)
library(data.table)
library(ggplot2)
library(readxl)
library(stringr)
library(lubridate)
library(countrycode)

source("0_data_cleaning.R")


###############################################################################
##
## Basic linear regression
##
###############################################################################

model <- lm(data=df, mortality_ratio_17~flexibility_score+primary_completion+gdp+country_name+period)
summary(model)

model1 <- lm(data=df, mortality_ratio_17~flex_binary_score+primary_completion+gdp+country_name+period)
summary(model1)


###############################################################################
##
## Analysis 
##
###############################################################################
-ATE
-ATE, ATU, ATT Gseparation. 
-Sensitivity analysis: lab sudan. 
- heterogeneity of effects (lab 4)
- Test positivity assumption. 
- Test Missingness.

