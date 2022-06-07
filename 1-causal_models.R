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
library(MatchIt)
library(fixest) # for fixed effect regression estimation

# Reading the dataset
df1 <- read_csv("Data/final_dataset.csv")

###############################################################################
##
## Calculating  ATE 
##
###############################################################################

#Linear regression model: mortality rate on flexibility of abortion laws controlling for 
#gdp, countries and periods
model <- lm(data=df1, mortality_ratio_17~flexibility_score+gdp+country_name+period)
summary(model)

#Linear regression model: mortality rate on binary flexibility score controlling for 
#gdp, countries and periods
model1 <- lm(data=df1, mortality_ratio_17~flex_binary_score+gdp+country_name+period)
summary(model1)

###############################################################################
##
## Empirical Estimation.
##
###############################################################################
#
# G-separation
#______________________________________________________________________________


##
##Calculating ATE using G-separation
## Create predicted Y_A for all observations, sets flex_score=f
Ypred<-predict(model1)
#Generate data sets where a is set to 0 and 1
df.flex_score0<-transform(df1, flex_binary_score=0) 
df.flex_score1<-transform(df1, flex_binary_score=1)
## Create Y_0 for all observations, sets flex_score=0
Y0<-predict(model1,newdata=df.flex_score0) 
## Create Y_1 for all observations, sets flex_score=1
Y1<-predict(model1,newdata=df.flex_score1)
## Calculate (Y_1 - Y_0) for each individual
ATE<-Y1-Y0

##Calculating ATT using G-separation

df.flex_score0 <- df.flex_score0 %>% mutate(observed_bin_score=df1$flex_binary_score)
df.flex_score1<- df.flex_score1 %>% mutate(observed_bin_score=df1$flex_binary_score)

df_T0 <- df.flex_score0 %>% filter(observed_bin_score==1)
df_T<-df1 %>% filter(flex_binary_score==1)
Y_T0<-predict(model1,newdata=df_T0) 
Y_T1<-predict(model1,newdata=df_T)
ATT<-Y_T0-Y_T1
##Calculating ATU using G-separation

df_U <- df1 %>% filter(flex_binary_score==0)
df_U1<-transform(df_U, flex_binary_score=1) 
Y_U0.S1<-predict(model1,newdata=df_U) 
Y_U.S1<-predict(model1,newdata=df_U1)
ATU<-Y_U0.S1-Y_U.S1

#------------------------------------------------------------------------------
# Sensitivity Analysis
#______________________________________________________________________________




#------------------------------------------------------------------------------
# Heterogeneity of effects (lab4)
#______________________________________________________________________________




- Test positivity assumption. 
- Test Missingness.

