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

source("0_data_cleaning.R")


###############################################################################
##
## Calculating  ATE 
##
###############################################################################

model <- lm(data=df, mortality_ratio_17~flexibility_score+primary_completion+gdp+country_name+period)
summary(model)

model1 <- lm(data=df1, mortality_ratio_17~flex_binary_score+gdp+country_name+period)
summary(model1)

model2 <- lm(data=df, mortality_ratio_17~flex_binary_score+primary_completion+gdp+period)
summary(model2)


###############################################################################
##
## Empirical Estimation.
##
###############################################################################
#
# G-separation
#______________________________________________________________________________

df1 <- df %>% select(-c(female_literacy, mortality_rate, primary_completion)) %>% drop_na()
##
##Calculating ATE using G-separation
## Create predicted Y_A for all observations, sets flex_score=f
Ypred.S1<-predict(model1)
#Generate data sets where a is set to 0 and 1
df.flex_score0<-transform(df1, flex_binary_score=0) 
df.flex_score1<-transform(df1, flex_binary_score=1)
## Create Y_0 for all observations, sets flex_score=0
Y0.S1<-predict(model1,newdata=df.flex_score0) 
## Create Y_1 for all observations, sets flex_score=1
Y1.S1<-predict(model1,newdata=df.flex_score1)
## Calculate (Y_1 - Y_0) for each individual
difference.S1<-Y1.S1-Y0.S1
## Create summary table
table2<-cbind(simdata[,c("W1", "W2", "A")], "Y"=round(simdata[,"Y"],2), "Ypred"=round(Ypred.S2,2), "Y_0"=round(Y0.S2,2), "Y_1"=round(Y1.S2,2), "Y_1-Y_0"=round(difference.S2,2))

##Calculating ATT using G-separation
df_T <- df1 %>% filter(flex_binary_score==1)
df_T0<-transform(df_T, flex_binary_score=0) 
Y_T0.S1<-predict(model1,newdata=df_T0) 
Y_T1.S1<-predict(model1,newdata=df_T)
ATT<-Y_T0.S1-Y_T1.S1
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

