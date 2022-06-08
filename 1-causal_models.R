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
library(sensemakr)

source("0_data_cleaning.R")

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

model2 <- lm(data=df, mortality_ratio_17~flex_binary_score+gdp+primary_completion+country_name+period)
summary(model2)

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

df_T0 <- df.flex_score0 %>% filter(observed_bin_score==1) #imputed results for treatment 0 for those who were treated
#df_T<-df1 %>% filter(flex_binary_score==1)
Y_T0<-predict(model1,newdata=df_T0) # prediction for imputed treatment 0
#Y_T1<-predict(model1,newdata=df_T)
ATT<-mean(Y_T0-df_T0$mortality_ratio_17) # difference between prediction and results
##Calculating ATU using G-separation

df_U <- df1 %>% filter(flex_binary_score==0)
df_U1<-transform(df_U, flex_binary_score=1) 
Y_U0.S1<-predict(model1,newdata=df_U) 
Y_U.S1<-predict(model1,newdata=df_U1)
ATU<-Y_U0.S1-Y_U.S1


df_T1 <- df.flex_score0 %>% filter(observed_bin_score==0) #imputed results for treatment 0 for those who were treated
#df_T<-df1 %>% filter(flex_binary_score==1)
Y_T1<-predict(model1,newdata=df_T1) # prediction for imputed treatment 0
#Y_T1<-predict(model1,newdata=df_T)
ATU<-mean(Y_T1-df_T1$mortality_ratio_17)
#------------------------------------------------------------------------------
# Sensitivity Analysis
#______________________________________________________________________________
df1.sensitivity <- sensemakr(model = model1, 
                                treatment = "flex_binary_score", # the treatment
                                benchmark_covariates = "gdp", # covariates that could be used to bound
                                kd = 1:3) # here we want to investigate the maximum strength of a confounder once,twice and three times as strong as female in explaining treatment and outcome variance
summary(df1.sensitivity)

plot(df1.sensitivity)



df.sensitivity <- sensemakr(model = model2, 
                            treatment = "flex_binary_score", # the treatment
                            benchmark_covariates = "primary_completion", # covariates that could be used to bound
                            kd = 1:3) # here we want to investigate the maximum strength of a confounder once,twice and three times as strong as female in explaining treatment and outcome variance
summary(df.sensitivity)

plot(df.sensitivity)
#------------------------------------------------------------------------------
# Heterogeneity of effects (lab4)
#______________________________________________________________________________

library(tidyverse)

#heterogeinity of effects

data <- read.csv("Data/final_dataset.csv")

#models

full <- lm(mortality_ratio_17 ~ flexibility_score + gdp + country_name + period, data=data)
outcome <- lm(mortality_ratio_17 ~ gdp + country_name + period, data=data)
treatment <- lm(flexibility_score ~ gdp + country_name + period, data=data)

#coefficients for the full model

coef(full)

#reconstruction of the effects using FWL

resid_outcome <- resid(outcome)
resid_treatment <- resid(treatment)
fwl <- lm(resid_outcome ~ resid_treatment - 1) #fwl regression
coef(fwl)


resid_FWL <- resid(fwl) #residuals of FWL regression
resid_full <- resid(full) #residuals of the full model

resid_full == r

#reconstructing weights

data <- mutate(data, w = resid_treatment^2)

#Who gets the highest weights? We'll average the weights for observations
#from each country in all of its observations (in different years)

country <- summarise(group_by(data, country_name), mean_w = mean(w))

#And we can look at the *least* influential countries in our ATE:


influence <- arrange(country, mean_w)
head(influence) #least influential countries
tail(influence) #most influential countries


  
least <- c("South Sudan", "Venezuela", "Luxembourg", "Finland", "Brazil", "Israel")
most <- c("SÃ£o TomÃ© & PrÃ­ncipe", "Angola", "Ireland", "Nepal", "Colombia", "Mexico")
  
least_influential <- data %>%
    filter(country_name %in% least) %>%
    ggplot(aes(flexibility_score, mortality_ratio_17)) +
    geom_point(position = "jitter") +
    labs(x = "Flexibility score",
         y = "MMR") +
    theme_bw() +
    facet_wrap(vars(country_name))

ggsave("Figures/least.png", dpi = 200, least_influential)

most_influential <- data %>%
  filter(country_name %in% most) %>%
  ggplot(aes(flexibility_score, mortality_ratio_17)) +
  geom_point(position = "jitter") +
  labs(x = "Flexibility score",
       y = "MMR") +
  theme_bw() +
  facet_wrap(vars(country_name))

ggsave("Figures/most.png", dpi = 200, most_influential)

#effective sample

effective_sample <- data %>% group_by(country_name) %>%
  summarise(weight = sum(w)) %>%
  mutate(weight_avg = weight/sum(weight),
         weight_pct = weight_avg * 100)

effective_sample %>% arrange(desc(weight_pct)) %>%
  mutate(cum_sum = cumsum(weight_pct)) %>%
  filter(cum_sum <= 52)

#plotting nominal and effective sample

nominal_sample <- effective_sample %>% arrange(desc(weight_pct)) %>%
  mutate(cum_sum = cumsum(weight_pct)) %>%
  filter(cum_sum <= 52) %>%
  pull(country_name)

nominal_sample[nominal_sample=="SÃ£o TomÃ© & PrÃ­ncipe"] <- "Sao Tome and Principe"
nominal_sample[nominal_sample=="CÃ´te dâ€™Ivoire"] <- "Côte d'Ivoire"
nominal_sample[nominal_sample=="Eswatini"] <- "Swaziland"
nominal_sample %in% world$brk_name


library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
nominal_world <- world %>% filter(brk_name %in% nominal_sample)

#effective sample plot
effective_plot <- ggplot(data = world) +
  geom_sf(data = world, fill = "white") +
  geom_sf(data = nominal_world, fill = "red")

ggsave("Figures/effective_sample.png", dpi = 200, effective_plot)

#full sample plot
full_sample <- data %>% 
  distinct(country_code) %>%
  pull(country_code)

full_sample %in% world$iso_a3 

full_world <- world %>% filter(iso_a3 %in% full_sample)

full_plot <- ggplot(data = world) +
  geom_sf(data = world, fill = "white") +
  geom_sf(data = full_world, fill = "black")

ggsave("Figures/full_sample.png", dpi = 200, full_plot)



  


- Test positivity assumption. 
- Test Missingness.

