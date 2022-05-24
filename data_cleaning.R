## ---------------------------
##
## Title: Data_Cleaning
##---------------------------


# Importing required libraries 
library(tidyverse)
library(tidyr)
library(janitor)
library(data.table)
library(ggplot2)
library(rgdal)
library(readxl)
library(stringr)

## ---------------------------
## 1.Importing the Maternal Mortality Datasets
## ---------------------------

#Maternal Mortality until 2017 no data on age groups
maternal_mortality_17 <-read.csv("Data/MMortalityWHO_2017.csv") 

maternal_mortality_17 <- maternal_mortality_17 %>% clean_names() %>% 
  cbind(str_split_fixed(maternal_mortality_17$Maternal.mortality.ratio..per.100.000.live.births.,"\\[", 2)) %>%
  rename("mortality_ratio"="1", "mortality_treshold"= "2") %>% 
  cbind(str_split_fixed(maternal_mortality_17$Number.of.maternal.deaths,"\\[", 2)) %>% 
  rename("number_deaths"="1", "deaths_treshold"= "2") %>% 
  select(-c(maternal_mortality_ratio_per_100_000_live_births,number_of_maternal_deaths))



maternal_mortality_17 <- 
  maternal_mortality_17 %>%  mutate(mortality_ratio=str_remove_all(maternal_mortality_17$mortality_ratio, " "),
                                    mortality_treshold=str_remove_all(maternal_mortality_17$mortality_treshold, "\\]"),
                                    mortality_treshold=str_remove_all(maternal_mortality_17$mortality_treshold, " "),
                                    number_deaths=str_remove_all(maternal_mortality_17$number_deaths, " "),
                                    deaths_treshold=str_remove_all(maternal_mortality_17$deaths_treshold, "\\]"),
                                    deaths_treshold=str_remove_all(maternal_mortality_17$deaths_treshold, " ")) %>% 
  select(country, year, mortality_ratio, number_deaths)


#Maternal Mortality until 2021 per age group
maternal_mortality_21 <-read.csv("Data/MortalityDatabase2021.csv", sep = ";") 
maternal_mortality_21 <- maternal_mortality_21 %>% clean_names() %>% 
  select(country_name, year, age_group_code, number, percentage_of_cause_specific_deaths_out_of_total_deaths, death_rate_per_100_000_population)
  
## ---------------------------
## 2.Importing the Global Abortion Policies Datasets
## ---------------------------

#Dataset containing types of abortion regulation (Source: UN: https://abortion-policies.srhr.org/)
laws <- read_excel("Data/laws.xlsx", na = "na")

#Dataset containing Abortion incidence in the world (Source:https://osf.io/6t4eh/)
incidence <- read.csv("Data/AbortionIncidence.csv")
