################################################################################
##
## Title: Data_Cleaning (Other Datasets)
################################################################################

# Importing required libraries 
library(tidyverse)
library(tidyr)
library(janitor)
library(data.table)
library(ggplot2)
library(readxl)
library(stringr)
library(lubridate)
library(countrycode)


## #############################################################################
## OTHER DATASETS
## #############################################################################

#Indicators dataset (still in need of a long format!)
indicators <- read_excel("Data/Indicators_Modified.xlsx", na = " ") %>% clean_names()



#Other restrictions that women may face when trying to get an abortion (still needs some cleaning up!)
other_restrictions <- read_excel("Data/AdditionalRestrictionsModified.xlsx", na = " ") %>% clean_names()

other_restrictions[other_restrictions == "Yes"] <- "1"
other_restrictions[other_restrictions == "No"] <- "0"
other_restrictions[other_restrictions == "Restriction Varies By Jurisdiction"] <-"0.5"


#Dataset on some clinical aspects of abortion access (who performs the abortion, government issued guidelines, etc.)
clinical_aspects<-  read_excel("Data/ClinicalAspectsAbortionCare_modified.xlsx", na = " ") %>% clean_names()

clinical_aspects[clinical_aspects == "No"] <- "0"
clinical_aspects[clinical_aspects == "Restriction Varies By Jurisdiction"] <-"0.5"

#Dataset containing types of abortion laws (Source: UN: https://abortion-policies.srhr.org/)
laws <- read_excel("Data/laws.xlsx", na = "na")

#Dataset containing Abortion incidence in the world (Source:https://osf.io/6t4eh/)
incidence <- read.csv("Data/AbortionIncidence.csv")