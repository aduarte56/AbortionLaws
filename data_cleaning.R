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
library(lubridate)
library(countrycode)

## ---------------------------
## 1.Importing the Maternal Mortality Datasets
## ---------------------------

#Maternal Mortality until 2017 no data on age groups (source: )
maternal_mortality_17 <-read.csv("Data/MMortalityWHO_2017.csv") 

#Changes column names and separates some entries
maternal_mortality_17 <- maternal_mortality_17 %>% clean_names() %>% 
  cbind(str_split_fixed(maternal_mortality_17$Maternal.mortality.ratio..per.100.000.live.births.,"\\[", 2)) %>%
  rename("mortality_ratio"="1", "mortality_treshold"= "2") %>% 
  cbind(str_split_fixed(maternal_mortality_17$Number.of.maternal.deaths,"\\[", 2)) %>% 
  rename("number_deaths"="1", "deaths_treshold"= "2") %>% 
  select(-c(maternal_mortality_ratio_per_100_000_live_births,number_of_maternal_deaths))


#Cleans up the entries that have information of two variables.
maternal_mortality_17 <- 
  maternal_mortality_17 %>%  mutate(mortality_ratio=str_remove_all(maternal_mortality_17$mortality_ratio, " "),
                                    mortality_treshold=str_remove_all(maternal_mortality_17$mortality_treshold, "\\]"),
                                    mortality_treshold=str_remove_all(maternal_mortality_17$mortality_treshold, " "),
                                    number_deaths=str_remove_all(maternal_mortality_17$number_deaths, " "),
                                    deaths_treshold=str_remove_all(maternal_mortality_17$deaths_treshold, "\\]"),
                                    deaths_treshold=str_remove_all(maternal_mortality_17$deaths_treshold, " ")) %>% 
  select(country, year, mortality_ratio, number_deaths)


#Maternal Mortality until 2021 (source: https://platform.who.int/mortality/themes/theme-details/topics/topic-details/MDB/maternal-conditions )
maternal_mortality_21 <-read.csv("Data/MortalityDatabase2021.csv", sep = ";") 
maternal_mortality_21 <- maternal_mortality_21 %>% clean_names() %>% 
  select(country_name, year, age_group_code, number, sex,
         percentage_of_cause_specific_deaths_out_of_total_deaths, death_rate_per_100_000_population) %>% 
  filter(age_group_code=="Age_all")
  
## ---------------------------
## 2.Importing the Global Abortion Policies Datasets
## ---------------------------

#1.Dataset containing types of abortion laws (Source: UN: https://abortion-policies.srhr.org/)
laws <- read_excel("Data/laws.xlsx", na = "na")

#2. Dataset containing Abortion incidence in the world (Source:https://osf.io/6t4eh/)
incidence <- read.csv("Data/AbortionIncidence.csv")

#3. Dataset containing types of abortion regulation per country gathered in 2013, but 
#from 2011. Referenced in the original paper.
#(Source: UN: Economics and social affairs: https://www.un.org/en/development/desa/population/publications/policy/world-abortion-policies-2013.asp)
grounds2011 <- read_excel("Data/AbortionGrounds2013.xlsx", na = "na")

#cleaning the grounds2011 dataset
grounds2011 <- grounds2011 %>%  clean_names() %>% na_if("..") %>% 
  rename("save_life"=to_save_a_womans_life_2011_1, "physical_health"=to_preserve_a_womans_physical_health_2011_2, 
         "mental_health"=to_preserve_a_womans_mental_health_2011_3, "rape_incest"=in_case_of_rape_or_incest_2011_4,
         "foetal_imp"=because_of_foetal_impairment_2011_5, "socioec"=for_economic_or_social_reasons_2011_6, 
         "on_request"=on_request_2011_7) 

grounds2011[grounds2011 == "X"] <- "1"
grounds2011[grounds2011 == "-"] <- "0"

#Creating a column with the aggregate value for the admitted grounds.
grounds2011 <- grounds2011 %>% mutate("sum_grounds"=as.numeric(save_life)+as.numeric(physical_health)+
                                        as.numeric(mental_health)+as.numeric(rape_incest)+as.numeric(foetal_imp)+
                                        as.numeric(socioec)+as.numeric(on_request))  

#4. Dataset containing world abortion policies as of 2021 per regions in federal states.
# (Source: UN: https://abortion-policies.srhr.org/)
grounds2022 <- read_excel("Data/Grounds2022.xlsx", na = " ") %>% clean_names()

grounds2022 <- grounds2022 %>%    
  rename("save_life"= x1a_to_save_a_woman_s_life_yes_no_law_varies_by_jurisdiction, 
         "health"=x1b_to_preserve_a_woman_s_health_yes_no_law_varies_by_jurisdiction,
         "physical_health"=x1c_to_preserve_a_woman_s_physical_health_yes_no_law_varies_by_jurisdiction, 
         "mental_health"=x1d_to_preserve_a_woman_s_mental_health_yes_no_law_varies_by_jurisdiction,
         "cognitive_disability"=x1e_in_cases_of_intellectual_or_cognitive_disability_of_the_woman_yes_no_law_varies_by_jurisdiction,
         "incest"=x1f_in_cases_of_incest_yes_no_law_varies_by_jurisdiction, 
         "rape"=x1g_in_cases_of_rape_yes_no_law_varies_by_jurisdiction,
         "foetal_imp"=x1h_in_cases_of_foetal_impairment_yes_no_law_varies_by_jurisdiction,
         "socioec"=x1i_for_economic_or_social_reasons_yes_no_law_varies_by_jurisdiction, 
         "on_request"=x1j_on_request_yes_no_law_varies_by_jurisdiction,
         "other"=x1k_other_please_specify,
         "national_jurisdiction"=country) %>% 
  mutate(country=countrycode(iso_code, origin = 'iso2c', destination = 'country.name'))

grounds2022[grounds2022 == "Yes"] <- "1"
grounds2022[grounds2022 == "No"] <- "0"
grounds2022[grounds2022 == "Law Varies By Jurisdiction"] <-"0.5"
grounds2022[6:15] <- lapply(grounds2022[6:15], as.numeric)

#Adding the grounds into a flexibility aggregated variable
grounds2022 <- grounds2022 %>% mutate("sum_grounds2022"=rowSums(grounds2022[6:15], na.rm=TRUE))

#Setting a column for year, date and month of the latest abortion policy per country.
grounds2022 <- grounds2022 %>% mutate(date_completed=dmy(date_completed)) %>% 
  mutate(year_completed=year(date_completed), month_completed=month(date_completed), day_completed=day(date_completed)) %>% 
  relocate(country, .after = national_jurisdiction) %>% 
  relocate(c(year_completed, month_completed,day_completed), .after = date_completed)



## ---------------------------
## 3.Other Datasets
## ---------------------------

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
