################################################################################
##
## Title: Data_Cleaning
################################################################################
# Contains cleaning code for datasets on: 
# 1. maternal mortality
# 2. Grounds on which abortion is permitted in the world 
# 3. GDP 
# 4. Female education (including both primary completion and female literacy)
# 
#  Joins all these datasets in the fifth step. 
################################################################################
##
## Required Libraries
##
## #############################################################################

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
##
## 1.Maternal Mortality Datasets
## - maternal_mortality_17 contains a maternal mortality ratio on all countries from 2000 to 2017
## - MMcomplete cointains data on maternal mortality from 1986 to 2021
## - both sets are cleaned and joined into the MMR dataset (since the two rates are different, 
##  they are included on different columns
## 
## #############################################################################
# 
# A. Maternal Mortality from 2000 to 2017 
#_______________________________________________________________________________

# Reading the csv file
maternal_mortality_17 <-read.csv("Data/MMortalityWHO_2017.csv") 

#Cleaning column names and separating indicator values from confidence intervals into two columns 
maternal_mortality_17 <- maternal_mortality_17 %>% clean_names() %>% 
  cbind(str_split_fixed(maternal_mortality_17$Maternal.mortality.ratio..per.100.000.live.births.,"\\[", 2)) %>%
  rename("mortality_ratio_17"="1", "mortality_treshold"= "2") %>% 
  cbind(str_split_fixed(maternal_mortality_17$Number.of.maternal.deaths,"\\[", 2)) %>% 
  rename("number_deaths"="1", "deaths_treshold"= "2") %>% 
  select(-c(maternal_mortality_ratio_per_100_000_live_births,number_of_maternal_deaths))


#Removes special characters in the confidence intervals columns
maternal_mortality_17 <- 
  maternal_mortality_17 %>%  mutate(mortality_ratio_17=str_remove_all(maternal_mortality_17$mortality_ratio_17, " "),
                                    mortality_treshold=str_remove_all(maternal_mortality_17$mortality_treshold, "\\]"),
                                    mortality_treshold=str_remove_all(maternal_mortality_17$mortality_treshold, " "),
                                    number_deaths=str_remove_all(maternal_mortality_17$number_deaths, " "),
                                    deaths_treshold=str_remove_all(maternal_mortality_17$deaths_treshold, "\\]"),
                                    deaths_treshold=str_remove_all(maternal_mortality_17$deaths_treshold, " "), 
                                    year=as.character(year), mortality_ratio_17=as.numeric(mortality_ratio_17)) 

#Chooses relevant columns
maternal_mortality_17 <- maternal_mortality_17 %>% rename("country_name"=country) %>% 
  mutate(country_code =countrycode(country_name, origin = 'country.name', destination = 'iso3c')) %>% 
  select(country_code, year, mortality_ratio_17) 
  

# ______________________________________________________________________________
# 
# B. Maternal Mortality from 1986 to 2021 
#_______________________________________________________________________________

# Reading the dataset
MMComplete <- read_excel("Data/MaternalMortalityComplete.xls", 
                  na = " ", skip=3, .name_repair = "minimal") %>% clean_names()

# Filters out entries not corresponding to countries (by code)
non_countries <- c("AFE", "AFW", "ARB", "CEB", "CHI", "CSS", "EAP", "EAR", "EAS", "ECA", "ECS", "EMU",
               "EUU", "FCS", "HIC", "HPC", "IBD", "IBT", "IDA", "IDB", "IDX", "INX", "LAC", "LCN", "LDC", "LIC", "LMC", "LMY", "LTE", "MEA", "MIC", "MNA",
               "NAC", "OED", "OSS", "PRE", "PSS", "PST", "SAS", "SSA", "SSF", "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD", "XKX")

MMComplete <- MMComplete %>% filter(!(country_code %in% non_countries)) 

# Standardizes countries' and columns' names. Changes the table into a long format table.
MMComplete <- MMComplete %>% 
  mutate (country_name =countrycode(country_code, origin = 'iso3c', destination = 'country.name')) %>% 
  select(-c(indicator_name, indicator_code)) %>% rename_with(~str_remove(., 'x')) %>% 
  select(country_name, country_code, as.character(seq(1996,2021, by=1))) %>% 
  pivot_longer(cols=as.character(seq(1996,2021, by=1)), names_to = "year",values_to="mortality_rate")


#Joining the Mortality rate 2017 with the complete MMrate dataset
MMR <- full_join(MMComplete, maternal_mortality_17, by = c("country_code", "year")) %>% select(-country_name)


## #############################################################################
##
## 2. ABORTION POLICIES DATASETS
##
## #############################################################################
#-------------------------------------------------------------------------------
# 
# POLICIES FROM 1996 TO 2015: 9 datasets obtained from the WHO archive webpage
#_______________________________________________________________________________

## Cleaning up the abortion policies datasets since 1996 until 2013 through a function.
## The function takes each file, standardizes their format and creates a flexibility score column:

# Creates a list of files on the PoliciesSince1996 folder
folder="Data/PoliciesSince1996/"
files=list.files(folder)
total <- length(files)

#Creates a new folder to store the cleaned datasets
#dir.create("Data/CleanedPoliciesSince1996")

for (i in 1:total) {
  #reads each file
  file_path=paste0("Data/PoliciesSince1996/", files[i],"")
  file_i=read_excel(file_path, 
                    na = " ", skip=1, .name_repair = "universal") %>% clean_names()
  
  #gets the year of each file (to be used for naming new files)
  year_i=as.character(file_i$government_support_for_family_planning[1])
  # Calculates flexibility scores per file and selects relevant columns
  file_i <- file_i %>% mutate(flexibility_score=round_half_up(nchar(grounds_on_which_abortion_is_permitted)/2), 
                              flexibility_score=ifelse(grounds_on_which_abortion_is_permitted =="No data available", NA, flexibility_score),
                              flexibility_score=ifelse(grounds_on_which_abortion_is_permitted =="Not permitted",0, flexibility_score)) %>% 
    filter(country_code!= is.na(country_code)) %>% mutate(year=year_i) %>%
    rename("grounds"=grounds_on_which_abortion_is_permitted, "government_support"=government_support_for_family_planning) %>% 
    select(country_name, country_code, region, development_level, least_developed_country, government_support, grounds, flexibility_score, year)
  #Writes the clean file on 
  write.csv(file_i,paste0("Data/CleanedPoliciesSince1996/",year_i,".csv"), row.names = FALSE)
} 

##The second function puts all the files together by binding their rows. 
folder1="Data/CleanedPoliciesSince1996/"
files1=list.files(folder1)
total <- length(files1)
complete_grounds=read_csv("Data/CleanedPoliciesSince1996/1996.csv")

for (i in 2:total) {
  #reads the new files
  file_path=paste0("Data/CleanedPoliciesSince1996/", files1[i],"")
  file_i=read_csv(file_path)
  #appends the rows of all the remaining files
  complete_grounds=rbind(complete_grounds, file_i)
  
} 

#selects relevant columns
complete_grounds <- complete_grounds %>% mutate(year=as.character(year)) %>% 
  select(-c(region, development_level, least_developed_country, government_support )) %>% 
  rename("numeric_country_code"=country_code) 
  
#recodes the country_code into a charactercode. Filters out observations from Sudan (including south sudan)
# and Serbia and Montenegro
complete_grounds <- complete_grounds%>% filter(numeric_country_code != 736 & numeric_country_code != 891) %>% 
  mutate(country_code =countrycode(numeric_country_code, origin = 'iso3n', destination = 'iso3c')) %>% 
  select(-c(numeric_country_code, country_name, grounds)) %>% relocate(country_code,.before =flexibility_score)

#-------------------------------------------------------------------------------
# 
# POLICIES FROM 2017: WHO archive webpage
#_______________________________________________________________________________

#Reading the file
grounds2017 <- read_excel("Data/PoliciesSince2015/Policies2017.xlsx", 
                         na = " ", skip=7, .name_repair = "minimal", sheet = 2) %>% clean_names()

#renaming columns, creating column names and removing empty columns
grounds2017 <- grounds2017 %>%    
  rename("save_life"= x1_a_to_save_a_womans_life, 
         "health"=x1_b_to_preserve_a_womans_health,
         "physical_health"=x1_c_to_preserve_a_womans_physical_health, 
         "mental_health"=x1_d_to_preserve_a_womans_mental_health,
         "cognitive_disability"=x1_e_in_cases_of_intellectual_or_cognitive_disability_of_the_woman,
         "incest"=x1_f_in_cases_of_incest, 
         "rape"=x1_g_in_cases_of_rape,
         "foetal_imp"=x1_h_in_cases_of_foetal_impairment,
         "socioec"=x1_i_for_economic_or_social_reasons, 
         "on_request"=x1_j_on_request,
         "other"=x1_k_other_legal_grounds,
         "country_name"=x_2,
         "numeric_country_code"=x_3) %>% 
  filter(numeric_country_code!= is.na(numeric_country_code), country_name!="Region or country") %>% 
  select(-c(x,x_4, x1_k_footnote, x1_footnote))

#recoding country_code from numeric to character code. Rearranging columns
grounds2017 <- grounds2017 %>% 
  mutate(country_code=countrycode(numeric_country_code, origin = 'iso3n', destination = 'iso3c')) %>% 
  select(-c(health, cognitive_disability, numeric_country_code, other)) %>% filter(country_code!=is.na(country_code)) %>% 
  relocate(country_code, .after = country_name)

#Turning yes-no values, binary  
grounds2017[grounds2017 == "Yes"] <- "1"
grounds2017[grounds2017 == "No"] <- "0"
grounds2017[grounds2017 == "―"] <- NA
grounds2017[3:10] <- lapply(grounds2017[3:10], as.numeric) 

#Adding the grounds into a flexibility aggregated variable after creating a single variable for rape and incest
grounds2017 <- grounds2017 %>% 
  mutate(incest_rape=case_when(incest==1 | rape==1~1,incest==0 & rape==0~0 )) %>%
  select(-incest, -rape) %>% relocate(incest_rape, .after = on_request) %>% 
  mutate("flexibility_score"=rowSums(grounds2017[3:9], na.rm=TRUE), year="2017") %>%
  select(country_code, flexibility_score, year)

#Appending 2017 data to the complete_grounds dataset
complete_grounds=rbind(complete_grounds, grounds2017)

#-------------------------------------------------------------------------------
# ABORTION POLICIES BETWEEN 2018-2022
#-------------------------------------------------------------------------------

# Dataset containing world abortion policies as of 2022 per regions in federal states.
# (Source: UN: https://abortion-policies.srhr.org/)
grounds2022 <- read_excel("Data/PoliciesSince2015/Grounds2022.xlsx", na = " ") %>% clean_names()

#renaming columns and creating column names 
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
  mutate(country_code=countrycode(iso_code, origin = 'iso2c', destination = 'iso3c'), 
         country_name=countrycode(country_code, origin = 'iso3c', destination = 'country.name')) %>% 
  select(-c(health, cognitive_disability))
  
#Changing yes and no's to binary variables
grounds2022[grounds2022 == "Yes"] <- "1"
grounds2022[grounds2022 == "No"] <- "0"
grounds2022[grounds2022 == "Law Varies By Jurisdiction"] <-"0.5"
grounds2022[6:13] <- lapply(grounds2022[6:13], as.numeric)

#Adding the grounds into a flexibility aggregated variable after creating a single variable for rape and incest
grounds2022 <- grounds2022 %>% mutate(incest_rape=case_when(incest==1 | rape==1~1,incest==0 & rape==0~0 )) %>%
  select(-incest, -rape) %>% relocate(incest_rape, .after = on_request) %>% 
  mutate("flexibility_score"=rowSums(grounds2022[6:12], na.rm=TRUE))

#Setting a column for year of the latest abortion policy per country.
grounds2022 <- grounds2022 %>% mutate(date_completed=dmy(date_completed)) %>% 
  mutate(year=year(date_completed)) %>% 
  relocate(c(country_name, country_code), .after = national_jurisdiction) %>% 
  relocate(c(year), .after = date_completed) 

#Agreggates the observation per states into a single Country variable
countries_states <- c("Australia", "Bosnia & Herzegovina", "Canada", "China", "Mexico", 
                      "Nigeria", "Switzerland", "United Kingdom" )

temp <- grounds2022 %>% filter(country_name %in% countries_states) %>% 
  select(-c(country_code, year, region, sub_region, iso_code, other, date_completed, flexibility_score)) 

write_excel_csv(temp,"Data/temp.csv")
temp2 <- read_excel("Data/temp1.xlsx", na="NA")
temp2[3:9] <- lapply(temp2[3:9], as.numeric)

temp2 <- temp2 %>% mutate(flexibility_score=rowSums(temp2[3:9], na.rm=TRUE))
                 
no_states <- grounds2022 %>% filter(country_name %in% countries_states, national_jurisdiction %in% temp2$national_jurisdiction) %>% 
  mutate(save_life=temp2$save_life, physical_health=temp2$physical_health, mental_health=temp2$mental_health,
         foetal_imp=temp2$foetal_imp, socioec=temp2$socioec, incest_rape=temp2$incest_rape, on_request=temp2$on_request,
         flexibility_score=temp2$flexibility_score)

grounds2022_2 <- grounds2022 %>% filter(!country_name %in% countries_states) %>% rbind(., no_states) %>% 
  select(country_code, flexibility_score, year) %>% filter(!year=="2017")

complete_grounds=rbind(complete_grounds, grounds2022_2)

## #############################################################################
## 3. GDP DATASET 
## ############################################################################

# Reading the dataset
gdp <- read_excel("Data/gdp.xls", 
                         na = " ", skip=3, .name_repair = "minimal") %>% clean_names()

# Filters out entries not corresponding to countries (by code)
non_countries <- c("AFE", "AFW", "ARB", "CEB", "CHI", "CSS", "EAP", "EAR", "EAS", "ECA", "ECS", "EMU",
               "EUU", "FCS", "HIC", "HPC", "IBD", "IBT", "IDA", "IDB", "IDX", "INX", "LAC", "LCN", "LDC", "LIC", "LMC", "LMY", "LTE", "MEA", "MIC", "MNA",
               "NAC", "OED", "OSS", "PRE", "PSS", "PST", "SAS", "SSA", "SSF", "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD", "XKX")

gdp <- gdp %>% filter(!(country_code %in% non_countries))

# Standardizes countries' and columns' names. Changes the table into a long format table.
gdp <- gdp %>% 
  select(-c(indicator_name, indicator_code)) %>% rename_with(~str_remove(., 'x')) %>% 
  select(country_name, country_code, as.character(seq(1996,2021, by=1))) %>% 
  pivot_longer(cols=as.character(seq(1996,2021, by=1)), names_to = "year",values_to="gdp") %>% 
  select(-country_name)


  

## #############################################################################
## 4. FEMALE EDUCATION
## - contains a female primary completion dataset obtained from: https://data.worldbank.org/indicator/SE.PRM.CMPT.FE.ZS 
## - contains a female literacy dataset: https://data.worldbank.org/indicator/SE.ADT.1524.LT.ZS
## ############################################################################

#------------------------------------------------------------------------------
# primary completion dataset
#-----------------------------------------------------------------------------
# Reading the dataset
primary_completion <- read_excel("Data/primary_completion.xls", 
                  na = " ", skip=3, .name_repair = "minimal") %>% clean_names()

# Filters out entries not corresponding to countries (by code)
non_countries <- c("AFE", "AFW", "ARB", "CEB", "CHI", "CSS", "EAP", "EAR", "EAS", "ECA", "ECS", "EMU",
               "EUU", "FCS", "HIC", "HPC", "IBD", "IBT", "IDA", "IDB", "IDX", "INX", "LAC", "LCN", "LDC", "LIC", "LMC", "LMY", "LTE", "MEA", "MIC", "MNA",
               "NAC", "OED", "OSS", "PRE", "PSS", "PST", "SAS", "SSA", "SSF", "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD", "XKX")

primary_completion <- primary_completion %>% filter(!(country_code %in% non_countries))


# Standardizes countries' and columns' names. Changes the table into a long format table.
primary_completion <- primary_completion %>% 
  select(-c(indicator_name, indicator_code)) %>% rename_with(~str_remove(., 'x')) %>% 
  select(country_name,country_code, as.character(seq(1996,2021, by=1))) %>% 
  pivot_longer(cols=as.character(seq(1996,2021, by=1)), names_to = "year",values_to="primary_completion")%>% 
  select(-country_name)


ggplot(data=primary_completion %>% group_by(country_code), aes(x=year, y=primary_completion)) +
  geom_line(data=primary_completion, aes(color=country_code))+
  geom_point(alpha=0.5)


#------------------------------------------------------------------------------
# female literacy dataset
#-----------------------------------------------------------------------------
# Reading the dataset
female_literacy <- read_excel("Data/female_literacy.xls", 
                                 na = " ", skip=3, .name_repair = "minimal") %>% clean_names()

# Filters out entries not corresponding to countries (by code)
non_countries <- c("AFE", "AFW", "ARB", "CEB", "CHI", "CSS", "EAP", "EAR", "EAS", "ECA", "ECS", "EMU",
               "EUU", "FCS", "HIC", "HPC", "IBD", "IBT", "IDA", "IDB", "IDX", "INX", "LAC", "LCN", "LDC", "LIC", "LMC", "LMY", "LTE", "MEA", "MIC", "MNA",
               "NAC", "OED", "OSS", "PRE", "PSS", "PST", "SAS", "SSA", "SSF", "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD", "XKX")

female_literacy <- female_literacy %>% filter(!(country_code %in% non_countries))


# Standardizes countries' and columns' names. Changes the table into a long format table.
female_literacy <- female_literacy %>% 
  select(-c(indicator_name, indicator_code)) %>% rename_with(~str_remove(., 'x')) %>% 
  select(country_name, country_code, as.character(seq(1996,2021, by=1))) %>% 
  pivot_longer(cols=as.character(seq(1996,2021, by=1)), names_to = "year",values_to="female_literacy")%>% 
  select(-country_name)


## #############################################################################
## 
## 5. Joined dataset: 
## Contains information on maternal mortality, maternal mortality rates, 
## GDP per country and female education 
##
## ############################################################################

#puts the maternal mortality dataset and the complete grounds dataset together
df <- full_join(MMR, complete_grounds, by = c("country_code", "year"))  

# Joins in all the other datasets to the df dataset
datasets=c("gdp", "primary_completion","female_literacy")
total=length(datasets)

for (i in 1:total) {
  #puts the datasets together via a full join
  set_i <- get(datasets[i])
  df <- full_join(df,set_i, by = c("country_code", "year") )
}

#Some modifications to the joined dataset
df <- df %>%mutate(country_name=countrycode(country_code, origin = 'iso3c', destination = 'country.name'), # creates a country_name variable
                   country_name=as.factor(country_name), #makes country_name a factor
                   flex_binary_score=case_when(flexibility_score>=4~1,flexibility_score<4~0 ), #Calculares a binary flexibility score
                   flex_binary_score=as.factor(flex_binary_score), #makes flex_binary_score a factor
                   flexibility_score=as.factor(flexibility_score), #makes flexibility_score a factor
                   period=case_when(year==1996~"1995–1999",
                                    year>=2000 & year<=2004~"2000–2004",
                                    year>=2005 & year<=2009~"2005–2009",
                                    year>=2010 & year<=2014~"2010–2014",
                                    year>=2015 & year<=2018~"2015–2018"),  # calculates a period variable based on 4 years intervals
                   period=as.factor(period),#makes period a factor
                   year=as.factor(year)) %>% #makes period a factor
  filter(year %in% seq(1996,2018, by=1)) #filters data from 1996 to 2015 (matching the abortion policies data we have)
