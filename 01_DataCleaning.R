# Data-cleaning script for alcohol-crosswalking project
# Clean and Prep HRS & NLSY data for cross-walking project
# Annie will provide the 

# Set-up ----

# Clear Environment
rm(list=ls())
gc()
# Libraries
library("tidyverse")

# Leverage Data-pooling cleaning ----
d <- list()
d$hrs <- readRDS("../Alcohol_Cognition/Datasets/hrs_recoded.RDS")
d$nlsy<- readRDS("../Alcohol_Cognition/Datasets/nlsy_recoded.RDS")

# hrs_long is only the subset from the data-pooling
# if not using cognitive scores, could consider using the entire dataset
d$hrs_long <- readRDS("Data/hrs_harmonized.RDS")
d$nlsy_long<- readRDS("Data/nlsy_harmonized.RDS")

# Create Baseline Data-set ----
d_cleaned <- list()

## NLSY ----
# Join in baseline characteristics
d_cleaned$nlsy <- right_join(d$nlsy %>% select(CASE_ID_NLSY_BL, 
                                               RACE_ETH_NLSY_BL, 
                                               FEMALE_NLSY_BL),
                             d$nlsy_long) %>%
  # Use the years most similar to HRS (2012, 2014)
  filter(Year == 2014) %>%
  # Remove observations missing this wave
  filter(!is.na(AGEINTERVIEW)) %>%
  filter(AGEINTERVIEW >= 50) %>%
  # Select the variables of interest (REVISE AS NECESSARY)
  select(CASE_ID_NLSY_BL, 
         AGEINTERVIEW, RACE_ETH_NLSY_BL, FEMALE_NLSY_BL, EDU_NEW,
         alcohol_lastmo, alcohol_fq6drinks,
         alcohol_ndayspastmo, alcohol_ndrinksavgday,
         CESD_NEW6PT, GENHEALTH_ORD,
         RATE_MEM_COG,RATE_PAST_COG, IMMED_RECALL,DELAY_RECALL,SERIAL_7,
         -all_of(ends_with("_z"))) %>%
  # Account for skip pattern
  mutate(alcohol_fq6drinks = case_when(alcohol_lastmo == "No" ~ "Never in the last 30 days",
                                       TRUE ~ alcohol_fq6drinks),
         alcohol_fq6drinks = factor(alcohol_fq6drinks,
                                    levels = c("Never in the last 30 days",
                                               "Less often than once a week",
                                               "1 or 2 times per week",
                                               "3 or 4 times per week",
                                               "5 or 6 times per week",
                                               "Everyday")),
         alcohol_ndayspastmo=case_when(alcohol_lastmo == "No" ~ 0,
                                       TRUE ~ alcohol_ndayspastmo),
         alcohol_ndrinksavgday=case_when(alcohol_lastmo == "No" ~ 0,
                                         TRUE ~ alcohol_ndrinksavgday),
         # Re-code age to match input 
         AGE_GRP = case_when(AGEINTERVIEW < 55 ~ "50-54",
                             AGEINTERVIEW < 60 ~ "55-60"),
         
         # Factor FEMALE
         FEMALE = case_when(FEMALE_NLSY_BL == 0 ~ "Male",
                            FEMALE_NLSY_BL == 1 ~ "Female"),
         FEMALE = as.factor(FEMALE),
         FEMALE = relevel(FEMALE, "Male"),
         
         # Factor RACEETH_NEW
         RACEETH_NEW = RACE_ETH_NLSY_BL,
         RACEETH_NEW = as.factor(RACEETH_NEW),
         RACEETH_NEW = relevel(RACEETH_NEW, "White"),
         
         # Factor AGE_GRP
         AGE_GRP = as.factor(AGE_GRP),
         AGE_GRP = relevel(AGE_GRP, "50-54")) %>%
  rename(ALC_FREQ.2_N = alcohol_ndayspastmo,
         ALC_QUANT_N= alcohol_ndrinksavgday) %>% 
  select(-AGEINTERVIEW,-FEMALE_NLSY_BL,-RACE_ETH_NLSY_BL,-alcohol_lastmo, 
         -alcohol_fq6drinks,
         -RATE_MEM_COG,-RATE_PAST_COG,-IMMED_RECALL,-DELAY_RECALL,-SERIAL_7)



## HRS ----

# Merge in baseline covariates
d_cleaned$hrs <- right_join(d$hrs %>% select(CASE_ID_HRS_RA,
                                             RACE_ETH_HRS_RA, FEMALE_HRS_RA,
                                             firstwave_HRS),
                            d$hrs_long) %>%
  # limit to wave joined
  filter(Wave == firstwave_HRS) %>% 
  filter(AGEINTERVIEW >= 50) %>%
  select(CASE_ID_HRS_RA,
         AGEINTERVIEW, RACE_ETH_HRS_RA, FEMALE_HRS_RA, EDU_NEW,
         #starts_with("alc"),
         ALCOHOL_NOW,
         alc_nweek,alc_ndrink,
         CESD_NEW6PT, GENHEALTH_ORD,
         Wave,
         #RATE_MEM_COG, RATE_PAST_COG, IMMED_RECALL, DELAY_RECALL, SERIAL_7,
         -ends_with("_z")) %>% 
  mutate(AGE_GRP = case_when(AGEINTERVIEW < 55 ~ "50-54",
                             AGEINTERVIEW < 60 ~ "55-59",
                             AGEINTERVIEW < 65 ~ "60-64",
                             AGEINTERVIEW < 70 ~ "65-69",
                             AGEINTERVIEW < 75 ~ "70-74",
                             AGEINTERVIEW < 80 ~ "75-79",
                             AGEINTERVIEW < 85 ~ "80-84",
                             AGEINTERVIEW>= 85 ~ "85+"),
         
         # Factor Female
         FEMALE = case_when(FEMALE_HRS_RA == 0 ~"Male",
                            FEMALE_HRS_RA == 1 ~"Female"),
         FEMALE = as.factor(FEMALE),
         FEMALE = relevel(FEMALE, "Male"),
         
         # Factor RACEETH_NEW
         RACEETH_NEW = RACE_ETH_HRS_RA,
         RACEETH_NEW = as.factor(RACEETH_NEW),
         RACEETH_NEW = relevel(RACEETH_NEW, "White"),
         
         # Factor AGE_GRP
         AGE_GRP = as.factor(AGE_GRP),
         AGE_GRP = relevel(AGE_GRP, "50-54")) %>%
  rename(ALC_FREQ_H = alc_nweek,
         ALC_QUANT_H= alc_ndrink) %>%
  select(-AGEINTERVIEW, -FEMALE_HRS_RA,
         #-ALCOHOL_NOW, 
         -RACE_ETH_HRS_RA)

# Save ----
saveRDS(d_cleaned, "Data/cleaned_HRS_NLSY.RDS")

# Next steps: 
# STACK THE DATASETS
# START WITH JUST THE OTHER ALCOHOL VARIABLE AND BUILD UP
# IMPUTATION ACROSS K DATASETS INCOPROTAING 
# USE MICE TO IMPUTE THE "OTHER" QUESTION
# RUN REGRESSION TO FIND THE ASSOCIATION, GETTING SEVERAL VARIANCES