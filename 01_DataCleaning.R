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
# hrs_long is only the subset from the data-pooling
# consider subsetting HRS sample to participants who joined in 1998+
d$hrs_long <- readRDS("Outputs/hrs_harmonized.RDS")
d$nlsy_long<- readRDS("Outputs/nlsy_harmonized.RDS")

# Create Baseline Data-set ----
d_cleaned <- list()

## NLSY ----
# Join in baseline characteristics
d_cleaned$nlsy <- d$nlsy_long %>%
  # Use the years most similar to HRS (2012, 2014) 
  # REVIEW THIS WITH ANNIE
  filter(Year == 2014) %>%
  # Remove observations missing this wave
  filter(!is.na(AGEINTERVIEW)) %>%
  filter(AGEINTERVIEW >= 50) %>%
  # Select the variables of interest (REVISE AS NECESSARY)
  select(CASE_ID_NLSY_BL, 
         AGEINTERVIEW, RACE_ETH, FEMALE, EDU_NEW,
         alcohol_lastmo, alcohol_fq6drinks,
         alcohol_ndayspastmo, alcohol_ndrinksavgday,
         CESD_NEW6PT, GENHEALTH_ORD,
         RATE_MEM_COG,RATE_PAST_COG, IMMED_RECALL,DELAY_RECALL,SERIAL_7,
         -all_of(ends_with("_z"))) %>%
  # Account for skip pattern
  mutate(alcohol_fq6drinks = 
           case_when(alcohol_lastmo == "No" ~ "Never in the last 30 days",
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
         FEMALE = case_when(FEMALE == 0 ~ "Male",
                            FEMALE == 1 ~ "Female"),
         FEMALE = as.factor(FEMALE),
         FEMALE = relevel(FEMALE, "Male"),
         
         # Factor RACEETH_NEW
         RACEETH_NEW = RACE_ETH,
         RACEETH_NEW = as.factor(RACEETH_NEW),
         RACEETH_NEW = relevel(RACEETH_NEW, "White"),
         
         # Factor AGE_GRP
         AGE_GRP = as.factor(AGE_GRP),
         AGE_GRP = relevel(AGE_GRP, "50-54")) %>%
  rename(ALC_FREQ.2_N = alcohol_ndayspastmo,
         ALC_QUANT_N= alcohol_ndrinksavgday) %>% 
  select(-AGEINTERVIEW,-alcohol_lastmo, 
         -alcohol_fq6drinks,
         -RATE_MEM_COG,-RATE_PAST_COG,
         #-IMMED_RECALL,-DELAY_RECALL,
         -SERIAL_7)



## HRS ----

# Merge in baseline covariates
d_cleaned$hrs <- d$hrs_long %>%
  # limit to wave joined
  filter(Wave == firstwave_HRS) %>% 
  filter(AGEINTERVIEW >= 50) %>%
  select(CASE_ID_HRS_RA,
         AGEINTERVIEW, RACE_ETH, FEMALE, EDU_NEW,
         #starts_with("alc"),
         ALCOHOL_NOW,
         alc_nweek,alc_ndrink,
         CESD_NEW6PT, GENHEALTH_ORD,
         Wave,
         #RATE_MEM_COG, RATE_PAST_COG, 
         IMMED_RECALL, DELAY_RECALL, #SERIAL_7,
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
         FEMALE = case_when(FEMALE == 0 ~"Male",
                            FEMALE == 1 ~"Female"),
         FEMALE = as.factor(FEMALE),
         FEMALE = relevel(FEMALE, "Male"),
         
         # Factor RACEETH_NEW
         RACEETH_NEW = RACE_ETH,
         RACEETH_NEW = as.factor(RACEETH_NEW),
         RACEETH_NEW = relevel(RACEETH_NEW, "White"),
         
         # Factor AGE_GRP
         AGE_GRP = as.factor(AGE_GRP),
         AGE_GRP = relevel(AGE_GRP, "50-54")) %>%
  rename(ALC_FREQ_H = alc_nweek,
         ALC_QUANT_H= alc_ndrink) %>%
  select(-AGEINTERVIEW)

# Save ----
saveRDS(d_cleaned, "Outputs/cleaned_HRS_NLSY.RDS")

# Next steps: 
# STACK THE DATASETS
# START WITH JUST THE OTHER ALCOHOL VARIABLE AND BUILD UP
# IMPUTATION ACROSS K DATASETS INCOPROTAING 
# USE MICE TO IMPUTE THE "OTHER" QUESTION
# RUN REGRESSION TO FIND THE ASSOCIATION, GETTING SEVERAL VARIANCES