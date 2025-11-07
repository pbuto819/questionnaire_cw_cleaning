# Set-up ----

# Clear Environment
rm(list=ls())
gc()
# Libraries
library("tidyverse")

# Helper Functions
user <- dget(file.path("~/Documents/Alcohol_Cognition/Helper Code/Users/peter_buto.R"))
dirs <- dget(file.path("~/Documents/Alcohol_Cognition/Helper Code/dirs.R"))(user)
fr   <- dget(file.path("~/Documents/Alcohol_Cognition/Helper Code/fileReader.R"))(dirs)

# Load Data ----
# Load in each data source's data
d <- list()
d$hrs <- readRDS("~/Documents/Alcohol_Cognition/Datasets/hrs_recoded.RDS") #Contains participants that started in waves 1-3
d$nlsy<- readRDS("~/Documents/Alcohol_Cognition/Datasets/nlsy_recoded.RDS")

# Harmonize all

## Figure out what necessary functions to write/store
## e.g. harmonize Alcohol might have it's own function
   # vs dataset dependent
   # as the dataset are imported, figure the necessary function to harmonize each variable

# Harmonize to see what the variables look like
  # Alcohol ()
    # NLSY: Total number days had a drink in last month (alc_ndrinks_84) (will also need age at this year)
      # Number of days had alcohol in last month
    # HRS:
      # days consumed/week
    # Compare people who have had more than 2x week vs less than? see how this can change

  # Cognition: sum of delayed and immediate recall (age at cognition)

# Create a wide data set for each variable (join together at the end)
# alternatively, create wave specific datasets and if not available, set as NA
 

# Variables Lists ----
# Create lists to store variables of interest

## Invariant ----
# Invariant variables: Case-ID, Birth year, Sex (Female), Birth Country, Race, Ethnicity

# HRS
d$variables$invariant$hrs <- d$hrs %>% 
  select(CASE_ID_HRS_RA,
         BIRTHYEAR_HRS_RA,
         FEMALE_HRS_RA, 
         USBIRTH_HRS_RA,
         #RACE_HRS_RA,ETH_HRS_RA,
         RACE_ETH_HRS_RA,
         MOM_EDU_IND_HRS_RA, DAD_EDU_IND_HRS_RA) 

# NLSY
d$variables$invariant$nlsy<- d$nlsy %>% 
  select(CASE_ID_NLSY_BL,
         BIRTHYEAR_NLSY_BL, 
         FEMALE_NLSY_BL, 
         USBIRTH_NLSY_BL, 
         #RACE_NLSY_BL,ETH_NLSY_BL,
         RACE_ETH_NLSY_BL,
         MOM_EDU_IND_NLSY_BL, DAD_EDU_IND_NLSY_BL) 

## Time-varying ----
# Age at time of interview, Height, Weight, BMI, Self-Reported Health, Physical Activity

d$variables$timevarying$hrs <- d$hrs %>%
  select(CASE_ID_HRS_RA,
         starts_with("AGEINTERVIEW"),
         starts_with("EDU_YEARS_HRS_RA"), # Not time varying
         starts_with("EDU_NEW_HRS_RA"),
         starts_with('DAD_EDU_HRS_RA'), # not time varying
         starts_with("MOM_EDU_HRS_RA"), # not time varying
         starts_with("MARRIAGE"),
         starts_with("MILITARY_"),   # not time varying
         starts_with("INCOME_PP_LOG10"),
         starts_with("RELIGION"),
         starts_with("HEIGHT_"),
         starts_with("WEIGHT_"),
         starts_with("CESD_NEW6PT"),
         starts_with("DIABETES"),
         starts_with("HYPERTENSION"),
         starts_with("CANCER"),
         starts_with("HEARTPROB"),
         starts_with("GENHEALTH"),
         starts_with("LIGHT_EXERCISE"),
         starts_with("VIG_EXERCISE"),
         starts_with('SMOKE_EVER'),
         starts_with("SMOKE_NOW"),
         starts_with("ALCOHOL_NOW"),
         starts_with("ALCOHOL_EVER"),
         starts_with("ALCOHOL_DRINKSPERMO"),
         starts_with("ALCOHOL_BINGE2"),
         starts_with("CAGE_MORNING"),
         starts_with("RATE_MEM_COG_"),
         starts_with("RATE_PAST_COG_"),
         starts_with("IMMED_RECALL"),
         starts_with("DELAY_RECALL"),
         starts_with("SERIAL_7_"),
         starts_with("CAGE"),
         starts_with("INTERVIEW_BEGDT"),
         # For the cross-walk:
         # Re-named alc_ever as "ALCOHOL_NOW"
         starts_with("alc_nweek"),
         starts_with("alc_ndrink")
         ) 

d$variables$timevarying$nlsy <- d$nlsy %>%
  select(CASE_ID_NLSY_BL,
         starts_with("AGEINTERVIEW"),
         starts_with("EDU_COMPLETED"),
         starts_with("EDU_NEW_NLSY"),
         starts_with("DAD_EDU_NLSY_BL"), # not time-varying
         starts_with("MOM_EDU_NLSY_BL"), # Not-time Varying
         starts_with("MARRIAGE"),
         starts_with("MILITARY"),
         starts_with("INCOME_PP_LOG10"),
         starts_with("RELIGION"), # Captures both raised and current
         starts_with("HEIGHT"),
         starts_with("WEIGHT"),
         starts_with("BMI"),
         starts_with("CESD_NEW6PT"),
         starts_with("DIABETES"),
         starts_with("HYPERTENSION"),
         starts_with("CANCER"),
         starts_with("HEARTPROB"),
         starts_with("GENHEALTH"),
         starts_with("LIGHT_EXERCISE"),
         starts_with("VIG_EXERCISE"),
         starts_with('SMOKE_EVER'),
         starts_with("SMOKE_NOW"),
         starts_with("ALCOHOL_NOW"),
         starts_with("ALCOHOL_EVER"),
         starts_with("ALCOHOL_DRINKSPERMO"),
         starts_with("ALCOHOL_BINGE2"),
         starts_with("CAGE_MORNING"),
         starts_with("RATE_MEM_COG_"),
         starts_with("RATE_PAST_COG_"),
         starts_with("IMMED_RECALL_NLSY_"),
         starts_with("DELAY_RECALL_NLSY_"),
         starts_with("SERIAL_7_NLSY_"),
         
         # For cross-walking:
         starts_with("alcohol_lastmo_NLSY"),
         starts_with("alcohol_fq6drinks_NLSY"),
         starts_with("alcohol_ndayspastmo_NLSY"),
         starts_with("alcohol_ndrinksavgday_NLSY")
         
         ) %>%
  select(-AGEINTERVIEW_NLSY_cog,
         #-alcohol_ndayspastmo_NLSY_1992
         -MILITARY_CHECK_NLSY_2016,
         -MILITARY_CHECK_NLSY_2018,
         -MILITARY_CHECK_NLSY_2020)

d$variables$timevarying$nlsy <- d$variables$timevarying$nlsy %>%
  select(-alcohol_ndayspastmo_NLSY_1992)

# Long Data ----
## NLSY ----
# Take the wide time-varying NLSY Data,
nlsy_tv_long <- d$variables$timevarying$nlsy  %>%
  # and pivot to a long dataset (excluding any baseline variables)
  # Include non-time varying variables here
  pivot_longer(cols = -c(CASE_ID_NLSY_BL, 
                         DAD_EDU_NLSY_BL, MOM_EDU_NLSY_BL, 
                         RELIGIONRAISED_NLSY_BL),
               names_to = c(".value", "Year"), # Collapse variables that share the same prefix & create year variable
               names_sep="_NLSY_") %>%  
  mutate(Year = as.numeric(Year))# set year as numeric


## HRS ----

# Repeat for HRS
hrs_tv_long <- d$variables$timevarying$hrs %>%
  select(
    -starts_with("CAGE_MORNING_CORE"),
  ) %>%
  pivot_longer(cols = -c(CASE_ID_HRS_RA,
                        EDU_YEARS_HRS_RA, EDU_NEW_HRS_RA,
                        DAD_EDU_HRS_RA, MOM_EDU_HRS_RA,
                        RELIGION_HRS_RA,MILITARY_HRS_RA),
               names_to = c(".value", "Wave"),
               names_sep = "_HRS_")

hrs_tv_long <- hrs_tv_long %>%
  mutate(
    # Each wave corresponds to a year, recode for ease of comparison
    # CORE files had years as suffixes, recoded to avoid error
    # (93 & 94) = wave 2; (95 & 96) = wave 3
    Year = recode(Wave,
                  `1` = 1992,`2` = 1994,`3` = 1996,`4` = 1998,
                  `5` = 2000,`6` = 2002,`7` = 2004,`8` = 2006,
                  `9` = 2008,`10`= 2010,`11`= 2012,`12`= 2014,
                  `13`= 2016,`14`= 2018,`15`= 2020),
    # Make height and weight in same units as NLSY
    HEIGHT = HEIGHT*39.3701, # converts m to in
    WEIGHT = WEIGHT*2.20462, # converts kg to lbs
    Wave = as.double(Wave)
  ) 


# Carry forward ----
# Fill in any missing data by carrying last observation forward
nlsy_tv_long <- nlsy_tv_long %>% 
  rename(
    DAD_EDU = DAD_EDU_NLSY_BL,
    MOM_EDU = MOM_EDU_NLSY_BL) %>%
  group_by(CASE_ID_NLSY_BL) %>%
  fill(HEIGHT, WEIGHT, LIGHT_EXERCISE, VIG_EXERCISE, GENHEALTH,
       EDU_COMPLETED,EDU_NEW,
       MARRIAGE,MILITARY,INCOME_PP_LOG10,RELIGIONCURRENT,
       CESD_NEW6PT,DIABETES,HYPERTENSION,CANCER,HEARTPROB,GENHEALTH,
       SMOKE_EVER,SMOKE_NOW,
       ALCOHOL_NOW,ALCOHOL_EVER,ALCOHOL_DRINKSPERMO,ALCOHOL_BINGE2,CAGE_MORNING,
       RATE_MEM_COG,	RATE_PAST_COG,	IMMED_RECALL,	DELAY_RECALL,	SERIAL_7) %>%
  ungroup() %>%
  # Re-calculate BMI
  mutate(BMI = (WEIGHT/(HEIGHT^2))*703) %>%
  group_by(CASE_ID_NLSY_BL) %>%
  fill(BMI) %>%
  ungroup() 

# Repeat for HRS
hrs_tv_long <- hrs_tv_long %>% 
  # Rename variables to match NLSY
  rename(RELIGIONCURRENT = RELIGION_HRS_RA,
         EDU_COMPLETED = EDU_YEARS_HRS_RA,
         EDU_NEW = EDU_NEW_HRS_RA,
         DAD_EDU = DAD_EDU_HRS_RA,
         MOM_EDU = MOM_EDU_HRS_RA,
         MILITARY = MILITARY_HRS_RA) %>%
  group_by(CASE_ID_HRS_RA) %>%
  fill(HEIGHT, WEIGHT, LIGHT_EXERCISE, VIG_EXERCISE, GENHEALTH,
       EDU_COMPLETED, EDU_NEW,
       MARRIAGE,MILITARY,INCOME_PP_LOG10,RELIGIONCURRENT,
       CESD_NEW6PT,DIABETES,HYPERTENSION,CANCER,HEARTPROB,GENHEALTH,
       RATE_MEM_COG,	RATE_PAST_COG,	IMMED_RECALL,	DELAY_RECALL,	SERIAL_7,
       SMOKE_EVER,SMOKE_NOW,
       ALCOHOL_NOW,ALCOHOL_DRINKSPERMO,ALCOHOL_BINGE2,CAGE_MORNING) %>%
  #fill(ALCOHOL_EVER, .direction="up") %>% - Scott said leave out for now. 
  ungroup() %>%
  # Calculate BMI in HRS
  mutate(BMI = (WEIGHT/(HEIGHT^2))*703) %>%
  fill(BMI) %>%
  group_by(CASE_ID_HRS_RA) %>%
  fill(BMI) %>%
  ungroup() 

# Trim some extremes ----
hrs_tv_long <- hrs_tv_long %>%
  filter(WEIGHT > min(WEIGHT, na.rm = TRUE) | is.na(WEIGHT),
         BMI > min(BMI, na.rm=TRUE) | is.na(BMI),
         BMI < max(BMI, na.rm=TRUE) | is.na(BMI))

nlsy_tv_long <- nlsy_tv_long %>%
  filter(BMI < max(BMI,na.rm=TRUE) | is.na(BMI))

# Count the number of of waves contributed by each HRS Participant
wave_n <- hrs_tv_long %>%
  group_by(CASE_ID_HRS_RA) %>%
  filter(!is.na(INTERVIEW_BEGDT)) %>%
  summarise(nwaves_contributed = n())
table(wave_n$nwaves_contributed)

# join back into long dataset. 
hrs_tv_long <- left_join(hrs_tv_long, wave_n)


# Some Data Cleaning ----

## re-factor variables ----
# first make categorical variables as ordinal for ease of analysis
nlsy_tv_long <- nlsy_tv_long %>% mutate(
  # These are no longer ordinal; now binary "More than 1x/week" vs not
  # LITEEX_ORD= recode(LIGHT_EXERCISE,
  #                         "More than 1x/week" = 0,
  #                         "1-3/mo" = 1,
  #                         "Less than 1/mo" = 2),
  # VIGEX_ORD= recode(VIG_EXERCISE,
  #                        "More than 1x/week" = 0,
  #                        "1-3/mo" = 1,
  #                        "Less than 1/mo" = 2),
  GENHEALTH_ORD=recode(GENHEALTH,
                            "Excellent" = 0,
                            "Very Good" = 1,
                            "Good" = 2,
                            "Fair" = 3,
                            "Poor" = 4),
  ALCOHOL_BINGE2_ORD = recode(ALCOHOL_BINGE2,
                                   "0 days" = 0,
                                   "1-3 days" = 1,
                                   "4-9 days" = 2,
                                   "10 days or more" = 3))


# first make categorical variables as ordinal for ease of analysis
hrs_tv_long <- hrs_tv_long %>% mutate(
  # now binary: "More than 1x/week" vs not
  # LITEEX_ORD = recode(LIGHT_EXERCISE,
  #                         "More than 1x/week" = 0,
  #                         "1-3/mo" = 1,
  #                         "Less than 1/mo" = 2),
  # VIGEX_ORD = recode(VIG_EXERCISE,
  #                        "More than 1x/week" = 0,
  #                        "1-3/mo" = 1,
  #                        "Less than 1/mo" = 2),
  GENHEALTH_ORD=recode(GENHEALTH,
                           "Excellent" = 0,
                           "Very Good" = 1,
                           "Good" = 2,
                           "Fair" = 3,
                           "Poor" = 4),
  ALCOHOL_BINGE2_ORD = recode(ALCOHOL_BINGE2,
                                  "0 days" = 0,
                                  "1-3 days" = 1,
                                  "4-9 days" = 2,
                                  "10 days or more" = 3))

saveRDS(nlsy_tv_long, "~/Documents/QuestionnaireCrosswalk/Data/nlsy_harmonized.RDS")
saveRDS(hrs_tv_long, "~/Documents/QuestionnaireCrosswalk/Data/hrs_harmonized.RDS")

