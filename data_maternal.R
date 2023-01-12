#**************************************************************************************
#*Maternal outcome
#*output: matOutcome.rda
#*includes: BESTEDD, GA_ENROLL_WKS, GA_ENROLL_DAYS
#**************************************************************************************

rm(list = ls())
library(tidyverse)
library(lubridate)

#**************************************************************************************
#*Maternal outcome
#*output: matOutcome.rda
#*includes: BESTEDD, GA_ENROLL_WKS, GA_ENROLL_DAYS, TRIMES_ENROLL (will add more later)
#**************************************************************************************
load("derived_data/matData.rda")

#Note: revise once we have India data since ACOG variable will be used for India
vars_bestedd <- matData %>%
  select(SCRNID, MOMID, PREGID, SITE, starts_with("M01_US_OHOSTDAT"), M01_KNOWN_LMP_SCORRES,
    M01_LMP_SCDAT, M01_GA_LMP_WEEKS_SCORRES, M01_GA_LMP_DAYS_SCORRES, M01_ESTIMATED_EDD_SCDAT,
    M01_US_GA_WKS_AGE_FTS1, M01_US_GA_DAYS_AGE_FTS1, M01_US_GA_WKS_AGE_FTS2, M01_US_GA_DAYS_AGE_FTS2,
    M01_US_GA_WKS_AGE_FTS3, M01_US_GA_DAYS_AGE_FTS3, M01_US_GA_WKS_AGE_FTS4, M01_US_GA_DAYS_AGE_FTS4,
    M01_US_EDD_BRTHDAT_FTS1, M01_US_EDD_BRTHDAT_FTS2, M01_US_EDD_BRTHDAT_FTS3, M01_US_EDD_BRTHDAT_FTS4,  
    M01_CAL_GA_WKS_AGE_FTS1, M01_CAL_GA_DAYS_AGE_FTS1, M01_CAL_GA_WKS_AGE_FTS2, M01_CAL_GA_DAYS_AGE_FTS2,
    M01_CAL_GA_WKS_AGE_FTS3, M01_CAL_GA_DAYS_AGE_FTS3, M01_CAL_GA_WKS_AGE_FTS4, M01_CAL_GA_DAYS_AGE_FTS4,
    M01_CAL_EDD_BRTHDAT_FTS1,  M01_CAL_EDD_BRTHDAT_FTS2,  M01_CAL_EDD_BRTHDAT_FTS3,  M01_CAL_EDD_BRTHDAT_FTS4,
    M02_SCRN_OBSSTDAT) %>%
rowwise() %>%
  mutate(#BESTEDD1 = dmy(M01_US_EDD_BRTHDAT_FTS1), #compare with BESTEDD once data fixed
    GAUSGSCRNDAYS = max((M01_US_GA_WKS_AGE_FTS1 * 7 + M01_US_GA_DAYS_AGE_FTS1),
                        (M01_US_GA_WKS_AGE_FTS2 * 7 + M01_US_GA_DAYS_AGE_FTS2),
                        (M01_US_GA_WKS_AGE_FTS3 * 7 + M01_US_GA_DAYS_AGE_FTS3),
                        (M01_US_GA_WKS_AGE_FTS4 * 7 + M01_US_GA_DAYS_AGE_FTS4)),
    BESTEDD = dmy(M01_US_OHOSTDAT_V1) + (280-GAUSGSCRNDAYS), 
         GA_ENROLL_WKS = (GAUSGSCRNDAYS + as.numeric(dmy(M02_SCRN_OBSSTDAT) - dmy(M01_US_OHOSTDAT_V1))) %/% 7,
         GA_ENROLL_DAYS = (GAUSGSCRNDAYS + as.numeric(dmy(M02_SCRN_OBSSTDAT) - dmy(M01_US_OHOSTDAT_V1))) %% 7,
         ) %>%
  mutate(TRIMES_ENROLL = case_when(
    GA_ENROLL_WKS > 3 & GA_ENROLL_WKS < 14 ~ 1, #first trimester
    GA_ENROLL_WKS >= 14 & GA_ENROLL_WKS < 20 ~ 2, #second trimester and eligible (14-27 wks = 2nd trimester)
    GA_ENROLL_WKS >= 20 ~ 3)) %>%  #ineligible 
  ungroup() %>% 
  select(SCRNID, MOMID, PREGID, SITE,  
         BESTEDD, BESTEDD_data, GA_ENROLL_WKS, GA_ENROLL_DAYS, TRIMES_ENROLL, 
         M01_US_EDD_BRTHDAT_FTS1, M01_US_GA_WKS_AGE_FTS1, M01_US_GA_DAYS_AGE_FTS1, matches("M01_US_OHOSTDAT_V"), M02_SCRN_OBSSTDAT)

#**************************************************************************************
#*MMerge Maternal Outcomes and save data
#**************************************************************************************
matOutcome <- vars_bestedd 

save(matOutcome, file = "derived_data/matOutcome.rda")