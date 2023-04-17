#**************************************************************************************
#*Monitoring report vars
#*Output: statusOutcome.rda
#*include: screen vars, enroll vars
#**************************************************************************************
# rm(list = ls())
library(tidyverse)
library(lubridate)
load("derived_data/dataMerged.rda")
load("derived_data/matData.rda")
load("derived_data/matOutcome.rda")

#**************************************************************************************
#*study date 
#**************************************************************************************
#study start date and cut date
study_date <- matData %>% 
    select(SCRNID,MOMID, PREGID, SITE,
           M00_SCRN_OBSSTDAT,
           M00_CON_DSSTDAT, # Date consent signed
           M00_CON_LAR_SIGNDAT, # Date that legal representative signed consent
           M00_CON_WITNESS_SIGNDAT, # Date that witness signed consent
           M00_ASSNT_DSSTDAT, # ASSNT Date
           M00_FORMCOMPLDAT_MNH00,
           M01_US_OHOSTDAT_V1,
           M01_FORMCOMPLDAT_MNH01_V1, M01_FORMCOMPLDAT_MNH01_V1,
           M02_SCRN_OBSSTDAT, 
           M02_FORMCOMPLDAT_MNH02
    ) %>% 
    rowwise() %>% 
    mutate(START_DATE_MOM = dmy(M02_SCRN_OBSSTDAT),
           START_DATE_MOM_MTH = floor_date(START_DATE_MOM, unit = "month"),
           START_DATE_MOM_WK = floor_date(START_DATE_MOM, unit = "week", week_start = getOption("lubridate.week.start",1)),
           START_DATE_MOM_DAY = floor_date(START_DATE_MOM, unit = "day")
           ) %>% 
    ungroup() %>% 
    group_by("SITE") %>% 
    mutate(START_DATE=min(START_DATE_MOM, na.rm = TRUE),
           START_DATE_MTH = floor_date(START_DATE, unit = "month"),
           START_DATE_WK = floor_date(START_DATE, unit = "week", week_start = getOption("lubridate.week.start",1))) %>% 
    mutate(CUT_DATE = max(dmy(M02_SCRN_OBSSTDAT), na.rm = TRUE),
           CUT_DATE_MTH = floor_date(CUT_DATE, unit = "month"),
           CUT_DATE_WK = floor_date(CUT_DATE, unit = "week", week_start = getOption("lubridate.week.start",1))) %>% 
    ungroup() %>% 
    select(SCRNID, MOMID, PREGID, SITE, START_DATE_MOM, START_DATE_MOM_MTH, START_DATE_MOM_WK, START_DATE_MOM_DAY,
           START_DATE, START_DATE_MTH, START_DATE_WK, CUT_DATE, CUT_DATE_MTH, CUT_DATE_WK, 
           M02_SCRN_OBSSTDAT, M00_SCRN_OBSSTDAT)

#**************************************************************************************
#*Pre-screening, screen and enrollment info
#**************************************************************************************
vars_enroll <- matData %>% 
  select(SCRNID, MOMID, PREGID, SITE,
         M00_SCRN_OBSSTDAT,
         M00_PREGNANT_IEORRES, M00_EGA_LT25_IEORRES, M00_AGE_IEORRES, 
         M00_CATCHMENT_IEORRES, M00_OTHR_IEORRES, 
         M00_CON_YN_DSDECOD, M00_CON_DSSTDAT,
         M00_CON_LAR_YN_DSDECOD, M00_CON_LAR_SIGNDAT,
         M00_CON_WITNESS_SIGNATURE_PRYN, M00_CON_WITNESS_SIGNDAT,
         M00_ASSNT_YN_DSDECOD, M00_ASSNT_DSSTDAT, M00_FORMCOMPLDAT_MNH00,
         M02_SCRN_OBSSTDAT, M02_AGE_IEORRES, M02_PC_IEORRES, 
         M02_CATCHMENT_IEORRES, M02_CATCH_REMAIN_IEORRES,
         M02_CONSENT_IEORRES, M02_FORMCOMPLDAT_MNH02) %>% 
  # four types of consent
  mutate(
    # participant consent (has both willingness & written consent)
    CONSENT_PART = case_when(
      M00_CON_YN_DSDECOD == 1 & !is.na(M00_CON_DSSTDAT) ~ 1,
      M00_CON_YN_DSDECOD == 0 & !is.na(M00_CON_DSSTDAT) ~ 0,
      TRUE ~ 77),
    # legal representative consent (has both willingness & written consent) *optional
    CONSENT_LAR = case_when(
      M00_CON_LAR_YN_DSDECOD == 1 & !is.na(M00_CON_LAR_SIGNDAT) ~ 1,  
      M00_CON_LAR_YN_DSDECOD == 0 & !is.na(M00_CON_LAR_SIGNDAT) ~ 0,
      TRUE ~ 77),
    # participant assent (has both willingness & written assent) *optional
    ASSNT_PART = case_when(
      M00_ASSNT_YN_DSDECOD == 1 & !is.na(M00_ASSNT_DSSTDAT) ~ 1,  
      M00_ASSNT_YN_DSDECOD == 0 & !is.na(M00_ASSNT_DSSTDAT) ~ 0,
      TRUE ~ 77),
    # witness consent (has written consent) *optional
    CONSENT_WITNESS = case_when(
      M00_CON_WITNESS_SIGNATURE_PRYN == 1 & !is.na(M00_CON_WITNESS_SIGNDAT) ~ 1,  
      M00_CON_WITNESS_SIGNATURE_PRYN == 0 & !is.na(M00_CON_WITNESS_SIGNDAT) ~ 0,
      TRUE ~ 77),
  ) %>% 
  mutate(
    # participant consent
    PRESCR_CONSENT = case_when(
      # ??? what's the consent criteria for Kenya/Ghana/Pakistan/Zambia/India ???
      CONSENT_PART == 0 | ASSNT_PART == 0 | CONSENT_LAR == 0 | CONSENT_WITNESS == 0 ~ 0,
      CONSENT_PART == 1 & (CONSENT_LAR == 1 | CONSENT_LAR == 77) & 
        (ASSNT_PART == 1 | ASSNT_PART == 77) & 
        (CONSENT_WITNESS == 1 | CONSENT_WITNESS == 77)  ~ 1,
      # DK
      TRUE ~ 99
    ),
    # screen status (M00 pre-screen is completed)
    PRESCREEN = case_when(
      !is.na(M00_SCRN_OBSSTDAT) & M00_SCRN_OBSSTDAT != "N/A" ~ 1, 
                       TRUE ~ 0
      ),
    #gap between no other reason to exclude and provide consent info
    GAP_CONSENT = ifelse(M00_OTHR_IEORRES == 0 & M00_CON_YN_DSDECOD == 77, 1, 0),
    # expected enrollment
    EXPECT = case_when(
      SITE == "Pakistan" | SITE == "India" ~ 6000/1095, 
      SITE == "Ghana" | SITE == "Kenya" ~ 3000/1095, 
      SITE == "Zambia" ~ 2250/1095) ,
    # eligible based on pre-screening
    PRESCR_ELIGIBLE = case_when(M00_PREGNANT_IEORRES == 1 & M00_EGA_LT25_IEORRES == 1 & M00_AGE_IEORRES == 1 &
                               M00_CATCHMENT_IEORRES == 1 & M00_OTHR_IEORRES == 0 & PRESCR_CONSENT == 1 ~ 1,
                             M00_PREGNANT_IEORRES == 0 | M00_EGA_LT25_IEORRES == 0 | M00_AGE_IEORRES == 0 |
                               M00_CATCHMENT_IEORRES == 0 | M00_OTHR_IEORRES == 1 | PRESCR_CONSENT == 0 ~ 0,
                             TRUE ~ 99),
    #screened (M02 enrollment)
    SCREEN = case_when(!is.na(M02_FORMCOMPLDAT_MNH02) ~ 1, 
                       TRUE ~ 0),
    # eligible 
    ELIGIBLE = case_when(M02_AGE_IEORRES == 1 & M02_PC_IEORRES == 1 & M02_CATCHMENT_IEORRES == 1 &
                           M02_CATCH_REMAIN_IEORRES == 1  ~ 1,
                         M02_AGE_IEORRES == 0 | M02_PC_IEORRES == 0 | M02_CATCHMENT_IEORRES == 0 |
                           M02_CATCH_REMAIN_IEORRES == 0  ~ 0,
                             TRUE ~ 99),
    CONSENT = case_when(!is.na(M02_CONSENT_IEORRES) ~ M02_CONSENT_IEORRES,
                        TRUE ~ 99),
    ENROLL = case_when(M02_CONSENT_IEORRES == 1 & !is.na(M02_FORMCOMPLDAT_MNH02) ~ 1, 
                       M02_CONSENT_IEORRES == 0 | M02_CONSENT_IEORRES == 77 ~ 0,
                       TRUE ~ 77)
  ) %>% 
  select(SCRNID, MOMID, PREGID, SITE, PRESCREEN, SCREEN, EXPECT, PRESCR_ELIGIBLE, 
         ELIGIBLE, PRESCR_CONSENT, CONSENT, ENROLL, GAP_CONSENT) 

#******Reasons for exclusion (check if cases match within mnh00 and 02)
vars_exrs <- matData %>% 
    left_join(vars_enroll,by = c("SCRNID", "MOMID", "PREGID", "SITE")) %>% 
    distinct() %>% 
  #reason for exclusion in pre-screen
  mutate(
    PRESCR_PREGSIGN = case_when(
      M00_PREGNANT_IEORRES == 1 ~ 1, 
      M00_PREGNANT_IEORRES == 0 ~ 0, 
    TRUE ~ 99),
    PRESCR_GA25 = case_when(
      M00_EGA_LT25_IEORRES == 1 ~ 1,
      M00_EGA_LT25_IEORRES == 0 ~ 0,
      M00_PREGNANT_IEORRES == 0 ~ 77,
      TRUE ~ 99),
    PRESCR_AGE = case_when(
      M00_AGE_IEORRES == 1 ~ 1,
      M00_AGE_IEORRES == 0 ~ 0,
      M00_PREGNANT_IEORRES == 0 | M00_EGA_LT25_IEORRES == 0 ~ 77,
      TRUE ~ 99),
    PRESCR_CATCHAREA = case_when(
      M00_CATCHMENT_IEORRES == 1 ~ 1,
      M00_CATCHMENT_IEORRES == 0 ~ 0,
      M00_PREGNANT_IEORRES == 0 | M00_EGA_LT25_IEORRES == 0 | M00_AGE_IEORRES == 0 ~ 77,
      TRUE ~ 99),
    PRESCR_OTHER = case_when(
      M00_OTHR_IEORRES== 0 ~ 1,
      M00_OTHR_IEORRES== 1 ~ 0,
      M00_PREGNANT_IEORRES == 0 | M00_EGA_LT25_IEORRES == 0 | M00_AGE_IEORRES == 0 | M00_CATCHMENT_IEORRES == 0~ 77,
      TRUE ~ 99)
) %>%
  #reason for exclusion in screen/enroll
  mutate(
    AGE = case_when(
      M02_AGE_IEORRES == 1 ~ 1,
      M02_AGE_IEORRES == 0 ~ 0,
      is.na(M02_FORMCOMPLDAT_MNH02) ~ 99,
      TRUE ~ 99),
    GA20 = case_when(
      M02_PC_IEORRES == 1 ~ 1,
      M02_PC_IEORRES == 0 ~ 0,
      M02_AGE_IEORRES == 0 ~ 77,
      TRUE ~ 99),
    CATCHAREA = case_when(
      M02_CATCHMENT_IEORRES == 1 & M02_CATCH_REMAIN_IEORRES == 1 ~ 1,
      M02_CATCHMENT_IEORRES == 0 | M02_CATCH_REMAIN_IEORRES == 0 ~ 0,
      M02_AGE_IEORRES == 0 | M02_PC_IEORRES ==0 ~ 77,
      TRUE ~ 99),
  ) %>% 
    select(SCRNID, MOMID, PREGID, SITE, AGE, GA20, CATCHAREA, PRESCR_PREGSIGN, PRESCR_GA25, PRESCR_AGE, PRESCR_CATCHAREA, PRESCR_OTHER)  


#**************************************************************************************
#*Merge and save data
#**************************************************************************************
statusOutcome <- study_date %>% 
  left_join(vars_enroll, by = c("SCRNID", "MOMID", "PREGID", "SITE")) %>% 
  left_join(vars_exrs, by = c("SCRNID", "MOMID", "PREGID", "SITE")) %>% 
  distinct() 

save(statusOutcome, file = "derived_data/statusOutcome.rda")
saveRDS(statusOutcome, file = "derived_data/statusOutcome.rds", version = 2)

load("derived_data/healthyOutcome.rda")
enrollOutcome <- healthyOutcome %>%
  left_join(statusOutcome, by = c("SCRNID", "MOMID", "PREGID", "SITE")) %>% 
  distinct()
save(enrollOutcome, file = "derived_data/enrollOutcome.rda")
saveRDS(enrollOutcome, file = "derived_data/enrollOutcome.rds")

