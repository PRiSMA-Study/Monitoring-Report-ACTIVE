#*****************************************************************************
#### MONITORING REPORT SETUP -- MATERNAL 
#* Function: Merge all forms together in wide format to create a dataset with one row for each woman for each visit 
#* Input: .RData files for each form (generated from 1. data import code)
#* Last updated: 30 March 2026 (cleaning up code)


#*Output:   
## MatData_Report: 
# input: MatData_wide 
# only includes a subset of variables from the maternal wide dataset to be used for report purposes
## InfData_Report: 
# input: InfData_wide 
# only includes a subset of variables from the infant wide dataset to be used for report purposes
## MatData_Screen_Enroll
# input: MatData_Report
# Includes all women screen and enrolled. Includes constructed variables on enrollment and screening status
## MatData_Anc_Visits
# input: MatData_Screen_Enroll
# Includes all women who are enrolled. Includes constructed variables relevant to ANC 
## MatData_Pnc_Visits
# input: MatData_Screen_Enroll
# Includes all women who are enrolled who have entered the PNC period. Includes constructed variables relevent to PNC
## MatData_Hb_Visit
# input: MatData_Anc_Visits
# Includes relevant constructed variables for ReMAPP tables looking at testing completion for Hb at each visit
## MatData_Hb_GA_Visit
# input: MatData_Anc_Visits
# Includes relevant constructed variables for ReMAPP tables looking at Hb test outcomes by gestational age
## healthyOutcome
# input: MatData_Anc_Visits
# Includes relevant constructed variables for ReMAPP healthy criteria 
#*****************************************************************************
# 1. Data Import ----
## load in data 
rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)

UploadDate = "2026-03-20"

load(paste0("~/Monitoring Report/data/cleaned/", UploadDate, "/", "MatData_Wide_", UploadDate, ".RData"))
load(paste0("~/Monitoring Report/data/cleaned/", UploadDate, "/", "InfData_Wide_", UploadDate, ".RData"))
load(paste0("~/Monitoring Report/data/cleaned/", UploadDate, "/", "MatData_Screened_", UploadDate, ".RData"))

# set path to save 
path_to_save <- "D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/"
setwd(paste0("D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/"))

#*****************************************************************************
#* Extract variables for monitoring report 
#*****************************************************************************
# 2. Extract variables required ----

#update/add/delete variable names in the varNames_sheet.xlsx (check if the var is multiple or singe when add)
MatNames_sheet <- read_excel("~/Monitoring Report/code/varNames_sheet.xlsx", sheet = "MaternalVars")
InfNames_sheet <- read_excel("~/Monitoring Report/code/varNames_sheet.xlsx", sheet = "InfantVars")

MatData_Report <- MatData_Wide %>% 
  select(matches(MatNames_sheet$varname), 
         US_GA_WKS_ENROLL, US_GA_DAYS_ENROLL,
         M02_SCRN_OBSSTDAT,EDD_US,BOE_GA_DAYS_ENROLL, 
         PREG_START_DATE,
         contains("_TYPE_VISIT_"), 
         contains("_VISIT_COMPLETE"), 
         contains("M04_ANC_OBSSTDAT_"),
         contains("M12_VISIT_OBSSTDAT_"),
         contains("GESTAGE_AT_VISIT_DAYS"),
         contains("GESTAGE_AT_VISIT_WKS"),
         contains("GESTAGE_AT_BIRTH_"), 
         contains("_PNC_AT_VISIT_"),
         contains("_GA_LMP_DAYS_SCORRES_"),
         contains("M04_FETAL_LOSS_DSSTDAT"),
         contains("M09_DELIV_DSSTDAT_INF"),
         contains("VISIT_DATE"),
         M00_KNOWN_DOBYN_SCORRES, DOB,
         contains("_FETUS_CT_PERES_US"),
         M02_SCRN_RETURN, contains("RBC_G6PD_LBORRES"), contains("M08_RBC_THALA"),
         contains("M08_RBC_SPFY"), contains("M08_RBC_SICKLE")
         ) 

## export 
save(MatData_Report, file= paste0(path_to_save, "MatData_Report",".RData",sep = ""))

InfData_Report <- InfData_Wide %>% 
  select(matches(InfNames_sheet$varname),
         M11_VISIT_COMPLETE_6,
         contains("_TYPE_VISIT_"), 
         contains("_VISIT_COMPLETE"),
         contains("_PNC_AT_VISIT_DAYS"),
         contains("_PNC_AT_VISIT_WKS")) %>% 
  mutate(DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")) 

## export 
save(InfData_Report, file= paste0(path_to_save, "InfData_Report",".RData",sep = ""))

mat_enroll <- read_excel(paste0("Z:/Outcome Data/", UploadDate, "/MAT_ENROLL.xlsx")) %>%
  select(SITE, SCRNID, MOMID, PREGID, ENROLL, PREG_START_DATE,GA_DIFF_DAYS,
         EDD_BOE,BOE_METHOD, BOE_GA_WKS_ENROLL, BOE_GA_DAYS_ENROLL, REMAPP_ENROLL, ENROLL_SCRN_DATE)


mat_enroll <- mat_enroll %>% 
  select(SITE, SCRNID, MOMID, PREGID, ENROLL, PREG_START_DATE,GA_DIFF_DAYS,
         EDD_BOE,BOE_METHOD, BOE_GA_WKS_ENROLL, BOE_GA_DAYS_ENROLL, REMAPP_ENROLL, ENROLL_SCRN_DATE)

table(mat_enroll$SITE)
dim(mat_enroll)

mnh04  <- read.csv(paste0("D:/Users/stacie.loisate/Documents/import/", UploadDate, "/mnh04_merged.csv")) %>% 
  select(SITE, MOMID, PREGID, M04_TYPE_VISIT, contains("FETAL_LOSS")) # %>% filter(M04_TYPE_VISIT %in% c(13,14))
#**************************************************************************************
#* PRISMA Tables 1-4
#* Table #: (not currently in use) Pre-screening and enrollment numbers for PRISMA MNH Study for the most recent one week
#* Table #: Cumulative pre-screening numbers for PRISMA MNH Study
#* Table #: Cumulative enrollment numbers for PRISMA MNH Study 
#* Table #: Study Status for PRISMA MNH Study 

#* Pre-screening, screen and enrollment info
#* Output = MatData_Screen_Enroll (tables mentioned above will use MatData_Screen_Enroll as the input)
#**************************************************************************************
# 3. Generate screen/enroll data (MatData_Screen_Enroll) ----

## 3.1. import ghana and zambia raw data to adjust screening dates ----
mnh02_gha <- read.csv(paste0("~/import/", UploadDate, "_gha/mnh02.csv")) 
mnh02_zam <- read.csv(paste0("~/import/", UploadDate, "_zam/mnh02.csv")) 


colnames(mnh02_gha) = toupper(colnames(mnh02_gha))
mnh02_gha_out <- mnh02_gha %>% 
  mutate(NEW_M02_FORMCOMPLDAT_MNH02 = dmy(FORMCOMPLDAT_MNH02),
         NEW_M02_SCRN_OBSSTDAT = dmy(SCRN_OBSSTDAT)
         ) %>% 
  mutate(SITE = "Ghana") %>% 
  select(SITE, SCRNID,NEW_M02_SCRN_OBSSTDAT, NEW_M02_FORMCOMPLDAT_MNH02,
         AGE_IEORRES, PC_IEORRES, CATCHMENT_IEORRES, 
         CATCH_REMAIN_IEORRES, CONSENT_IEORRES, SCRN_RETURN)

colnames(mnh02_zam) = toupper(colnames(mnh02_zam))
mnh02_zam_out <- mnh02_zam %>%
  mutate(NEW_M02_FORMCOMPLDAT_MNH02 = dmy(SCRN_OBSSTDAT),
         NEW_M02_SCRN_OBSSTDAT = dmy(SCRN_OBSSTDAT)
  ) %>%
  mutate(SITE = "Zambia") %>%
  select(SITE, SCRNID, NEW_M02_SCRN_OBSSTDAT, NEW_M02_FORMCOMPLDAT_MNH02,
         AGE_IEORRES, PC_IEORRES, CATCHMENT_IEORRES,
         CATCH_REMAIN_IEORRES, CONSENT_IEORRES, SCRN_RETURN)
table(mnh02_zam_out$NEW_M02_SCRN_OBSSTDAT, useNA = "ifany")

gc()

MatData_Screened_NoDup <- MatData_Screened %>% 
    group_by(SITE, SCRNID) %>%
    arrange(-desc(M00_SCRN_OBSSTDAT)) %>%
    slice(1) %>%
    mutate(n=n()) %>%
    ungroup() %>%
    select(-n)

dup_test <- MatData_Screened %>% select(SITE, SCRNID, MOMID, PREGID, M00_SCRN_OBSSTDAT) %>%  group_by(SITE, SCRNID) %>% mutate(n=n()) %>% filter(n>1)
table(MatData_Screened$SITE)
table(MatData_Screened_NoDup$SITE)
length(unique(dup_test$SCRNID))                           ## how many unique scrnids in the original dataset
dim(MatData_Screened)[1] - dim(MatData_Screened_NoDup)[1] ## how many datapoints were remove 

## 3.2 generate study start date and cut date ----
MatData_Screen_Enroll <-  MatData_Screened_NoDup %>% # MatData_Report
  mutate(MOMID = ifelse(SITE == "Zambia" & !str_detect(MOMID, "Z"), NA, MOMID),
         PREGID = ifelse(SITE == "Zambia" & !str_detect(PREGID, "Z"), NA, PREGID)
  ) %>% 
  ## 4/25 updates for ghana
  full_join(mnh02_gha_out, by = c("SITE", "SCRNID")) %>%
  mutate(M02_SCRN_OBSSTDAT = case_when(SITE == "Ghana" ~ NEW_M02_SCRN_OBSSTDAT, TRUE ~ M02_SCRN_OBSSTDAT),
         M02_FORMCOMPLDAT_MNH02 = case_when(SITE == "Ghana" ~ NEW_M02_FORMCOMPLDAT_MNH02, TRUE ~ M02_FORMCOMPLDAT_MNH02),
         M02_AGE_IEORRES = case_when(SITE == "Ghana" ~ AGE_IEORRES, TRUE ~ M02_AGE_IEORRES),
         M02_PC_IEORRES = case_when(SITE == "Ghana" ~ PC_IEORRES, TRUE ~ M02_PC_IEORRES),
         M02_CATCHMENT_IEORRES= case_when(SITE == "Ghana" ~ CATCHMENT_IEORRES, TRUE ~ M02_CATCHMENT_IEORRES),
         M02_CATCH_REMAIN_IEORRES = case_when(SITE == "Ghana" ~ CATCH_REMAIN_IEORRES, TRUE ~ M02_CATCH_REMAIN_IEORRES),
         M02_CONSENT_IEORRES = case_when(SITE == "Ghana" ~ CONSENT_IEORRES, TRUE ~ M02_CONSENT_IEORRES),
         M02_SCRN_RETURN = case_when(SITE == "Ghana" ~ SCRN_RETURN, TRUE ~ M02_SCRN_RETURN)
         ) %>%
  # select(-NEW_M02_SCRN_OBSSTDAT) %>%
  select(-NEW_M02_SCRN_OBSSTDAT, -NEW_M02_FORMCOMPLDAT_MNH02,
         -AGE_IEORRES, -PC_IEORRES, -CATCHMENT_IEORRES,
         -CATCH_REMAIN_IEORRES, -CONSENT_IEORRES, -SCRN_RETURN) %>%
  # 4/25 updates for Zambia
  full_join(mnh02_zam_out, by = c("SITE", "SCRNID")) %>%
  mutate(M02_SCRN_OBSSTDAT = case_when(SITE == "Zambia" ~ NEW_M02_SCRN_OBSSTDAT, TRUE ~ M02_SCRN_OBSSTDAT),
         M02_FORMCOMPLDAT_MNH02 = case_when(SITE == "Zambia" ~ NEW_M02_FORMCOMPLDAT_MNH02, TRUE ~ M02_FORMCOMPLDAT_MNH02),
         M02_AGE_IEORRES = case_when(SITE == "Zambia" ~ AGE_IEORRES, TRUE ~ M02_AGE_IEORRES),
         M02_PC_IEORRES = case_when(SITE == "Zambia" ~ PC_IEORRES, TRUE ~ M02_PC_IEORRES),
         M02_CATCHMENT_IEORRES= case_when(SITE == "Zambia" ~ CATCHMENT_IEORRES, TRUE ~ M02_CATCHMENT_IEORRES),
         M02_CATCH_REMAIN_IEORRES = case_when(SITE == "Zambia" ~ CATCH_REMAIN_IEORRES, TRUE ~ M02_CATCH_REMAIN_IEORRES),
         M02_CONSENT_IEORRES = case_when(SITE == "Zambia" ~ CONSENT_IEORRES, TRUE ~ M02_CONSENT_IEORRES),
         M02_SCRN_RETURN = case_when(SITE == "Zambia" ~ SCRN_RETURN, TRUE ~ M02_SCRN_RETURN)
  ) %>%
  # select(SITE, SCRNID, M02_SCRN_OBSSTDAT, M02_FORMCOMPLDAT_MNH02) %>% filter(SCRNID %in% test$SCRNID)
  mutate(START_DATE_MOM = as.Date(M02_SCRN_OBSSTDAT, format = "%Y-%m-%d"),
         START_DATE_MOM_MTH = floor_date(START_DATE_MOM, unit = "month"),
         START_DATE_MOM_WK = floor_date(START_DATE_MOM, unit = "week", week_start = getOption("lubridate.week.start",1)),
         START_DATE_MOM_DAY = floor_date(START_DATE_MOM, unit = "day")
  ) %>% 
  ungroup() %>% 
  mutate(START_DATE=min(START_DATE_MOM, na.rm = TRUE),
         START_DATE_MTH = floor_date(START_DATE, unit = "month"),
         START_DATE_WK = floor_date(START_DATE, unit = "week", week_start = getOption("lubridate.week.start",1))) %>% 
  mutate(CUT_DATE = max(as.Date(M02_SCRN_OBSSTDAT, format = "%Y-%m-%d"), na.rm = TRUE),
         CUT_DATE_MTH = floor_date(CUT_DATE, unit = "month"),
         CUT_DATE_WK = floor_date(CUT_DATE, unit = "week", week_start = getOption("lubridate.week.start",1))) %>% 
  ungroup()  %>% 
  select(-NEW_M02_SCRN_OBSSTDAT, -NEW_M02_FORMCOMPLDAT_MNH02,
         -AGE_IEORRES, -PC_IEORRES, -CATCHMENT_IEORRES, 
         -CATCH_REMAIN_IEORRES, -CONSENT_IEORRES, -SCRN_RETURN) 

gc()

## 3.3 code in enrollment criteria ----
MatData_Screen_Enroll <- MatData_Screen_Enroll %>% 
  full_join(mat_enroll[c("SITE","SCRNID", "MOMID", "PREGID", "ENROLL")],
            by = c("SITE","SCRNID", "MOMID", "PREGID")) %>% 
  ## PRESCREENING CRITERIA
  # four types of consent
  mutate(
    # screen status (M00 pre-screen is completed)
    PRESCREEN = case_when(
      !is.na(M00_SCRN_OBSSTDAT) ~ 1,
      TRUE ~ 0
    ),
    #gap between no other reason to exclude and provide consent info
    GAP_CONSENT = ifelse(M00_OTHR_IEORRES == 0 & M00_CON_YN_DSDECOD == 77, 1, 0),
    # expected enrollment
    EXPECT = case_when(
      SITE == "Pakistan" ~ 6000/1095,
      SITE == "India-CMC" ~ 3300/1095,
      SITE == "India-SAS" ~ 2000/365,
      SITE == "Ghana" | SITE == "Kenya" ~ 3000/1095, 
      SITE == "Zambia" ~ 2250/1095) ,
    # binary variable indicating if a woman was screened 
    SCREEN = case_when(!is.na(M02_FORMCOMPLDAT_MNH02) ~ 1, 
                       TRUE ~ 0),
  ## SCREENING CRITERIA
    ## eligible & enrolled
    ## M02_AGE_IEORRES = meet age requirement?
    ## M02_PC_IEORRES = <20wks gestation?
    ## M02_CATCHMENT_IEORRES = live in catchment area?
    ## M02_CATCH_REMAIN_IEORRES = stay in catchment area?
    ELIGIBLE = case_when(M02_AGE_IEORRES == 1 & M02_PC_IEORRES == 1 & M02_CATCHMENT_IEORRES == 1 &
                           M02_CATCH_REMAIN_IEORRES == 1  ~ 1,
                           # (M02_SCRN_RETURN != 0 | is.na(M02_SCRN_RETURN)) 
                         M02_AGE_IEORRES == 0 | M02_PC_IEORRES == 0 | M02_CATCHMENT_IEORRES == 0 |
                           M02_CATCH_REMAIN_IEORRES == 0  ~ 0,
                         TRUE ~ 99),
    CONSENT = case_when(!is.na(M02_CONSENT_IEORRES) ~ M02_CONSENT_IEORRES,
                        TRUE ~ 99)) %>% 
  ## Assign denominators
  mutate(PRESCREEN_DENOM = case_when(PRESCREEN == 1 ~ 1,
                                     TRUE ~ 0)) %>%      
  mutate(SCREEN_DENOM = case_when(SCREEN == 1 ~ 1,
                                  TRUE ~ 0)) %>%     
  mutate(ENROLL_DENOM = case_when(ENROLL == 1 ~ 1,
                                  TRUE ~ 0)) %>% 
  # collect data on re-enrollment for pakistan 
  group_by(SITE, MOMID) %>% 
  mutate(n=n()) %>% 
  mutate(ENROLL_FREQ = case_when(SITE == "Pakistan" & n>1 ~ 1, TRUE ~ 0))


table(MatData_Screen_Enroll$ENROLL, MatData_Screen_Enroll$SITE)
table(MatData_Screen_Enroll$ENROLL_DENOM, MatData_Screen_Enroll$SITE)

## 3.4 merge in fetal loss date to calculate pregnancy end date ----
fetal_loss <- mnh04 %>% 
  # mutate(M04_FETAL_LOSS_DSSTDAT = parse_date_time(M04_FETAL_LOSS_DSSTDAT, order = c("%m/%d/%Y"))) %>% 
  mutate(M04_FETAL_LOSS_DSSTDAT = ymd(M04_FETAL_LOSS_DSSTDAT)) %>% 
  # filter(M04_TYPE_VISIT%in% c(13, 14)) %>% 
  mutate(M04_FETAL_LOSS_DSSTDAT = replace(M04_FETAL_LOSS_DSSTDAT, M04_FETAL_LOSS_DSSTDAT %in% c(ymd("1907-07-07"), ymd("1905-05-05")), NA)) %>% 
  select(SITE, MOMID, PREGID, M04_FETAL_LOSS_DSSTDAT, contains("M04_FETAL_LOSS_DSDECOD")) %>% 
  filter(!is.na(M04_FETAL_LOSS_DSSTDAT)) %>% 
  mutate(M04_FETAL_LOSS_DSSTDAT = ymd(M04_FETAL_LOSS_DSSTDAT)) %>% 
  group_by(SITE, MOMID, PREGID) %>%
  arrange(-desc(M04_FETAL_LOSS_DSSTDAT)) %>%
  slice(1) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  select(-n)

## the follow two lines should be the same number; if not, there are duplicates in fetal_loss datasets
length(unique(fetal_loss$PREGID))
dim(fetal_loss)[1]

## 3.5 extract Reasons for exclusion ---- 
  # note: (check if cases match within mnh00 and 02)
MatData_Screen_Enroll <- MatData_Screen_Enroll %>% 
  #reason for exclusion in pre-screen
  mutate(
    PRESCR_PREGSIGN = ifelse(M00_PREGNANT_IEORRES == 1, 1,
                             ifelse(M00_PREGNANT_IEORRES == 0, 0, 99)),
    
    ## if you answer 1 to PRESCR_PREGSIGN, you answer PRESCR_GA25
    PRESCR_GA25 = ifelse(M00_EGA_LT25_IEORRES == 1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan"), 1,
                         ifelse(M00_EGA_LT25_IEORRES == 0 & M00_PREGNANT_IEORRES == 1, 0, 99)),
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25, you answer PRESCR_AGE
    PRESCR_AGE = ifelse(M00_AGE_IEORRES == 1, 1,
                      ifelse(M00_AGE_IEORRES == 0  & M00_PREGNANT_IEORRES == 1 & 
                            (M00_EGA_LT25_IEORRES==1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan")), 0, 99)),
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25 & PRESCR_AGE, you answer PRESCR_CATCHAREA
    PRESCR_CATCHAREA = ifelse(M00_CATCHMENT_IEORRES == 1, 1,
                              ifelse(M00_CATCHMENT_IEORRES == 0 & 
                                       M00_AGE_IEORRES == 1  & 
                                       M00_PREGNANT_IEORRES == 1 & 
                                       (M00_EGA_LT25_IEORRES==1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan")), 0, 99)),
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25 & PRESCR_AGE & PRESCR_CATCHAREA, you answer PRESCR_OTHER
    PRESCR_OTHER = ifelse(M00_OTHR_IEORRES == 0 | (M00_OTHR_IEORRES==77 & SITE == "Pakistan"), 1,
                          ifelse(M00_OTHR_IEORRES == 1 & 
                                   M00_CATCHMENT_IEORRES == 1 & 
                                   M00_AGE_IEORRES == 1  & 
                                   M00_PREGNANT_IEORRES == 1 & 
                                   (M00_EGA_LT25_IEORRES==1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan")), 0, 99)), 
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25 & PRESCR_AGE & PRESCR_CATCHAREA & PRESCR_OTHER, you answer CONSENT_PRESCREEN
    CONSENT_PRESCREEN = ifelse(M00_CON_YN_DSDECOD == 1 | M00_ASSNT_YN_DSDECOD == 1| 
                                 M00_CON_LAR_YN_DSDECOD == 1, 1, 
                               ifelse((M00_CON_YN_DSDECOD == 0 | 
                                         M00_ASSNT_YN_DSDECOD == 0| M00_ASSNT_YN_DSDECOD == 0) & ## consent has "or" statements. if one method did not consent, then the rest are "77"
                                        (M00_CON_LAR_YN_DSDECOD == 0 | M00_CON_LAR_YN_DSDECOD == 77) & 
                                       (M00_OTHR_IEORRES == 0 | (M00_OTHR_IEORRES==77 & SITE == "Pakistan")) & 
                                        M00_CATCHMENT_IEORRES == 1 & 
                                        M00_AGE_IEORRES == 1  & 
                                        M00_PREGNANT_IEORRES == 1 & 
                                        (M00_EGA_LT25_IEORRES==1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan")), 0, 99))) %>% 
  # eligible based on pre-screening
  mutate(PRESCR_ELIGIBLE = ifelse(M00_PREGNANT_IEORRES == 1 & 
                                   (M00_EGA_LT25_IEORRES == 1 | (M00_EGA_LT25_IEORRES==77 & SITE == "Pakistan")) & 
                                    M00_AGE_IEORRES == 1 &
                                    M00_CATCHMENT_IEORRES == 1 & 
                                    (M00_OTHR_IEORRES == 0 | (M00_OTHR_IEORRES==77 & SITE == "Pakistan")) & 
                                    CONSENT_PRESCREEN == 1, 1,
                                  ifelse(M00_PREGNANT_IEORRES == 0 | M00_EGA_LT25_IEORRES == 0 | M00_AGE_IEORRES == 0 |
                                           M00_CATCHMENT_IEORRES == 0 | M00_OTHR_IEORRES == 1 | CONSENT_PRESCREEN == 0, 0, 99)
  )) %>% 
  
  #reason for exclusion in screen/enroll
  mutate(
    SCRN_RETURN = case_when(M02_SCRN_RETURN == 0 ~ 0,  ##  | M02_SCRN_RETURN ==77 need to decide what to do with 77 
                            TRUE ~ 1), ## all other instances included NA for sites not using this variable will be considered "scrn_return = yes"
    AGE = ifelse(M02_AGE_IEORRES == 1, 1,
                 ifelse(M02_AGE_IEORRES == 0, 0, 99)),
    GA20 = ifelse(M02_PC_IEORRES == 1, 1,
                  ifelse(M02_PC_IEORRES == 0 & 
                           M02_AGE_IEORRES == 1 & 
                           (M02_SCRN_RETURN != 0 | is.na(M02_SCRN_RETURN)) , 0, 99)),
    
    CATCHAREA = ifelse(M02_CATCHMENT_IEORRES == 1, 1,
                       ifelse(M02_CATCHMENT_IEORRES == 0 &
                                M02_PC_IEORRES == 1 & 
                                M02_AGE_IEORRES == 1 & 
                                (M02_SCRN_RETURN != 0 | is.na(M02_SCRN_RETURN)), 0, 99)),
    CATCHREMAIN = ifelse(M02_CATCH_REMAIN_IEORRES == 1, 1, 
                         ifelse(M02_CATCH_REMAIN_IEORRES == 0 & 
                                  M02_CATCHMENT_IEORRES == 1 & 
                                  M02_PC_IEORRES == 1 & 
                                  M02_AGE_IEORRES == 1 & 
                                  (M02_SCRN_RETURN != 0 | is.na(M02_SCRN_RETURN)), 0, 99)), 
    SCRN_CONSENT = ifelse(M02_CONSENT_IEORRES == 1, 1, 
                          ifelse(M02_CONSENT_IEORRES == 0 & 
                                   M02_CATCH_REMAIN_IEORRES == 1 & 
                                   M02_CATCHMENT_IEORRES == 1 &
                                   M02_PC_IEORRES == 1 & 
                                   M02_AGE_IEORRES == 1 & 
                                   (M02_SCRN_RETURN != 0 | is.na(M02_SCRN_RETURN)), 0, 99))) %>% 
  ## generate variable with age at pregnancy end
  # first, pull all instances of miscarriage in MNH04 
  left_join(fetal_loss, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(MatData_Wide %>% select(SITE, MOMID, PREGID, PREG_START_DATE,DOB, GESTAGE_AT_BIRTH_DAYS), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(MISCARRIAGE = ifelse(M04_FETAL_LOSS_DSDECOD== 1, 1, 0)) %>% 
  # calculate preg end in days (MISCARRIAGE DATE-PREG_START_DATE)
  mutate(PREG_START_DATE = ymd(PREG_START_DATE)) %>% 
  mutate(ENDPREG_DAYS = ifelse(is.na(MISCARRIAGE), GESTAGE_AT_BIRTH_DAYS, 
                               ifelse(MISCARRIAGE==1, M04_FETAL_LOSS_DSSTDAT-PREG_START_DATE, GESTAGE_AT_BIRTH_DAYS))) %>%   # if miscarriage=1, use MISCARRIAGE_DATE as DOB 
  mutate(DOB = ifelse(is.na(MISCARRIAGE), as.character(DOB),
                      ifelse(MISCARRIAGE==1, as.character(M04_FETAL_LOSS_DSSTDAT), 
                             as.character(DOB))) )%>% 
  # convert DOB to date class 
  mutate(DOB = as.Date(DOB, format = "%Y-%m-%d"))

gc()

## 3.6 merge in closeout form ----
## Has woman closed out? 
MatData_Screen_Enroll <- MatData_Screen_Enroll %>% 
  left_join(MatData_Wide %>% select(SITE, MOMID, PREGID, M23_CLOSE_DSDECOD), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(M23_VISIT_COMPLETE = ifelse(M23_CLOSE_DSDECOD == 1 | M23_CLOSE_DSDECOD == 2 | M23_CLOSE_DSDECOD == 3 |
                                       M23_CLOSE_DSDECOD == 4 | M23_CLOSE_DSDECOD == 5 | M23_CLOSE_DSDECOD == 9,1,0)) %>% 
  ## CLOSEOUT YES/NO 
  mutate(MATERNAL_CLOSEOUT_YN = ifelse(M23_CLOSE_DSDECOD == 1 | M23_CLOSE_DSDECOD == 2 | 
                                         M23_CLOSE_DSDECOD == 3 | M23_CLOSE_DSDECOD == 4 | 
                                         M23_CLOSE_DSDECOD == 5 | M23_CLOSE_DSDECOD == 6, 1, 0)) 

MatData_Screen_Enroll <- MatData_Screen_Enroll %>% distinct(SITE,SCRNID, MOMID, PREGID, .keep_all = TRUE) 

length(unique(MatData_Screen_Enroll$SCRNID)) 
dim(MatData_Screen_Enroll[1])

## 3.7 check for duplicates ----
duplicates <- MatData_Screen_Enroll %>%
  select(SITE, SCRNID, MOMID, PREGID, ENROLL) %>%
  group_by(SITE, SCRNID) %>%
  mutate(n=n()) %>%
  filter(n>1)


MatData_Screen_Enroll <- MatData_Screen_Enroll %>%
  mutate(KEEP = case_when(MOMID %in% duplicates$MOMID & !is.na(PREGID) ~ 0, TRUE ~ 1)) %>% 
  filter(KEEP == 1) %>% 
  select(-KEEP)

## check PNC data wide 
## 3.8 export MatData_Screen_Enroll ----
save(MatData_Screen_Enroll, file= paste0(path_to_save, "MatData_Screen_Enroll",".RData",sep = ""))
gc()

#**************************************************************************************
### MatData_Anc_Visits
# input: MatData_Screen_Enroll
# Includes all women who are enrolled. Includes constructed variables relevant to ANC 

# Table #: ANC and PNC visit completion for PRISMA
# Table #: ANC protocol compliance for PRISMA (only for the first summary row. Prot_Compliance_Anc used for the rest of the table)
# output: MatData_Anc_Visits (tables mentioned above will use MatData_Anc_Visits as the input [for the ANC portion only])
#**************************************************************************************
# 4. ANC visit data (MatData_Anc_Visits) ----

MatData_Report_out <- MatData_Report %>%
  left_join(MatData_Screen_Enroll %>% select(SITE, MOMID, PREGID,ENDPREG_DAYS,MISCARRIAGE,SCREEN, ENROLL, ENROLL_DENOM), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(PREG_START_DATE  = ymd(PREG_START_DATE)) 

## 4.1 calculate visit windows ----
MatData_Anc_Visits <- MatData_Report_out %>% 
  filter(ENROLL == 1) %>% 
  ## CALCULATE ON TIME AND LATE ANC WINDOWS
  mutate(ENROLL_ONTIME = (EDD_US - as.difftime(280, unit="days")) + as.difftime(139, unit="days"),
         ENROLL_LATE = (EDD_US - as.difftime(280, unit="days")) + as.difftime(139, unit="days"),
         ANC20_ONTIME = (EDD_US - as.difftime(280, unit="days")) + as.difftime(160, unit="days"),
         ANC20_LATE = (EDD_US - as.difftime(280, unit="days")) + as.difftime(181, unit="days"),
         ANC28_ONTIME = (EDD_US - as.difftime(280, unit="days")) + as.difftime(216, unit="days"),
         ANC28_LATE = (EDD_US - as.difftime(280, unit="days")) + as.difftime(216, unit="days"),
         ANC32_ONTIME = (EDD_US - as.difftime(280, unit="days")) + as.difftime(237, unit="days"),
         ANC32_LATE = (EDD_US - as.difftime(280, unit="days")) + as.difftime(237, unit="days"), 
         ANC36_ONTIME = (EDD_US - as.difftime(280, unit="days")) + as.difftime(272, unit="days"),
         ANC36_LATE = (EDD_US - as.difftime(280, unit="days")) + as.difftime(272, unit="days")) %>%
  ## CALCULATE INDICATOR VARIABLES for passed ON-TIME ANC WINDOWS - same as overdue code, but exclude the visit completion piece
  ## using upload date
  mutate(ENROLL_PASS = ifelse(ENROLL_ONTIME<UploadDate, 1, 0),
         ANC20_PASS = ifelse(ANC20_ONTIME<UploadDate, 1, 0),
         ANC28_PASS = ifelse(ANC28_ONTIME<UploadDate, 1, 0),
         ANC32_PASS = ifelse(ANC32_ONTIME<UploadDate, 1, 0),
         ANC36_PASS = ifelse(ANC36_ONTIME<UploadDate, 1, 0)) %>% 
  ## CALCULATE INDICATOR VARIABLES for missed PASSED LATE WINDOWS
  mutate(ENROLL_PASS_LATE = ifelse(ENROLL_LATE<UploadDate, 1, 0),
         ANC20_PASS_LATE = ifelse(ANC20_LATE<UploadDate & US_GA_WKS_ENROLL <= 17, 1, 0),
         ANC28_PASS_LATE = ifelse(ANC28_LATE<UploadDate, 1, 0),
         ANC32_PASS_LATE = ifelse(ANC32_LATE<UploadDate, 1, 0),
         ANC36_PASS_LATE = ifelse(ANC36_LATE<UploadDate, 1, 0)) %>% 
  ## CALCULATE INDICATOR VARIABLES for missed ANC visits ON-TIME WINDOWS
  ## max date has to be less than the upload date
  mutate(ENROLL_OVERDUE = ifelse(UploadDate>ENROLL_LATE & is.na(M04_VISIT_COMPLETE_1), 1, 0),
         ANC20_OVERDUE = ifelse(UploadDate>ANC20_LATE & is.na(M04_VISIT_COMPLETE_2) & US_GA_WKS_ENROLL <=17, 1, 0),
         ANC28_OVERDUE = ifelse(UploadDate>ANC28_LATE & is.na(M04_VISIT_COMPLETE_3), 1, 0),
         ANC32_OVERDUE = ifelse(UploadDate>ANC32_LATE & is.na(M04_VISIT_COMPLETE_4), 1, 0),
         ANC36_OVERDUE = ifelse(UploadDate>ANC36_LATE & is.na(M04_VISIT_COMPLETE_5), 1, 0)) %>%
  ## CALCULATE INDICATOR VARIALBE FOR ANY VISIT TYPE = i
     ## 4.2 calculate visit complete windows ----
  mutate(ANY_TYPE_VISIT_COMPLETE_1 = ifelse((M01_TYPE_VISIT_1 == 1 & M01_VISIT_COMPLETE_1 == 1) | 
                                              (M04_TYPE_VISIT_1 == 1 & M04_VISIT_COMPLETE_1 == 1) |
                                              (M05_TYPE_VISIT_1 == 1 & M05_VISIT_COMPLETE_1 == 1) |
                                              (M06_TYPE_VISIT_1 == 1 & M06_VISIT_COMPLETE_1 == 1) |
                                              (M07_TYPE_VISIT_1 == 1 & M07_VISIT_COMPLETE_1 == 1) |
                                              (M08_TYPE_VISIT_1 == 1 & M08_VISIT_COMPLETE_1 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_2 = ifelse((M04_TYPE_VISIT_2 == 2 & M04_VISIT_COMPLETE_2 == 1 & US_GA_WKS_ENROLL <= 17) |
                                              (M05_TYPE_VISIT_2 == 2 & M05_VISIT_COMPLETE_2 == 1 & US_GA_WKS_ENROLL <= 17) |
                                              (M06_TYPE_VISIT_2 == 2 & M06_VISIT_COMPLETE_2 == 1 & US_GA_WKS_ENROLL <= 17) |
                                              (M07_TYPE_VISIT_2 == 2 & M07_VISIT_COMPLETE_2 == 1 & US_GA_WKS_ENROLL <= 17) |
                                              (M08_TYPE_VISIT_2 == 2 & M08_VISIT_COMPLETE_2 == 1 & US_GA_WKS_ENROLL <= 17) | 
                                              (M25_TYPE_VISIT_2 == 2 & M25_VISIT_COMPLETE_2 == 1 & US_GA_WKS_ENROLL <= 17) | 
                                              (M26_TYPE_VISIT_2 == 2 & M26_VISIT_COMPLETE_2 == 1 & US_GA_WKS_ENROLL <= 17), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_3 = ifelse((M04_TYPE_VISIT_3 == 3 & M04_VISIT_COMPLETE_3 == 1) |
                                              (M05_TYPE_VISIT_3 == 3 & M05_VISIT_COMPLETE_3 == 1) |
                                              (M06_TYPE_VISIT_3 == 3 & M06_VISIT_COMPLETE_3 == 1) |
                                              (M07_TYPE_VISIT_3 == 3 & M07_VISIT_COMPLETE_3 == 1) |
                                              (M08_TYPE_VISIT_3 == 3 & M08_VISIT_COMPLETE_3 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_4 = ifelse((M01_TYPE_VISIT_4 == 4 & M01_VISIT_COMPLETE_4 == 1) | 
                                              (M04_TYPE_VISIT_4 == 4 & M04_VISIT_COMPLETE_4 == 1) |
                                              (M05_TYPE_VISIT_4 == 4 & M05_VISIT_COMPLETE_4 == 1) |
                                              (M06_TYPE_VISIT_4 == 4 & M06_VISIT_COMPLETE_4 == 1) |
                                              (M07_TYPE_VISIT_4 == 4 & M07_VISIT_COMPLETE_4 == 1) |
                                              (M08_TYPE_VISIT_4 == 4 & M08_VISIT_COMPLETE_4 == 1) | 
                                              (M25_TYPE_VISIT_4 == 4 & M25_VISIT_COMPLETE_4 == 1) | 
                                              (M26_TYPE_VISIT_4 == 4 & M26_VISIT_COMPLETE_4 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_5 = ifelse((M04_TYPE_VISIT_5 == 5 & M04_VISIT_COMPLETE_5 == 1) |
                                              (M05_TYPE_VISIT_5 == 5 & M05_VISIT_COMPLETE_5 == 1) |
                                              (M06_TYPE_VISIT_5 == 5 & M06_VISIT_COMPLETE_5 == 1) |
                                              (M07_TYPE_VISIT_5 == 5 & M07_VISIT_COMPLETE_5 == 1) |
                                              (M08_TYPE_VISIT_5 == 5 & M08_VISIT_COMPLETE_5 == 1), 1, 0)) %>% 
  ## 4.3 generate indicator variable for censoring ----
  ## 1. generate indicator variable if closeout was during ANC or PNC period 
  mutate(M23_CLOSEOUT_PERIOD = ifelse(!(is.na(DOB)), "PNC", "ANC")) %>% 
  ## 2. calculate GA window at closeout
  mutate(M23_AGE_VISIT_DAYS = as.numeric(ymd(M23_CLOSE_DSSTDAT)-PREG_START_DATE),
         M23_AGE_VISIT_WKS = as.numeric(ymd(M23_CLOSE_DSSTDAT)-PREG_START_DATE) %/% 7) %>% 
  ## 3. assign indicator variable for closeout window (what ga window did the woman closeout in)
  mutate(M23_CLOSEOUT_WINDOW =   ifelse(M23_CLOSEOUT_PERIOD == "ANC" &  M23_AGE_VISIT_DAYS >= 126 & M23_AGE_VISIT_DAYS <= 181 & US_GA_WKS_ENROLL <= 17, 2, ## only participants who are <= 17wks at enrollment will have this visit 
                                 ifelse(M23_CLOSEOUT_PERIOD == "ANC" & M23_AGE_VISIT_DAYS <=139, 1, 
                                 ifelse(M23_CLOSEOUT_PERIOD == "ANC" & M23_AGE_VISIT_DAYS >= 182 & M23_AGE_VISIT_DAYS <= 216, 3, 
                                 ifelse(M23_CLOSEOUT_PERIOD == "ANC" & M23_AGE_VISIT_DAYS >= 217 & M23_AGE_VISIT_DAYS <= 237, 4, ## MIGHT BE UPDATING THE 237 NUMBER 
                                    ## Since all visit types are assigned based on the GA at the time of assessment, we don't need to worry about 4 vs 5. 
                                    ## if ANC32 is missed, it is conducted at ANC36 visit
                                 ifelse(M23_CLOSEOUT_PERIOD == "ANC" & M23_AGE_VISIT_DAYS >= 238 & M23_AGE_VISIT_DAYS <= 322, 5, 88)))))) %>% 
  mutate(CLOSEOUT = case_when(!is.na(M23_AGE_VISIT_WKS) ~ 1, TRUE ~ 0))



## 4.4 export MatData_Anc_Visits ----
save(MatData_Anc_Visits, file= paste0(path_to_save, "MatData_Anc_Visits",".RData",sep = ""))

## examples below: 
# if someone delievered at 213 days (30wks); NO anc28/ anc32/  anc36
# if someone delievered at 54 days (7wks); NO ANC20 / anc28/ anc32/  anc36
# if someone delievered at 310 days (44wks);  ANC20 / anc28/ anc32/  anc36
# if someone delievered at 270 days (38wks);  NO anc36
#**************************************************************************************
### MatData_Ipc_Visits
# input: MatData_Screen_Enroll
# Includes all women who are enrolled

## this table is a bit different because 3 different datasets are used to define each sub-section
# Table #: IPC visit completion and PRISMA protocol compliance for IPC
  # "IPC Visit Completion" section: 
    # output: MatData_Ipc_Visits_Mat (tables mentioned above will use MatData_Ipc_Visits_Mat as the input)
  
  # "Maternal IPC Protocol Compliance" section: 
    # output: MatData_Ipc_Visits_Compliance_Mat (tables mentioned above will use MatData_Ipc_Visits_Compliance_Mat as the input)
  
  # "Infant IPC Protocol Compliance" section: 
    # output: MatData_Ipc_Visits_Compliance_Inf (tables mentioned above will use MatData_Ipc_Visits_Compliance_Inf as the input)

# Figure #: Distribution of gestational ages of participants who passed the IPC window and do not yet have a pregnancy outcome reported
    # output: MatData_Ipc_Visits_Mat (figure mentioned above will use MatData_Ipc_Visits_Mat as the input)

#**************************************************************************************
# 5. IPC visit data (MatData_Ipc_Visits) ----
# extract unique MOM/INFANT pair from infant wide data  - one row for each MOM/INFANT pair
InfData_Report_Mat <- InfData_Report %>% distinct(SITE, MOMID, PREGID, .keep_all = TRUE )

## 5.1 calculating windows ----
MatData_Ipc_Visits_Mat <- MatData_Report_out %>% 
  select(SITE, MOMID, PREGID, ENROLL, DOB, EDD_US,PREG_START_DATE, ENDPREG_DAYS,M09_TYPE_VISIT_6, M23_CLOSE_DSSTDAT,
         M10_TYPE_VISIT_6, M09_VISIT_COMPLETE_6, M10_VISIT_COMPLETE_6,M23_CLOSE_DSDECOD, # M09_INFANTS_FAORRES_6
         contains("GESTAGE_AT_VISIT_DAYS")) %>% 
  filter(ENROLL == 1,
         (is.na(ENDPREG_DAYS) | ENDPREG_DAYS > 139)) %>% ## filter for anyone who is enrolled and has delivered (DOB is not NA) 
  # join with InfData_Report_Mom
  left_join(InfData_Report_Mat[c("SITE", "MOMID", "PREGID", "INFANTID", "M11_TYPE_VISIT_6", "M11_VISIT_COMPLETE_6")], 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  ## CALCULATE LATE IPC WINDOWS
  mutate(IPC_LATE = PREG_START_DATE + 300) %>% 
  ## using upload date
  mutate(IPC_PASS = ifelse(IPC_LATE<UploadDate, 1, 0)) %>%  
  ## CALCULATE INDICATOR VARIALBE FOR ANY VISIT TYPE = i
  mutate(ANY_TYPE_VISIT_COMPLETE_6 = ifelse((M09_TYPE_VISIT_6 == 6 & M09_VISIT_COMPLETE_6 == 1) | 
                                              (M10_TYPE_VISIT_6 == 6 & M10_VISIT_COMPLETE_6 == 1), 1, 0)) %>% 
  ## GENERATE INDICATOR VARIABLE FOR PARTICPANTS WHO HAVE CLOSED OUT EITHER DUE TO LTFU (M23_CLOSE_DSDECOD==4), 
      # WITHDREW (M23_CLOSE_DSDECOD==5), OR INVESTIGATOR CLOSED (M23_CLOSE_DSDECOD==6)
  # if a woman any closeout selected 
  ## 5.2 generate closeout indicator ----
  mutate(CLOSED_OUT =  ifelse(is.na(M23_CLOSE_DSDECOD), 0, 
                              ifelse(M23_CLOSE_DSDECOD %in% c(1, 2, 3, 4, 5, 6), 1, 0)))  %>% 
  ## CALCULATE INDICATOR VARIALBE FOR DENOMINATOR
  mutate(IPC_DENOM = ifelse((ANY_TYPE_VISIT_COMPLETE_6==1 | IPC_PASS==1) & 
                              ((M23_CLOSE_DSSTDAT > IPC_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)) %>%  # only include participants who have not yet closed out
                              # CLOSED_OUT != 1, 1, 0)) %>% ## remove any participant who has been lost to follow up NEW 
  ## CALCULATE INDICATOR VARIALBE FOR MISSING ALL VISIT TYPE = 6
  mutate(GA42_MISSING_IPC = ifelse((IPC_DENOM ==1) & is.na(ANY_TYPE_VISIT_COMPLETE_6), 1, 0)) 
  
table(MatData_Ipc_Visits_Mat$GA42_MISSING_IPC, MatData_Ipc_Visits_Mat$SITE)

## 5.3 export MatData_Ipc_Visits_Mat ----
save(MatData_Ipc_Visits_Mat, file= paste0(path_to_save, "MatData_Ipc_Visits_Mat",".RData",sep = ""))

### 5.4 generate ipc protocol compliance (MOM ONLY) ----

# maternal protocol compliance -- only maternal for mnh09/10 - one row for each mom 
MatData_Ipc_Visits_Compliance_Mat <- MatData_Report_out %>% 
  select(SITE, MOMID, PREGID, ENROLL, DOB,PREG_START_DATE, EDD_US, ENDPREG_DAYS,M09_TYPE_VISIT_6, 
         M10_TYPE_VISIT_6, M09_VISIT_COMPLETE_6, M10_VISIT_COMPLETE_6 ) %>% # M09_INFANTS_FAORRES_6
  filter(ENROLL == 1,
         (is.na(ENDPREG_DAYS) | ENDPREG_DAYS > 139)) %>% ## filter for anyone who is enrolled and has delivered (DOB is not NA) 
  ## CALCULATE INDICATOR VARIALBE FOR ANY VISIT TYPE = i
  mutate(ANY_TYPE_VISIT_COMPLETE_6 = ifelse((M09_TYPE_VISIT_6 == 6 & M09_VISIT_COMPLETE_6 == 1) | 
                                              (M10_TYPE_VISIT_6 == 6 & M10_VISIT_COMPLETE_6 == 1), 1, 0)) 

## export 
save(MatData_Ipc_Visits_Compliance_Mat, file= paste0(path_to_save, "MatData_Ipc_Visits_Compliance_Mat",".RData",sep = ""))

### 5.5 generate ipc protocol compliance (INFANT ONLY) ----

# infant protocol compliance -- only infant for mnh11 - one row for each infant 
MatData_Ipc_Visits_Compliance_Inf <- InfData_Report %>% 
  select(SITE, INFANTID, PREGID, M11_TYPE_VISIT_6, M11_VISIT_COMPLETE_6) %>% 
  ## remove any duplicates
  group_by(INFANTID) %>% 
  mutate(dup = n(), 
         remove = case_when(dup >1 & (is.na(M11_TYPE_VISIT_6) | is.na(PREGID)) ~ 1, TRUE ~ 0)) %>% 
  filter(remove ==0) %>% 
  select(-dup, -remove) %>% 
  ungroup() %>% 
  full_join(MatData_Report_out[c("SITE", "MOMID", "PREGID", "ENROLL", "DOB", "EDD_US", "ENDPREG_DAYS","M09_TYPE_VISIT_6", 
                                    "M10_TYPE_VISIT_6", "M09_VISIT_COMPLETE_6", "M10_VISIT_COMPLETE_6")], # , "M09_INFANTS_FAORRES_6"
            by = c("SITE",  "PREGID"), multiple = "all") %>% 
  filter(ENROLL == 1 & ENDPREG_DAYS > 139) %>% 
  relocate(MOMID, .after = SITE) %>% 
  relocate(INFANTID, .after = PREGID) %>% 
  ## CALCULATE INDICATOR VARIALBE FOR ANY VISIT TYPE = i
  mutate(ANY_TYPE_VISIT_COMPLETE_6 = ifelse((M09_TYPE_VISIT_6 == 6 & M09_VISIT_COMPLETE_6 == 1) | 
                                              (M10_TYPE_VISIT_6 == 6 & M10_VISIT_COMPLETE_6 == 1) | 
                                              (M11_TYPE_VISIT_6 == 6 & M11_VISIT_COMPLETE_6 == 1), 1, 0)) 

## export 

save(MatData_Ipc_Visits_Compliance_Inf, file= paste0(path_to_save, "MatData_Ipc_Visits_Compliance_Inf",".RData",sep = ""))

#**************************************************************************************
### MatData_Pnc_Visits
# input: MatData_Screen_Enroll
# Includes all women who are enrolled. Includes constructed variables relevant to PNC 

# Table #: ANC and PNC visit completion for PRISMA
# output: MatData_Pnc_Visits (tables mentioned above will use MatData_Pnc_Visits as the input [for the PNC portion only])
#**************************************************************************************
# 6. PNC data set (MatData_Pnc_Visits) ----

## MatData_Pnc_Visits
## 6.1 calculate visit windows ----
MatData_Pnc_Visits <- MatData_Report_out %>% 
  select(SITE,SCRNID, MOMID, PREGID, ENROLL,US_GA_WKS_ENROLL, DOB, PREG_START_DATE,ENDPREG_DAYS,MISCARRIAGE,
         ends_with("_7"),  ends_with("_8"),  ends_with("_9"), 
         ends_with("_10"), ends_with("_11"), ends_with("_12"),starts_with("M09_BIRTH_DSTERM_INF"), contains("M23_")) %>% 
  ## CALCULATE ON TIME AND LATE PNC WINDOWS 
  mutate(PNC0_ONTIME = DOB + as.difftime(5, unit="days"),
         PNC0_LATE = DOB + as.difftime(5, unit="days"),
         PNC1_ONTIME = DOB + as.difftime(14, unit="days"),
         PNC1_LATE = DOB + as.difftime(14, unit="days"),
         PNC4_ONTIME = DOB + as.difftime(35, unit="days"),
         PNC4_LATE = DOB + as.difftime(35, unit="days"),
         PNC6_ONTIME = DOB + as.difftime(55, unit="days"),
         PNC6_LATE = DOB + as.difftime(104, unit="days"),
         PNC6_LATE_PROT = DOB + as.difftime(90, unit="days"),
         PNC26_ONTIME = DOB + as.difftime(202, unit="days"),
         PNC26_LATE = DOB + as.difftime(279, unit="days"),
         PNC52_ONTIME = DOB + as.difftime(384, unit="days"),
         PNC52_LATE = DOB + as.difftime(454, unit="days")) %>%
  ## CALCULATE INDICATOR VARIABLES for passed ON-TIME PNC window - same as overdue code, but exclude the visit completion piece
  ## using upload date
  mutate(PNC0_PASS = ifelse(PNC0_ONTIME<UploadDate, 1, 0),
         PNC1_PASS = ifelse(PNC1_ONTIME<UploadDate, 1, 0),
         PNC4_PASS = ifelse(PNC4_ONTIME<UploadDate, 1, 0),
         PNC6_PASS = ifelse(PNC6_ONTIME<UploadDate, 1, 0),
         PNC26_PASS = ifelse(PNC26_ONTIME<UploadDate, 1, 0),
         PNC52_PASS = ifelse(PNC52_ONTIME<UploadDate, 1, 0)) %>% 
  ## CALCULATE INDICATOR VARIABLES for passed LATE PNC window - same as overdue code, but exclude the visit completion piece
  ## using upload date
  mutate(PNC0_PASS_LATE = ifelse(PNC0_LATE<UploadDate, 1, 0),
         PNC1_PASS_LATE = ifelse(PNC1_LATE<UploadDate, 1, 0),
         PNC4_PASS_LATE = ifelse(PNC4_LATE<UploadDate, 1, 0),
         PNC6_PASS_LATE = ifelse(PNC6_LATE<UploadDate, 1, 0),
         PNC6_PASS_LATE_PROT = ifelse(PNC6_LATE_PROT<UploadDate, 1, 0),
         PNC26_PASS_LATE = ifelse(PNC26_LATE<UploadDate, 1, 0),
         PNC52_PASS_LATE = ifelse(PNC52_LATE<UploadDate, 1, 0)) %>% 
  ## CALCULATE INDICATOR VARIABLES for missed PNC visits 
  ## max date has to be less than the upload date
  mutate(PNC0_OVERDUE = ifelse(UploadDate>PNC0_LATE & is.na(M12_VISIT_COMPLETE_7), 1, 0),
         PNC1_OVERDUE = ifelse(UploadDate>PNC1_LATE & is.na(M12_VISIT_COMPLETE_8), 1, 0),
         PNC4_OVERDUE = ifelse(UploadDate>PNC4_LATE & is.na(M12_VISIT_COMPLETE_9), 1, 0),
         PNC6_OVERDUE = ifelse(UploadDate>PNC6_LATE & is.na(M12_VISIT_COMPLETE_10), 1, 0),
         PNC26_OVERDUE = ifelse(UploadDate>PNC26_LATE & is.na(M12_VISIT_COMPLETE_11), 1, 0),
         PNC52_OVERDUE = ifelse(UploadDate>PNC52_LATE & is.na(M12_VISIT_COMPLETE_12), 1, 0)) %>% 
  ## CALCULATE INDICATOR VARIALBE FOR ANY VISIT TYPE = i
  ## 6.2 generate visit completion indicators ----
  mutate(ANY_TYPE_VISIT_COMPLETE_7 = ifelse((M06_TYPE_VISIT_7 == 7 & M06_VISIT_COMPLETE_7 == 1) |
                                              (M12_TYPE_VISIT_7 == 7 & M12_VISIT_COMPLETE_7 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_8 = ifelse((M06_TYPE_VISIT_8 == 8 & M06_VISIT_COMPLETE_8 == 1) |
                                              (M12_TYPE_VISIT_8 == 8 & M12_VISIT_COMPLETE_8 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_9 = ifelse((M06_TYPE_VISIT_9 == 9 & M06_VISIT_COMPLETE_9 == 1) |
                                              (M12_TYPE_VISIT_9 == 9 & M12_VISIT_COMPLETE_9 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_10 = ifelse((M05_TYPE_VISIT_10 == 10 & M05_VISIT_COMPLETE_10 == 1) |
                                               (M06_TYPE_VISIT_10 == 10 & M06_VISIT_COMPLETE_10 == 1) |
                                               (M07_TYPE_VISIT_10 == 10 & M07_VISIT_COMPLETE_10 == 1) |
                                               (M08_TYPE_VISIT_10 == 10 & M08_VISIT_COMPLETE_10 == 1) | 
                                               (M12_TYPE_VISIT_10 == 10 & M12_VISIT_COMPLETE_10 == 1) | 
                                               (M25_TYPE_VISIT_10 == 10 & M25_VISIT_COMPLETE_10 == 1) | 
                                               (M26_TYPE_VISIT_10 == 10 & M26_VISIT_COMPLETE_10 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_11 = ifelse((M05_TYPE_VISIT_11 == 11 & M05_VISIT_COMPLETE_11 == 1) |
                                               (M06_TYPE_VISIT_11 == 11 & M06_VISIT_COMPLETE_11 == 1) |
                                               (M07_TYPE_VISIT_11 == 11 & M07_VISIT_COMPLETE_11 == 1) |
                                               (M08_TYPE_VISIT_11 == 11 & M08_VISIT_COMPLETE_11 == 1) |
                                               (M12_TYPE_VISIT_11 == 11 & M12_VISIT_COMPLETE_11 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_12 = ifelse((M05_TYPE_VISIT_12 == 12 & M05_VISIT_COMPLETE_12 == 1) |
                                               (M06_TYPE_VISIT_12 == 12 & M06_VISIT_COMPLETE_12 == 1) |
                                               (M12_TYPE_VISIT_12 == 12 & M12_VISIT_COMPLETE_12 == 1), 1, 0)) %>% 
  # ## EXTRACT BIRTHOUTCOME
  mutate(BIRTH_OUTCOME_YN = ifelse(M09_BIRTH_DSTERM_INF1_6 == 1 | M09_BIRTH_DSTERM_INF1_6 == 2 |
                                   M09_BIRTH_DSTERM_INF2_6 == 1 | M09_BIRTH_DSTERM_INF2_6 == 2 |
                                   M09_BIRTH_DSTERM_INF3_6 == 1 | M09_BIRTH_DSTERM_INF3_6 == 2 |
                                   M09_BIRTH_DSTERM_INF4_6 == 1 | M09_BIRTH_DSTERM_INF4_6 == 2,1,  0))  %>%
  ## GENERATE INDICATOR VARIALBE FOR CENSORING 
  ## 6.3 generate censoring indicators ----
  ## 1. generate indicator variable if closeout was during ANC or PNC period 
  mutate(M23_CLOSEOUT_PERIOD = ifelse(!is.na(DOB), "PNC", "ANC"))

## 6.4 export MatData_Pnc_Visits ----
save(MatData_Pnc_Visits, file= paste0(path_to_save, "MatData_Pnc_Visits",".RData",sep = ""))
#**************************************************************************************
#### VISIT COMPLETION 
#* Are forms completed with visit status = 1 or 2? 
#* Table 5

# Table #: ANC and PNC visit completion for PRISMA
  # output: Visit_Complete_Anc (tables mentioned above will use Visit_Complete_Anc as the input)
  # output: Visit_Complete_Pnc (tables mentioned above will use Visit_Complete_Pnc as the input)

#* Num: Any form with visit type = i AND visit status = 1 or 2 
#* Denom: Passed window for visit type = i
#**************************************************************************************
## ANC

# 7. ANC visit completion ----
Visit_Complete_Anc <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, PREGID,US_GA_WKS_ENROLL, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"),
         contains("_ONTIME"),contains("_LATE"),ENDPREG_DAYS, M23_CLOSE_DSSTDAT) %>% 
  ## ONTIME WINDOWS 
  mutate(VC_ENROLL_NUM =ifelse(ANY_TYPE_VISIT_COMPLETE_1 == 1 & ENROLL_PASS == 1 , 1, 0), ## if the closeout date is passed the enroll on time window then they are included
         VC_ANC20_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_2 == 1 & ANC20_PASS == 1 & 
                                 (ENDPREG_DAYS>160 | is.na(ENDPREG_DAYS)), 1, 0),
         VC_ANC28_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_3 == 1 & ANC28_PASS == 1  & 
                                 (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS)), 1, 0),
         
         VC_ANC32_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_4 == 1 & ANC32_PASS == 1  &
                                 (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)), 1, 0),
         
         VC_ANC36_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_5 == 1 & ANC36_PASS == 1 & 
                                 (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS)), 1, 0)) %>% 
  
  ## exclude particpants who do not have EDD -- we are not able to calculate their windows - exclude from num and denom
  mutate(VC_ENROLL_NUM = ifelse(is.na(ENROLL_PASS), NA, VC_ENROLL_NUM),
         VC_ANC20_NUM = ifelse(is.na(ANC20_PASS), NA, VC_ANC20_NUM),
         VC_ANC28_NUM = ifelse(is.na(ANC28_PASS), NA, VC_ANC28_NUM),
         VC_ANC32_NUM = ifelse(is.na(ANC32_PASS), NA, VC_ANC32_NUM),
         VC_ANC36_NUM = ifelse(is.na(ANC36_PASS), NA, VC_ANC36_NUM)) %>% 
  ## LATE WINDOWS
  # Numerator for ANC Visit Completion
  mutate(VC_ENROLL_NUM_LATE =ifelse(ANY_TYPE_VISIT_COMPLETE_1 == 1 & ENROLL_PASS_LATE == 1, 1, 0),
         VC_ANC20_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_2 == 1 & ANC20_PASS_LATE == 1 & 
                                      (ENDPREG_DAYS>181 | is.na(ENDPREG_DAYS)), 1, 0),
         
         VC_ANC28_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_3 == 1  & ANC28_PASS_LATE &
                                      (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS)), 1, 0),
         
         VC_ANC32_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_4 == 1 & ANC32_PASS_LATE == 1  &
                                      (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)), 1, 0),
         
         VC_ANC36_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_5 == 1 & ANC36_PASS_LATE == 1  &
                                      (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS)), 1, 0)) %>% 
  
  ## exclude particpants who do not have EDD -- we are not able to calculate their windows - exclude from num and denom
  mutate(VC_ENROLL_NUM_LATE = ifelse(is.na(ENROLL_PASS_LATE), NA, VC_ENROLL_NUM_LATE),
         VC_ANC20_NUM_LATE = ifelse(is.na(ANC20_PASS_LATE), NA, VC_ANC20_NUM_LATE),
         VC_ANC28_NUM_LATE = ifelse(is.na(ANC28_PASS_LATE), NA, VC_ANC28_NUM_LATE),
         VC_ANC32_NUM_LATE = ifelse(is.na(ANC32_PASS_LATE), NA, VC_ANC32_NUM_LATE),
         VC_ANC36_NUM_LATE = ifelse(is.na(ANC36_PASS_LATE), NA, VC_ANC36_NUM_LATE)) %>% 
  ## generate denominators - ONTIME
  mutate(VC_ENROLL_DENOM = ifelse(ENROLL_PASS==1, 1, 0), 
         VC_ANC20_DENOM = ifelse(ANC20_PASS==1 & US_GA_WKS_ENROLL <= 17 & 
                                   (ENDPREG_DAYS>160 | is.na(ENDPREG_DAYS)) & ## birth outcome after window or has not yet delivered
                                   ((M23_CLOSE_DSSTDAT > ANC20_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),  ## closed out after window or has not yet closed out
         VC_ANC28_DENOM = ifelse(ANC28_PASS==1  &
                                   (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS)) & ## birth outcome after window or has not yet delivered
                                   ((M23_CLOSE_DSSTDAT > ANC28_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),  ## closed out after window or has not yet closed out
         VC_ANC32_DENOM = ifelse(ANC32_PASS==1  & 
                                   (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & ## birth outcome after window or has not yet delivered
                                 ((M23_CLOSE_DSSTDAT > ANC32_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),  ## closed out after window or has not yet closed out
         VC_ANC36_DENOM = ifelse(ANC36_PASS==1  &
                                   (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS)) & ## birth outcome after window or has not yet delivered
                                 ((M23_CLOSE_DSSTDAT > ANC28_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),  ## closed out after window or has not yet closed out 
  ) %>% 
  ## generate denominators - LATE
  mutate(VC_ENROLL_DENOM_LATE = ifelse(ENROLL_PASS_LATE == 1, 1, 0), 
         VC_ANC20_DENOM_LATE = ifelse(ANC20_PASS_LATE == 1 & US_GA_WKS_ENROLL <= 17 &
                                        (ENDPREG_DAYS>181 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC20_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),  ## closed out after window or has not yet closed out
         VC_ANC28_DENOM_LATE = ifelse(ANC28_PASS_LATE == 1  &
                                        (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC28_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0), 
         VC_ANC32_DENOM_LATE = ifelse(ANC32_PASS_LATE == 1  & 
                                        (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC32_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0), 
         VC_ANC36_DENOM_LATE = ifelse(ANC36_PASS_LATE == 1  & 
                                        (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS))  & 
                                        ((M23_CLOSE_DSSTDAT > ANC36_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)
  )

## export 
save(Visit_Complete_Anc, file= paste0(path_to_save, "Visit_Complete_Anc",".RData",sep = ""))

# PNC
# 8. PNC visit completion ----
## i filtered out anyone who has an endpreg and then
# for pnc visits where we do not expect those with miscarriages, i filter 
Visit_Complete_Pnc <- MatData_Pnc_Visits %>%
  filter(!is.na(ENDPREG_DAYS)) %>% ## only include participants with a birth outcome
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"),
         contains("_PASS"), contains("_ONTIME"), contains("_LATE"), M23_CLOSE_DSSTDAT,M23_CLOSE_DSDECOD, DOB, ENDPREG_DAYS) %>%
  ## ON TIME WINDOWS
  # Numerator for PNC Visit Completion -- exclude any particpants who have closed out (M23_CLOSE_DSSTDAT must be greater than the on-time window (if it's not then that means the participant has closeout in the window))
  mutate(VC_PNC0_NUM =ifelse(ANY_TYPE_VISIT_COMPLETE_7 == 1 & PNC0_PASS == 1, 1, 0),
         VC_PNC1_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_8 == 1 & PNC1_PASS == 1, 1, 0),
         VC_PNC4_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_9 == 1 & PNC4_PASS == 1, 1, 0),
         VC_PNC6_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_10 == 1 & PNC6_PASS == 1, 1, 0),
         VC_PNC26_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_11 == 1 & PNC26_PASS == 1, 1, 0),
         VC_PNC52_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_12 == 1 & PNC52_PASS == 1, 1, 0)) %>%
  ## exclude particpants who do not have DOB -- we are not able to calculate their windows - exclude from num and denom
  mutate(VC_PNC0_NUM = ifelse(is.na(PNC0_PASS), NA, VC_PNC0_NUM),
         VC_PNC1_NUM = ifelse(is.na(PNC1_PASS), NA, VC_PNC1_NUM),
         VC_PNC4_NUM = ifelse(is.na(PNC4_PASS), NA, VC_PNC4_NUM),
         VC_PNC6_NUM = ifelse(is.na(PNC6_PASS), NA, VC_PNC6_NUM),
         VC_PNC26_NUM = ifelse(is.na(PNC26_PASS), NA, VC_PNC26_NUM),
         VC_PNC52_NUM = ifelse(is.na(PNC52_PASS), NA, VC_PNC52_NUM)) %>%
  ## LATE WINDOWS
  # Numerator for PNC Visit Completion -- NEED TO EXCLUDE ANY PARTICIAPNT WHO HAS CLOSED OUT YET (M23_CLOSE_DSSTDAT).
  mutate(VC_PNC0_NUM_LATE =ifelse(ANY_TYPE_VISIT_COMPLETE_7 == 1 & PNC0_PASS_LATE == 1 &
                                    ((M23_CLOSE_DSSTDAT > PNC0_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC1_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_8 == 1 & PNC1_PASS_LATE == 1 &
                                     ((M23_CLOSE_DSSTDAT > PNC1_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC4_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_9 == 1 & PNC4_PASS_LATE == 1 &
                                     ((M23_CLOSE_DSSTDAT > PNC4_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC6_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_10 == 1 & PNC6_PASS_LATE == 1 &
                                     ((M23_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC6_NUM_LATE_PROT = ifelse(ANY_TYPE_VISIT_COMPLETE_10 == 1 & PNC6_PASS_LATE_PROT == 1 &
                                     ((M23_CLOSE_DSSTDAT > PNC6_LATE_PROT) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         
         VC_PNC26_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_11 == 1 & PNC26_PASS_LATE == 1 &
                                      ((M23_CLOSE_DSSTDAT > PNC26_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC52_NUM_LATE = ifelse((ANY_TYPE_VISIT_COMPLETE_12 == 1 & PNC52_PASS_LATE == 1 & is.na(M23_CLOSE_DSSTDAT)) |
                                      (ANY_TYPE_VISIT_COMPLETE_12 == 1 & M23_CLOSE_DSDECOD ==1), 1, 0)) %>%
  ## exclude particpants who do not have DOB -- we are not able to calculate their windows - exclude from num and denom
  mutate(VC_PNC0_NUM_LATE = ifelse(is.na(PNC0_PASS_LATE), NA, VC_PNC0_NUM_LATE),
         VC_PNC1_NUM_LATE = ifelse(is.na(PNC1_PASS_LATE), NA, VC_PNC1_NUM_LATE),
         VC_PNC4_NUM_LATE = ifelse(is.na(PNC4_PASS_LATE), NA, VC_PNC4_NUM_LATE),
         VC_PNC6_NUM_LATE = ifelse(is.na(PNC6_PASS_LATE), NA, VC_PNC6_NUM_LATE),
         VC_PNC6_NUM_LATE_PROT = ifelse(is.na(PNC6_PASS_LATE_PROT), NA, VC_PNC6_NUM_LATE_PROT),

         VC_PNC26_NUM_LATE = ifelse(is.na(PNC26_PASS_LATE), NA, VC_PNC26_NUM_LATE),
         VC_PNC52_NUM_LATE = ifelse(is.na(PNC52_PASS_LATE), NA, VC_PNC52_NUM_LATE)) %>%
  ## generate denominators - late
  mutate(VC_PNC0_DENOM_LATE = ifelse(PNC0_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC0_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC1_DENOM_LATE = ifelse(PNC1_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC1_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC4_DENOM_LATE = ifelse(PNC4_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC4_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC6_DENOM_LATE = ifelse(PNC6_PASS_LATE==1 & 
                                       ((M23_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         
         VC_PNC6_DENOM_LATE_PROT = ifelse(PNC6_PASS_LATE_PROT==1 & 
                                       ((M23_CLOSE_DSSTDAT > PNC6_LATE_PROT) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         
         VC_PNC26_DENOM_LATE = ifelse(PNC26_PASS_LATE==1 & ENDPREG_DAYS>139 & 
                                        ((M23_CLOSE_DSSTDAT > PNC26_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         
         VC_PNC52_DENOM_LATE = ifelse((PNC52_PASS_LATE==1 & ENDPREG_DAYS>139 & is.na(M23_CLOSE_DSSTDAT)) | 
                                        M23_CLOSE_DSDECOD==1, 1, 0)

  )

## export 
save(Visit_Complete_Pnc, file= paste0(path_to_save, "Visit_Complete_Pnc",".RData",sep = ""))
#**************************************************************************************
#### PROTOCOL COMPLIANCE  
#* Definition: Are all expected forms for the visit complete? 

# Table #: ANC protocol compliance for PRISMA
  # output: Prot_Compliance_Anc (tables mentioned above will use Prot_Compliance_Anc as the input)

# Table #: PNC protocol compliance for PRISMA
  # output: Prot_Compliance_Pnc (tables mentioned above will use Prot_Compliance_Pnc as the input)

# Table #: Protocol compliance for PRISMA depression assessment
# output: Prot_Compliance_MNH25 (tables mentioned above will use Prot_Compliance_MNH25 as the input)

# Table #: Protocol compliance for ReMAPP fatigue assessment
# output: Prot_Compliance_MNH26 (tables mentioned above will use Prot_Compliance_MNH26 as the input)

#* Num (hard code in monitoring report rmd): For each form: visit type = i AND visit status = 1 or 2 AND passed window for visit type = i 
#* Denom: Any form with visit type = i AND visit status = 1 or 2 AND passed window for visit type = i 
#**************************************************************************************
## ANC
# 9. ANC protocol compliance (Prot_Compliance_Anc) ----

## CALCULATE DENOMINATORS FOR PROTOCOL COMPLIANCE  
Prot_Compliance_Anc <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, PREGID, ENROLL, M02_SCRN_OBSSTDAT,SCREEN,US_GA_WKS_ENROLL, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), 
         contains("_VISIT_COMPLETE_")) %>% 
  # DENOMINATOR for protocol compliance 
  mutate(PC_ENROLL_DENOM =ifelse(ANY_TYPE_VISIT_COMPLETE_1 == 1 & ENROLL_PASS_LATE == 1, 1, 0),
         PC_ANC20_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_2 == 1 & ANC20_PASS_LATE == 1, 1, 0),
         PC_ANC28_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_3 == 1 & ANC28_PASS_LATE == 1, 1, 0),
         PC_ANC32_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_4 == 1 & ANC32_PASS_LATE == 1, 1, 0),
         PC_ANC36_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_5 == 1 & ANC36_PASS_LATE == 1, 1, 0)) 

## export 
save(Prot_Compliance_Anc, file= paste0(path_to_save, "Prot_Compliance_Anc",".RData",sep = ""))

# 10. PNC protocol compliance (Prot_Compliance_Pnc) ----
## PNC 
## CALCULATE DENOMINATORS FOR PROTOCOL COMPLIANCE  
Prot_Compliance_Pnc <- MatData_Pnc_Visits %>% 
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), ENDPREG_DAYS,
         contains("_VISIT_COMPLETE_"), BIRTH_OUTCOME_YN, contains("M23_"), contains("_ONTIME"), M23_CLOSE_DSDECOD, M06_TYPE_VISIT_7, M06_VISIT_COMPLETE_7, M12_TYPE_VISIT_7,M12_VISIT_COMPLETE_7) %>% 
  #filter(BIRTH_OUTCOME_YN == 1) %>% 
  filter(!is.na(ENDPREG_DAYS)) %>% ## exclude anyone without a birth outcome (missing ENDPREG_DAYS)
  # DENOMINATOR for protocol compliance
  mutate(PC_PNC0_DENOM =ifelse(ANY_TYPE_VISIT_COMPLETE_7 == 1 & PNC0_PASS_LATE == 1, 1, 0),
         PC_PNC1_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_8 == 1 & PNC1_PASS_LATE == 1, 1, 0),
         PC_PNC4_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_9 == 1 & PNC4_PASS_LATE == 1, 1, 0),
         PC_PNC6_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_10 == 1 & PNC6_PASS_LATE == 1 , 1, 0),
         PC_PNC26_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_11 == 1 & PNC26_PASS_LATE == 1, 1, 0),
         PC_PNC52_DENOM = ifelse((ANY_TYPE_VISIT_COMPLETE_12 == 1 & PNC52_PASS_LATE == 1) | 
                                   (ANY_TYPE_VISIT_COMPLETE_12 == 1 &  M23_CLOSE_DSDECOD== 1), 1, 0)) 

## export 
save(Prot_Compliance_Pnc, file= paste0(path_to_save, "Prot_Compliance_Pnc",".RData",sep = ""))


# ## MNH25 protocol compliance 
MNH25_Pnc <- MatData_Pnc_Visits %>% select(SITE, MOMID, PREGID, DOB,M25_VISIT_COMPLETE_10, ANY_TYPE_VISIT_COMPLETE_10, PNC6_PASS_LATE ,PNC6_LATE)

Prot_Compliance_MNH25 <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, PREGID, M02_SCRN_OBSSTDAT,US_GA_WKS_ENROLL,  ENDPREG_DAYS,M23_CLOSE_DSSTDAT,
         ANY_TYPE_VISIT_COMPLETE_1,ANY_TYPE_VISIT_COMPLETE_2, ANY_TYPE_VISIT_COMPLETE_4, ANY_TYPE_VISIT_COMPLETE_5,
         ENROLL_PASS_LATE, ANC20_PASS_LATE, ANC32_PASS_LATE, ANC36_PASS_LATE,
         ENROLL_LATE, ANC20_LATE, ANC32_LATE, ANC36_LATE,
         M25_VISIT_COMPLETE_1, M25_VISIT_COMPLETE_2, M25_VISIT_COMPLETE_4, M25_VISIT_COMPLETE_5) %>%
  
  # NUMERATOR for protocol compliance 
  mutate(M25_ANCLESS20_NUM =ifelse(((M25_VISIT_COMPLETE_1 == 1 & ANC20_LATE < UploadDate) | 
                                    (M25_VISIT_COMPLETE_2 == 1 & ANC20_LATE < UploadDate)) & 
                                     (ENDPREG_DAYS>181 | is.na(ENDPREG_DAYS)) & 
                                     ((M23_CLOSE_DSSTDAT > ANC20_LATE) | is.na(M23_CLOSE_DSSTDAT)),1, 0),
         
         M25_ANCOVER31_NUM =ifelse(((M25_VISIT_COMPLETE_4 == 1 | M25_VISIT_COMPLETE_5) & ANC32_PASS_LATE == 1) &
                                     (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & 
                                     ((M23_CLOSE_DSSTDAT > ANC32_LATE) | is.na(M23_CLOSE_DSSTDAT)),1,0)
  ) %>% 
  # DENOMINATOR for protocol compliance, 
  mutate(PC_ANCLESS20_DENOM = ifelse((ANC20_LATE < UploadDate) &
                                       (ENDPREG_DAYS>181 | is.na(ENDPREG_DAYS)) & 
                                       ((M23_CLOSE_DSSTDAT > ANC20_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1,0),

         PC_ANCOVER31_DENOM = ifelse(ANC32_PASS_LATE == 1 & 
                                       (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS))  & 
                                       ((M23_CLOSE_DSSTDAT > ANC32_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)) %>% 
  left_join(MNH25_Pnc, by = c("SITE", "MOMID", "PREGID")) %>% 
  
  mutate(PC_PNC6_DENOM = ifelse(PNC6_PASS_LATE == 1 & 
                                  ((M23_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0))

save(Prot_Compliance_MNH25, file= paste0(path_to_save, "Prot_Compliance_MNH25",".RData",sep = ""))


# 11. MNH26 protocol compliance (Prot_Compliance_MNH26) ----
## MNH26 protocol compliance 
MNH26_Pnc <- MatData_Pnc_Visits %>% select(SITE, MOMID, PREGID, DOB, M26_VISIT_COMPLETE_10, ANY_TYPE_VISIT_COMPLETE_10,PNC6_LATE, PNC6_PASS_LATE)

Prot_Compliance_MNH26 <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, PREGID, M02_SCRN_OBSSTDAT,US_GA_WKS_ENROLL, ENDPREG_DAYS,M23_CLOSE_DSSTDAT,
         ANY_TYPE_VISIT_COMPLETE_1,ANY_TYPE_VISIT_COMPLETE_2, ANY_TYPE_VISIT_COMPLETE_4, ANY_TYPE_VISIT_COMPLETE_5,
         ENROLL_PASS_LATE, ANC20_PASS_LATE, ANC32_PASS_LATE, ANC36_PASS_LATE,
         ENROLL_LATE, ANC20_LATE, ANC32_LATE, ANC36_LATE,
         M26_VISIT_COMPLETE_1, M26_VISIT_COMPLETE_2, M26_VISIT_COMPLETE_4, M26_VISIT_COMPLETE_5) %>% 
  left_join(mat_enroll %>% select(SITE, MOMID, PREGID, REMAPP_ENROLL, ENROLL_SCRN_DATE), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(REMAPP_ENROLL_CMC_FIX = case_when(SITE == "India-CMC" &
                                             ENROLL_SCRN_DATE >= "2023-06-20" & ENROLL_SCRN_DATE <= "2025-07-01" ~ 1, 
                                           TRUE ~ REMAPP_ENROLL)) %>% 
  
  mutate(REMAPP_ENROLL = case_when(SITE != "India-CMC" ~ REMAPP_ENROLL, 
                                   SITE == "India-CMC" ~ REMAPP_ENROLL_CMC_FIX, TRUE ~ REMAPP_ENROLL)) %>% 
  filter(REMAPP_ENROLL ==1)  %>% 
  # NUMERATOR for protocol compliance
  mutate(M26_ANCLESS20_NUM =ifelse(((M26_VISIT_COMPLETE_1 == 1 & ANC20_LATE < UploadDate) |
                                      (M26_VISIT_COMPLETE_2 == 1 & ANC20_LATE < UploadDate)) &
                                     (ENDPREG_DAYS>181 | is.na(ENDPREG_DAYS)) &
                                     ((M23_CLOSE_DSSTDAT > ANC20_LATE) | is.na(M23_CLOSE_DSSTDAT)),1, 0),
         M26_ANCOVER31_NUM =ifelse(((M26_VISIT_COMPLETE_4 == 1 | M26_VISIT_COMPLETE_5) & ANC32_PASS_LATE == 1) &
                                     (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) &
                                     ((M23_CLOSE_DSSTDAT > ANC32_LATE) | is.na(M23_CLOSE_DSSTDAT)),1,0)
  ) %>%
  # DENOMINATOR for protocol compliance
  mutate(PC_ANCLESS20_DENOM = ifelse((ANC20_LATE < UploadDate) &
                                       (ENDPREG_DAYS>181 | is.na(ENDPREG_DAYS)) & 
                                       ((M23_CLOSE_DSSTDAT > ANC20_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0), 
         PC_ANCOVER31_DENOM = ifelse(ANC32_PASS_LATE == 1 & 
                                       (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS))  & 
                                       ((M23_CLOSE_DSSTDAT > ANC32_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)) %>% 
  left_join(MNH26_Pnc, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(PC_PNC6_DENOM = ifelse(PNC6_PASS_LATE == 1 & 
                                  ((M23_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)) 

# Export
save(Prot_Compliance_MNH26, file= paste0(path_to_save, "Prot_Compliance_MNH26",".RData",sep = ""))
#**************************************************************************************
####  FORM COMPLETION
#* Are forms completed regardless of visit status? 

# Table #: ANC form completion for PRISMA
    # output: Form_Completion_Anc (tables mentioned above will use Form_Completion_Anc as the input)
    # note: MatData_Anc_Visits is used for the first summary row only 

# Table #: PNC visit maternal form completion for PRISMA
  # output: Form_Completion_Pnc (tables mentioned above will use Form_Completion_Pnc as the input)
  # note: MatData_Pnc_Visits is used for the first summary row only 

#* Num (hard code in monitoring report rmd): for each form: visit type = i AND have any visit status AND passed window for visit type = i 
#* Denom: Any form with visit type = i AND have any visit status AND passed window for visit type = i
#**************************************************************************************
# 12. ANC form completion (Form_Completion_Anc) ----
## ANC
Form_Completion_Anc <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, PREGID,M02_SCRN_OBSSTDAT,US_GA_WKS_ENROLL, ENROLL, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), 
         contains("_VISIT_COMPLETE_"),contains("_TYPE_VISIT_")) %>% 
  # DENOMINATOR for form completion
  # step 1. indicator for any type visit = i with any visit status 
  mutate(TYPE_VISIT_ANY_STATUS_1 = ifelse((M01_TYPE_VISIT_1 == 1 & !is.na(M01_VISIT_COMPLETE_1)) | 
                                            #(M03_TYPE_VISIT_1 == 1 & !is.na(M03_VISIT_COMPLETE_1)) |
                                            (M04_TYPE_VISIT_1 == 1 & !is.na(M04_VISIT_COMPLETE_1)) |
                                            (M05_TYPE_VISIT_1 == 1 & !is.na(M05_VISIT_COMPLETE_1)) |
                                            (M06_TYPE_VISIT_1 == 1 & !is.na(M06_VISIT_COMPLETE_1)) |
                                            (M07_TYPE_VISIT_1 == 1 & !is.na(M07_VISIT_COMPLETE_1)) |
                                            (M08_TYPE_VISIT_1 == 1 & !is.na(M08_VISIT_COMPLETE_1)), 1, 0),
         TYPE_VISIT_ANY_STATUS_2 = ifelse((M04_TYPE_VISIT_2 == 2 & !is.na(M04_VISIT_COMPLETE_2) & US_GA_WKS_ENROLL <= 17) |
                                            (M05_TYPE_VISIT_2 == 2 & !is.na(M05_VISIT_COMPLETE_2) & US_GA_WKS_ENROLL <= 17) |
                                            (M06_TYPE_VISIT_2 == 2 & !is.na(M06_VISIT_COMPLETE_2) & US_GA_WKS_ENROLL <= 17) |
                                            (M07_TYPE_VISIT_2 == 2 & !is.na(M07_VISIT_COMPLETE_2) & US_GA_WKS_ENROLL <= 17) |
                                            (M08_TYPE_VISIT_2 == 2 & !is.na(M08_VISIT_COMPLETE_2) & US_GA_WKS_ENROLL <= 17) | 
                                            (M25_TYPE_VISIT_2 == 2 & !is.na(M25_VISIT_COMPLETE_2) & US_GA_WKS_ENROLL <= 17) | 
                                            (M26_TYPE_VISIT_2 == 2 & !is.na(M26_VISIT_COMPLETE_2) & US_GA_WKS_ENROLL <= 17), 1, 0),
         TYPE_VISIT_ANY_STATUS_3 = ifelse((M04_TYPE_VISIT_3 == 3 & !is.na(M04_VISIT_COMPLETE_3)) |
                                            (M05_TYPE_VISIT_3 == 3 & !is.na(M05_VISIT_COMPLETE_3)) |
                                            (M06_TYPE_VISIT_3 == 3 & !is.na(M06_VISIT_COMPLETE_3)) |
                                            (M07_TYPE_VISIT_3 == 3 & !is.na(M07_VISIT_COMPLETE_3)) |
                                            (M08_TYPE_VISIT_3 == 3 & !is.na(M08_VISIT_COMPLETE_3)), 1, 0),
         TYPE_VISIT_ANY_STATUS_4 = ifelse((M01_TYPE_VISIT_4 == 4 & !is.na(M01_VISIT_COMPLETE_4)) | 
                                            (M04_TYPE_VISIT_4 == 4 & !is.na(M04_VISIT_COMPLETE_4)) |
                                            (M05_TYPE_VISIT_4 == 4 & !is.na(M05_VISIT_COMPLETE_4)) |
                                            (M06_TYPE_VISIT_4 == 4 & !is.na(M06_VISIT_COMPLETE_4)) |
                                            (M07_TYPE_VISIT_4 == 4 & !is.na(M07_VISIT_COMPLETE_4)) |
                                            (M08_TYPE_VISIT_4 == 4 & !is.na(M08_VISIT_COMPLETE_4)) | 
                                            (M25_TYPE_VISIT_4 == 4 & !is.na(M25_VISIT_COMPLETE_4)) | 
                                            (M26_TYPE_VISIT_4 == 4 & !is.na(M26_VISIT_COMPLETE_4)), 1, 0),
         TYPE_VISIT_ANY_STATUS_5 = ifelse((M04_TYPE_VISIT_5 == 5 & !is.na(M04_VISIT_COMPLETE_5)) |
                                            (M05_TYPE_VISIT_5 == 5 & !is.na(M05_VISIT_COMPLETE_5)) |
                                            (M06_TYPE_VISIT_5 == 5 & !is.na(M06_VISIT_COMPLETE_5)) |
                                            (M07_TYPE_VISIT_5 == 5 & !is.na(M07_VISIT_COMPLETE_5)) |
                                            (M08_TYPE_VISIT_5 == 5 & !is.na(M08_VISIT_COMPLETE_5)), 1, 0)) %>% 
  # step 2. generate denominator 
  mutate(FC_ENROLL_DENOM =ifelse(TYPE_VISIT_ANY_STATUS_1 == 1 & ENROLL_PASS_LATE == 1, 1, 0),
         FC_ANC20_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_2 == 1 & ANC20_PASS_LATE == 1, 1, 0),
         FC_ANC28_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_3 == 1 & ANC28_PASS_LATE == 1, 1, 0),
         FC_ANC32_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_4 == 1 & ANC32_PASS_LATE == 1, 1, 0),
         FC_ANC36_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_5 == 1 & ANC36_PASS_LATE == 1, 1, 0)) 

## export 
save(Form_Completion_Anc, file= paste0(path_to_save, "Form_Completion_Anc",".RData",sep = ""))

# 13. ANC form completion (Form_Completion_Pnc) ----
## PNC 
Form_Completion_Pnc <- MatData_Pnc_Visits %>% 
  select(SITE, MOMID, PREGID, ENDPREG_DAYS,contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), 
         contains("_VISIT_COMPLETE_"),contains("_TYPE_VISIT_"),PNC52_LATE,PNC52_ONTIME, BIRTH_OUTCOME_YN,
         M23_CLOSE_DSDECOD) %>%
  filter(!is.na(ENDPREG_DAYS)) %>% ## exclude anyone without a birth outcome (missing ENDPREG_DAYS)
  # DENOMINATOR for form completion
  # step 1. indicator for any type visit = i with any visit status 
  mutate(TYPE_VISIT_ANY_STATUS_7 = ifelse((M06_TYPE_VISIT_7 == 7 & !is.na(M06_VISIT_COMPLETE_7)) |
                                            (M12_TYPE_VISIT_7 == 7 & !is.na(M12_VISIT_COMPLETE_7)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_8 = ifelse((M06_TYPE_VISIT_8 == 8 & !is.na(M06_VISIT_COMPLETE_8)) |
                                            (M12_TYPE_VISIT_8 == 8 & !is.na(M12_VISIT_COMPLETE_8)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_9 = ifelse((M06_TYPE_VISIT_9 == 9 & !is.na(M06_VISIT_COMPLETE_9)) |
                                            (M12_TYPE_VISIT_9 == 9 & !is.na(M12_VISIT_COMPLETE_9)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_10 = ifelse((M05_TYPE_VISIT_10 == 10 & !is.na(M05_VISIT_COMPLETE_10)) |
                                             (M06_TYPE_VISIT_10 == 10 & !is.na(M06_VISIT_COMPLETE_10)) |
                                             (M07_TYPE_VISIT_10 == 10 & !is.na(M07_VISIT_COMPLETE_10)) |
                                             (M08_TYPE_VISIT_10 == 10 & !is.na(M08_VISIT_COMPLETE_10)) |
                                             (M12_TYPE_VISIT_10 == 10 & !is.na(M12_VISIT_COMPLETE_10)) | 
                                             (M25_TYPE_VISIT_10 == 10 & !is.na(M25_VISIT_COMPLETE_10)) | 
                                             (M26_TYPE_VISIT_10 == 10 & !is.na(M26_VISIT_COMPLETE_10)), 1, 0),
         ## might need to add some detail here of who we expect after the 42 day fu (only women who have had a live birth)
         TYPE_VISIT_ANY_STATUS_11 = ifelse((M05_TYPE_VISIT_11 == 11 & !is.na(M05_VISIT_COMPLETE_11)) |
                                             (M06_TYPE_VISIT_11 == 11 & !is.na(M06_VISIT_COMPLETE_11)) |
                                             (M07_TYPE_VISIT_11 == 11 & !is.na(M07_VISIT_COMPLETE_11)) |
                                             (M08_TYPE_VISIT_11 == 11 & !is.na(M08_VISIT_COMPLETE_11)) | 
                                             (M12_TYPE_VISIT_11 == 11 & !is.na(M12_VISIT_COMPLETE_11)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_12 = ifelse((M05_TYPE_VISIT_12 == 12 & !is.na(M05_VISIT_COMPLETE_12)) |
                                             (M06_TYPE_VISIT_12 == 12 & !is.na(M06_VISIT_COMPLETE_12)) |
                                             (M12_TYPE_VISIT_12 == 12 & !is.na(M12_VISIT_COMPLETE_12)), 1, 0)) %>% 
  # step 2. generate denominator 
  mutate(FC_PNC0_DENOM =ifelse(TYPE_VISIT_ANY_STATUS_7 == 1 & PNC0_PASS_LATE == 1, 1, 0),
         FC_PNC1_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_8 == 1 & PNC1_PASS_LATE == 1, 1, 0),
         FC_PNC4_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_9 == 1 & PNC4_PASS_LATE == 1, 1, 0),
         FC_PNC6_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_10 == 1 & PNC6_PASS_LATE == 1, 1, 0),
         FC_PNC26_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_11 == 1 & PNC26_PASS_LATE == 1, 1, 0),
         FC_PNC52_DENOM = ifelse((TYPE_VISIT_ANY_STATUS_12 == 1 & PNC52_PASS_LATE == 1) |
                                  (TYPE_VISIT_ANY_STATUS_12 == 1 & M23_CLOSE_DSDECOD == 1), 1, 0)) 

## export 
save(Form_Completion_Pnc, file= paste0(path_to_save, "Form_Completion_Pnc",".RData",sep = ""))
#**************************************************************************************
#### HEAT MAPS
#* Are forms completed with visit status = 1 or 2 for EACH visit? 

# Figure #: Heat map of form completion for maternal ANC visits in PRISMA
  # output: Figure mentioned above will use Heat_Maps_Pnc

# Figure #: Heat maps of form completion for maternal PNC visits in PRISMA
  # output: Figure mentioned above will use Heat_Maps_Pnc


#* Num: By form: visit type = i AND visit status = 1 or 2 AND passed late window 
#* Denom: Passed window for visit type = i AND didn't closeout AND has not yet delivered
#**************************************************************************************
# 14. ANC heat map (Heat_Maps_Anc) ----

## ANC
Heat_Maps_Anc <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, ENROLL, PREGID, US_GA_WKS_ENROLL, contains("TYPE_VISIT_"), contains("_VISIT_COMPLETE_"),
         contains("_LATE"),ENDPREG_DAYS, M23_CLOSE_DSSTDAT) %>% 
  ## generate denominators - LATE
    # has the late window passed AND has not delivered AND has not closed out
  mutate(HM_ENROLL_DENOM_LATE = ifelse(ENROLL_PASS_LATE == 1, 1, 0), 
         HM_ANC20_DENOM_LATE = ifelse((ANC20_PASS_LATE == 1 & US_GA_WKS_ENROLL <= 17 &
                                        (ENDPREG_DAYS>181 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC20_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),  ## closed out after window or has not yet closed out
         HM_ANC28_DENOM_LATE = ifelse((ANC28_PASS_LATE == 1  &
                                        (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC28_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0), 
         HM_ANC32_DENOM_LATE = ifelse((ANC32_PASS_LATE == 1  & 
                                        (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC32_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0), 
         HM_ANC36_DENOM_LATE = ifelse((ANC36_PASS_LATE == 1  & 
                                        (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS))  & 
                                        ((M23_CLOSE_DSSTDAT > ANC36_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0)
  ) 
## export 
save(Heat_Maps_Anc, file= paste0(path_to_save, "Heat_Maps_Anc",".RData",sep = ""))

# 15. PNC heat map (Heat_Maps_Pnc) ----
# PNC
## i filtered out anyone who has an endpreg and then
# for pnc visits where we do not expect those with miscarriages, i filter 
Heat_Maps_Pnc <- MatData_Pnc_Visits %>%
  filter(!is.na(ENDPREG_DAYS)) %>% ## only include participants with a birth outcome
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("TYPE_VISIT"),
         contains("VISIT_COMPLETE"),
         contains("_PASS"), contains("_ONTIME"), contains("_LATE"), M23_CLOSE_DSSTDAT, M23_CLOSE_DSDECOD, DOB, ENDPREG_DAYS) %>%
  ## generate denominators - LATE
  # has the late window passed AND has not delivered AND has not closed out OR has a form in this visit 
  mutate(HM_PNC0_DENOM_LATE = ifelse((PNC0_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC0_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),
         HM_PNC1_DENOM_LATE = ifelse((PNC1_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC1_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),
         HM_PNC4_DENOM_LATE = ifelse((PNC4_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC4_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),
         HM_PNC6_DENOM_LATE = ifelse((PNC6_PASS_LATE==1 & 
                                       ((M23_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),
         
         HM_PNC26_DENOM_LATE = ifelse((PNC26_PASS_LATE==1 & ENDPREG_DAYS>139 & 
                                        ((M23_CLOSE_DSSTDAT > PNC26_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),
         
         HM_PNC52_DENOM_LATE = ifelse((PNC52_PASS_LATE==1 & ENDPREG_DAYS>139 & is.na(M23_CLOSE_DSSTDAT)) | 
                                        M23_CLOSE_DSDECOD==1, 1, 0)
  )

## export 
save(Heat_Maps_Pnc, file= paste0(path_to_save, "Heat_Maps_Pnc",".RData",sep = ""))

#**************************************************************************************
#* Hemoglobin measurements for participants in ReMAPP per visit 
#* Only looking at those who are enrolled 

# Table #: Protocol compliance for ReMAPP hemoglobin measurements by CBC
  # output: MatData_Hb_Visit (tables mentioned above will use MatData_Hb_Visit as the input)

#* Output = MatData_Hb_VISIT
#* Input = MatData_Anc_Visits, MatData_Pnc_Visits
#**************************************************************************************
# 16. ReMAPP Hb (by visit) (MatData_Hb_Visit) ----

MatData_Hb_Visit <- MatData_Anc_Visits %>% 
  left_join(MatData_Pnc_Visits[c("SITE", "MOMID", "PREGID","PNC6_OVERDUE","PNC6_PASS_LATE", "PNC6_LATE")], 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(mat_enroll %>% select(SITE, MOMID, PREGID, REMAPP_ENROLL, ENROLL_SCRN_DATE), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(HB_COMPLETED_1 = ifelse(M08_CBC_LBPERF_1_1 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_1 == 0, 0, 99)),
         HB_COMPLETED_2 = ifelse(M08_CBC_LBPERF_1_2 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_2 == 0, 0, 99)),
         HB_COMPLETED_3 = ifelse(M08_CBC_LBPERF_1_3 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_3 == 0, 0, 99)),
         HB_COMPLETED_4 = ifelse(M08_CBC_LBPERF_1_4 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_4 == 0, 0, 99)),
         HB_COMPLETED_5 = ifelse(M08_CBC_LBPERF_1_5 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_5 == 0, 0, 99)),
         HB_COMPLETED_10 = ifelse(M08_CBC_LBPERF_1_10 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_10 == 0, 0, 99))
         ) %>% 
  ## replace outliars with NA
  mutate(M08_CBC_HB_LBORRES_1 = ifelse(M08_CBC_HB_LBORRES_1 < 1 | M08_CBC_HB_LBORRES_1 > 20, NA, M08_CBC_HB_LBORRES_1),
         M08_CBC_HB_LBORRES_2 = ifelse(M08_CBC_HB_LBORRES_2 < 1 | M08_CBC_HB_LBORRES_2 > 20, NA, M08_CBC_HB_LBORRES_2),
         M08_CBC_HB_LBORRES_3 = ifelse(M08_CBC_HB_LBORRES_3 < 1 | M08_CBC_HB_LBORRES_3 > 20, NA, M08_CBC_HB_LBORRES_3),
         M08_CBC_HB_LBORRES_4 = ifelse(M08_CBC_HB_LBORRES_4 < 1 | M08_CBC_HB_LBORRES_4 > 20, NA, M08_CBC_HB_LBORRES_4),
         M08_CBC_HB_LBORRES_5 = ifelse(M08_CBC_HB_LBORRES_5 < 1 | M08_CBC_HB_LBORRES_5 > 20, NA, M08_CBC_HB_LBORRES_5),
         M08_CBC_HB_LBORRES_10 = ifelse(M08_CBC_HB_LBORRES_10 < 1 | M08_CBC_HB_LBORRES_10 > 20, NA, M08_CBC_HB_LBORRES_10)) %>%
  ## denominator is n completed MNH08 with visit type=ANC-20 OR passed late window with >25 weeks gestation & has not delivered or dleivered with GA >late visit window AND hasnot close out .
  mutate(DenHBV1 = ifelse((M08_VISIT_COMPLETE_1 == 1 & M08_TYPE_VISIT_1 == 1) |  ENROLL_PASS_LATE== 1, 1, 0),
         DenHBV2 = ifelse((M08_VISIT_COMPLETE_2 == 1 & M08_TYPE_VISIT_2 == 2 & US_GA_WKS_ENROLL <= 17) | (ANC20_PASS_LATE == 1 & 
                            (ENDPREG_DAYS>181 | is.na(ENDPREG_DAYS)) & ((M23_CLOSE_DSSTDAT > ANC20_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0), 
         
         DenHBV3 = ifelse((M08_VISIT_COMPLETE_3 == 1 & M08_TYPE_VISIT_3 == 3) |  (ANC28_PASS_LATE == 1 & (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS)) & 
                            ((M23_CLOSE_DSSTDAT > ANC28_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),
         
         DenHBV4 = ifelse((M08_VISIT_COMPLETE_4 == 1 & M08_TYPE_VISIT_4 == 4) |  (ANC32_PASS_LATE == 1 & (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & 
                            ((M23_CLOSE_DSSTDAT > ANC32_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),
         
         DenHBV5 = ifelse((M08_VISIT_COMPLETE_5 == 1 & M08_TYPE_VISIT_5 == 5) |  (ANC36_PASS_LATE == 1 & (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS))  & 
                            ((M23_CLOSE_DSSTDAT > ANC36_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),
         
         DenHBV10 = ifelse((M08_VISIT_COMPLETE_10 == 1 & M08_TYPE_VISIT_10 == 10) | (PNC6_PASS_LATE==1 & 
                             ((M23_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0)) %>%
  ## add remapp launch date for each site since these are remapp criteria 
  mutate(REMAPP_ENROLL_CMC_FIX = case_when(SITE == "India-CMC" &
                                             ENROLL_SCRN_DATE >= "2023-06-20" & ENROLL_SCRN_DATE <= "2025-07-01" ~ 1, 
                                           TRUE ~ REMAPP_ENROLL)) %>% 
  
  mutate(REMAPP_ENROLL = case_when(SITE != "India-CMC" ~ REMAPP_ENROLL, 
                                          SITE == "India-CMC" ~ REMAPP_ENROLL_CMC_FIX, TRUE ~ REMAPP_ENROLL)) %>% 
  filter(ENROLL == 1 & REMAPP_ENROLL ==1) %>% 
  select(SITE, MOMID, PREGID, ENROLL,ENROLL_DENOM, REMAPP_ENROLL, M02_SCRN_OBSSTDAT,contains("HB_COMPLETED_"), contains("DenHBV"), contains("PASS_LATE"), contains("_LATE"), M23_CLOSE_DSSTDAT, ENDPREG_DAYS)

MatData_Hb_Visit <- MatData_Anc_Visits %>% 
  left_join(MatData_Pnc_Visits[c("SITE", "MOMID", "PREGID","PNC6_OVERDUE","PNC6_PASS_LATE", "PNC6_LATE")], 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(mat_enroll %>% select(SITE, MOMID, PREGID, REMAPP_ENROLL, ENROLL_SCRN_DATE), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(HB_COMPLETED_1 = ifelse(M08_CBC_LBPERF_1_1 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_1 == 0, 0, 99)),
         HB_COMPLETED_2 = ifelse(M08_CBC_LBPERF_1_2 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_2 == 0, 0, 99)),
         HB_COMPLETED_3 = ifelse(M08_CBC_LBPERF_1_3 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_3 == 0, 0, 99)),
         HB_COMPLETED_4 = ifelse(M08_CBC_LBPERF_1_4 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_4 == 0, 0, 99)),
         HB_COMPLETED_5 = ifelse(M08_CBC_LBPERF_1_5 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_5 == 0, 0, 99)),
         HB_COMPLETED_10 = ifelse(M08_CBC_LBPERF_1_10 == 1, 1, 
                                  ifelse(M08_CBC_LBPERF_1_10 == 0, 0, 99))
  ) %>% 
  # replace outliars with NA
  mutate(M08_CBC_HB_LBORRES_1 = ifelse(M08_CBC_HB_LBORRES_1 < 1 | M08_CBC_HB_LBORRES_1 > 20, NA, M08_CBC_HB_LBORRES_1),
         M08_CBC_HB_LBORRES_2 = ifelse(M08_CBC_HB_LBORRES_2 < 1 | M08_CBC_HB_LBORRES_2 > 20, NA, M08_CBC_HB_LBORRES_2),
         M08_CBC_HB_LBORRES_3 = ifelse(M08_CBC_HB_LBORRES_3 < 1 | M08_CBC_HB_LBORRES_3 > 20, NA, M08_CBC_HB_LBORRES_3),
         M08_CBC_HB_LBORRES_4 = ifelse(M08_CBC_HB_LBORRES_4 < 1 | M08_CBC_HB_LBORRES_4 > 20, NA, M08_CBC_HB_LBORRES_4),
         M08_CBC_HB_LBORRES_5 = ifelse(M08_CBC_HB_LBORRES_5 < 1 | M08_CBC_HB_LBORRES_5 > 20, NA, M08_CBC_HB_LBORRES_5),
         M08_CBC_HB_LBORRES_10 = ifelse(M08_CBC_HB_LBORRES_10 < 1 | M08_CBC_HB_LBORRES_10 > 20, NA, M08_CBC_HB_LBORRES_10)) %>%
 # generate new variable if we want to exclude their closeout
  mutate(INCLUDE_CLOSEOUT = case_when(M23_CLOSE_DSDECOD %in% c(4,5,6) ~ 0, ## remove withdraws, ltfu, terminated participation
                                      M23_CLOSE_DSDECOD %in% c(1,2,3) ~ 1,
                                      TRUE ~ M23_CLOSE_DSDECOD)) %>% 
   ## denominator is n completed MNH08 with visit type=AN C-20 OR passed late window with >25 weeks gestation & has not delivered or dleivered with GA >late visit window AND hasnot close out .
  mutate(OLD_DenHBV1 = ifelse((M08_VISIT_COMPLETE_1 == 1 & M08_TYPE_VISIT_1 == 1) |  ENROLL_PASS_LATE== 1, 1, 0),
         OLD_DenHBV2 = ifelse((M08_VISIT_COMPLETE_2 == 1 & M08_TYPE_VISIT_2 == 2 & US_GA_WKS_ENROLL <= 17) | 
                              (ANC20_PASS_LATE == 1 & 
                      (ENDPREG_DAYS>181 | is.na(ENDPREG_DAYS)) & ((M23_CLOSE_DSSTDAT > ANC20_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0), 
         
         OLD_DenHBV3 = ifelse((M08_VISIT_COMPLETE_3 == 1 & M08_TYPE_VISIT_3 == 3) |  (ANC28_PASS_LATE == 1 & (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS)) & 
                                                                                    ((M23_CLOSE_DSSTDAT > ANC28_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),
         
         OLD_DenHBV4 = ifelse((M08_VISIT_COMPLETE_4 == 1 & M08_TYPE_VISIT_4 == 4) |  (ANC32_PASS_LATE == 1 & (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & 
                                                                                    ((M23_CLOSE_DSSTDAT > ANC32_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),
         
         OLD_DenHBV5 = ifelse((M08_VISIT_COMPLETE_5 == 1 & M08_TYPE_VISIT_5 == 5) |  (ANC36_PASS_LATE == 1 & (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS))  & 
                                                                                    ((M23_CLOSE_DSSTDAT > ANC36_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0),
         
         OLD_DenHBV10 = ifelse((M08_VISIT_COMPLETE_10 == 1 & M08_TYPE_VISIT_10 == 10) | (PNC6_PASS_LATE==1 & 
                                                                                       ((M23_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M23_CLOSE_DSSTDAT))), 1, 0)) %>%
  ## update denominators 
  mutate(DenHBV1 = ifelse((M08_VISIT_COMPLETE_1 == 1 & M08_TYPE_VISIT_1 == 1) |  ENROLL_PASS_LATE== 1, 1, 0),
         DenHBV2 = ifelse((M08_VISIT_COMPLETE_2 == 1 & M08_TYPE_VISIT_2 == 2 & US_GA_WKS_ENROLL <= 17) | (ANC20_PASS_LATE == 1 & 
                                           (ENDPREG_DAYS>181 | is.na(ENDPREG_DAYS)) &
                                             ((M23_CLOSE_DSSTDAT > ANC20_LATE & INCLUDE_CLOSEOUT ==1) | is.na(M23_CLOSE_DSSTDAT) | M23_CLOSE_DSDECOD %in% c(4,5,6))), 1, 0), 
         
         DenHBV3 = ifelse((M08_VISIT_COMPLETE_3 == 1 & M08_TYPE_VISIT_3 == 3) |  
                            (ANC28_PASS_LATE == 1 & (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS)) & 
                               ((M23_CLOSE_DSSTDAT > ANC28_LATE & INCLUDE_CLOSEOUT ==1) | is.na(M23_CLOSE_DSSTDAT) | M23_CLOSE_DSDECOD %in% c(4,5,6))), 1, 0),
         
         DenHBV4 = ifelse((M08_VISIT_COMPLETE_4 == 1 & M08_TYPE_VISIT_4 == 4) |  
                            (ANC32_PASS_LATE == 1 & (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & 
                             ((M23_CLOSE_DSSTDAT > ANC32_LATE & INCLUDE_CLOSEOUT ==1) | is.na(M23_CLOSE_DSSTDAT) | M23_CLOSE_DSDECOD %in% c(4,5,6))), 1, 0),
         
         DenHBV5 = ifelse((M08_VISIT_COMPLETE_5 == 1 & M08_TYPE_VISIT_5 == 5) |  
                            (ANC36_PASS_LATE == 1 & (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS))  &
                               ((M23_CLOSE_DSSTDAT > ANC36_LATE & INCLUDE_CLOSEOUT ==1) | is.na(M23_CLOSE_DSSTDAT) | M23_CLOSE_DSDECOD %in% c(4,5,6))), 1, 0),
         
         DenHBV10 = ifelse((M08_VISIT_COMPLETE_10 == 1 & M08_TYPE_VISIT_10 == 10) | (PNC6_PASS_LATE==1 & 
                            ((M23_CLOSE_DSSTDAT > PNC6_LATE & INCLUDE_CLOSEOUT ==1) | is.na(M23_CLOSE_DSSTDAT) | M23_CLOSE_DSDECOD %in% c(4,5,6))), 1, 0)) %>%
  ## add remapp launch date for each site since these are remapp criteria 
  mutate(REMAPP_ENROLL_CMC_FIX = case_when(SITE == "India-CMC" &
                                             ENROLL_SCRN_DATE >= "2023-06-20" & ENROLL_SCRN_DATE <= "2025-07-01" ~ 1, 
                                           TRUE ~ REMAPP_ENROLL)) %>% 
  
  mutate(REMAPP_ENROLL = case_when(SITE != "India-CMC" ~ REMAPP_ENROLL, 
                                   SITE == "India-CMC" ~ REMAPP_ENROLL_CMC_FIX, TRUE ~ REMAPP_ENROLL)) %>% 
  filter(ENROLL == 1 & REMAPP_ENROLL ==1) %>% 
  select(SITE, MOMID, PREGID, ENROLL,ENROLL_DENOM,M23_CLOSE_DSDECOD, INCLUDE_CLOSEOUT,REMAPP_ENROLL, M02_SCRN_OBSSTDAT,contains("HB_COMPLETED_"), contains("DenHBV"), contains("PASS_LATE"), contains("_LATE"), M23_CLOSE_DSSTDAT, ENDPREG_DAYS)



## export 
save(MatData_Hb_Visit, file= paste0(path_to_save, "MatData_Hb_Visit",".RData",sep = ""))
#**************************************************************************************
#* Hemoglobin measures by gestational age for participants enrolled in PRISMA MNH 
#* Only looking at those who are enrolled 
#* Output = MatData_HB_GA_Visit
#* Input = MatData_Anc_Visits

## NOTE: the below code is not currently in use in the monitoring report. 

#* We need: 
  #* GA at each visit 
  #* M08_CBC_HB_LBORRES_1
  #* M08_CBC_HB_LBORRES_1
  #* M06_SPHB_VSSTAT -- Was non-invasive total hemoglobin (SpHb) measured at this visit?
  #* M06_SPHB_LBORRES -- Record non-invasive total hemoglobin (SpHb): . g/dL
  #* M06_HB_POC_LBPERF -- Was point-of-care hemoglobin test performed at this visit?
  #* M06_HB_POC_LBORRES -- Record point-of-care hemoglobin : . g/dL
#**************************************************************************************
# 17. ReMAPP Hb (by visit & GA) (MatData_Hb_GA_Visit) ----

MatData_Anc_Remapp <- MatData_Anc_Visits %>% 
  left_join(mat_enroll %>% select(SITE, MOMID, PREGID, REMAPP_ENROLL, ENROLL_SCRN_DATE), by = c("SITE", "MOMID", "PREGID")) %>% 
  ## add remapp launch date for each site since these are remapp criteria 
  mutate(REMAPP_ENROLL_CMC_FIX = case_when(SITE == "India-CMC" &
                                             ENROLL_SCRN_DATE >= "2023-06-20" & ENROLL_SCRN_DATE <= "2025-07-01" ~ 1, 
                                           TRUE ~ REMAPP_ENROLL)) %>% 
  mutate(REMAPP_ENROLL = case_when(SITE != "India-CMC" ~ REMAPP_ENROLL, 
                                   SITE == "India-CMC" ~ REMAPP_ENROLL_CMC_FIX, TRUE ~ REMAPP_ENROLL)) %>% 
  filter(ENROLL == 1 & REMAPP_ENROLL ==1) 

MatData_Hb_GA_Visit_1 <- MatData_Anc_Remapp %>% 
  select(SITE, MOMID, PREGID, M08_CBC_HB_LBORRES_1,
         M06_SPHB_LBORRES_1,  M06_HB_POC_LBORRES_1, M06_GESTAGE_AT_VISIT_DAYS_1) %>% 
  mutate(TYPE_VISIT = 1) %>% 
  mutate(CBC = replace(M08_CBC_HB_LBORRES_1, M08_CBC_HB_LBORRES_1 == -7, NA ),
         SPHB = replace(M06_SPHB_LBORRES_1, M06_SPHB_LBORRES_1 == -7, NA ),
         POC = replace(M06_HB_POC_LBORRES_1, M06_HB_POC_LBORRES_1 == -7, NA ),
         GA = M06_GESTAGE_AT_VISIT_DAYS_1) %>% 
  select(SITE, MOMID, PREGID,TYPE_VISIT, CBC, SPHB, POC, GA)

MatData_Hb_GA_Visit_2 <- MatData_Anc_Remapp %>% 
  select(SITE, MOMID, PREGID, M08_CBC_HB_LBORRES_2,
         M06_SPHB_LBORRES_2,  M06_HB_POC_LBORRES_2, M06_GESTAGE_AT_VISIT_DAYS_2) %>% 
  mutate(TYPE_VISIT = 2) %>% 
  mutate(CBC = replace(M08_CBC_HB_LBORRES_2, M08_CBC_HB_LBORRES_2 == -7, NA ),
         SPHB = replace(M06_SPHB_LBORRES_2, M06_SPHB_LBORRES_2 == -7, NA ),
         POC = replace(M06_HB_POC_LBORRES_2, M06_HB_POC_LBORRES_2 == -7, NA ),
         GA = M06_GESTAGE_AT_VISIT_DAYS_2) %>% 
  select(SITE, MOMID, PREGID,TYPE_VISIT, CBC, SPHB, POC, GA)

MatData_Hb_GA_Visit_3 <- MatData_Anc_Remapp %>% 
  select(SITE, MOMID, PREGID, M08_CBC_HB_LBORRES_3,
         M06_SPHB_LBORRES_3,  M06_HB_POC_LBORRES_3, M06_GESTAGE_AT_VISIT_DAYS_3) %>% 
  mutate(TYPE_VISIT = 3) %>% 
  mutate(CBC = replace(M08_CBC_HB_LBORRES_3, M08_CBC_HB_LBORRES_3 == -7, NA ),
         SPHB = replace(M06_SPHB_LBORRES_3, M06_SPHB_LBORRES_3 == -7, NA ),
         POC = replace(M06_HB_POC_LBORRES_3, M06_HB_POC_LBORRES_3 == -7, NA ),
         GA = M06_GESTAGE_AT_VISIT_DAYS_3) %>% 
  select(SITE, MOMID, PREGID,TYPE_VISIT, CBC, SPHB, POC, GA)

MatData_Hb_GA_Visit_4 <- MatData_Anc_Remapp %>% 
  select(SITE, MOMID, PREGID, M08_CBC_HB_LBORRES_4,
         M06_SPHB_LBORRES_4,  M06_HB_POC_LBORRES_4, M06_GESTAGE_AT_VISIT_DAYS_4) %>% 
  mutate(TYPE_VISIT = 4) %>% 
  mutate(CBC = replace(M08_CBC_HB_LBORRES_4, M08_CBC_HB_LBORRES_4 == -7, NA ),
         SPHB = replace(M06_SPHB_LBORRES_4, M06_SPHB_LBORRES_4 == -7, NA ),
         POC = replace(M06_HB_POC_LBORRES_4, M06_HB_POC_LBORRES_4 == -7, NA ),
         GA = M06_GESTAGE_AT_VISIT_DAYS_4) %>% 
  select(SITE, MOMID, PREGID,TYPE_VISIT, CBC, SPHB, POC, GA)

MatData_Hb_GA_Visit_5 <- MatData_Anc_Remapp %>% 
  select(SITE, MOMID, PREGID, M08_CBC_HB_LBORRES_5,
         M06_SPHB_LBORRES_5,  M06_HB_POC_LBORRES_5, M06_GESTAGE_AT_VISIT_DAYS_5) %>% 
  mutate(TYPE_VISIT = 5) %>% 
  mutate(CBC = replace(M08_CBC_HB_LBORRES_5, M08_CBC_HB_LBORRES_5 == -7, NA ),
         SPHB = replace(M06_SPHB_LBORRES_5, M06_SPHB_LBORRES_5 == -7, NA ),
         POC = replace(M06_HB_POC_LBORRES_5, M06_HB_POC_LBORRES_5 == -7, NA ),
         GA = M06_GESTAGE_AT_VISIT_DAYS_5) %>% 
  select(SITE, MOMID, PREGID,TYPE_VISIT, CBC, SPHB, POC, GA)

MatData_Hb_GA_Visit = rbind(MatData_Hb_GA_Visit_1, MatData_Hb_GA_Visit_2,MatData_Hb_GA_Visit_3,
                            MatData_Hb_GA_Visit_4,MatData_Hb_GA_Visit_5)

MatData_Hb_GA_Visit$SPHB = ifelse(MatData_Hb_GA_Visit$SPHB == "n/a", NA, as.numeric(MatData_Hb_GA_Visit$SPHB))

MatData_Hb_GA_Visit <- MatData_Hb_GA_Visit %>% 
  pivot_longer(CBC:POC) %>% 
  rename("TEST" = "name",
         "RESULT" = "value") %>%    
  mutate(GA_WKS = floor(GA/7), 
         TRIMESTER = case_when(
           GA_WKS > 0 & GA_WKS <14 ~ 1, 
           GA_WKS >= 14 & GA_WKS <28 ~ 2, 
           GA_WKS >=28 & GA_WKS<42 ~ 3, 
           TRUE ~ NA
         ))

## export 
save(MatData_Hb_GA_Visit, file= paste0(path_to_save, "MatData_Hb_GA_Visit",".RData",sep = ""))
#**************************************************************************************
#*ReMAPP healthy cohort criteria 

# Table #: Participant eligibility for the ReMAPP healthy cohort
  # output: healthyOutcome (tables mentioned above will use healthyOutcome as the input)

# Figure #: Eligibility status and missingness by ReMAPP healthy cohort criteria
  # output: df_eli_long (figure mentioned above will use df_eli_long as the input)

#*Output: healthyOutcome.rda
 #*includes: CRIT_s, HEALTHY_ELIGIBLE
#**************************************************************************************
# 18. ReMAPP Healthy Cohort (healthyOutcome) ----

## 18.1 import kenya g6pd data ----
## kenya g6pd variables 
m08_g6pd_ke <- read.csv(paste0("~/import/", UploadDate, "_ke/mnh08.csv")) %>% select(MOMID, PREGID, TYPE_VISIT, RBC_G6PD_LBORRES_Interpret) %>% 
  rename(M08_TYPE_VISIT  = TYPE_VISIT) %>% 
  filter(RBC_G6PD_LBORRES_Interpret %in% c(1,2,3))

mnh08_g6pd  <- read.csv(paste0("D:/Users/stacie.loisate/Documents/import/", UploadDate, "/mnh08_merged.csv")) %>%
  select(SITE, MOMID, PREGID, M08_TYPE_VISIT,M08_LBSTDAT, contains("G6PD")) %>%
  left_join(m08_g6pd_ke, by = c("MOMID", "PREGID", "M08_TYPE_VISIT")) %>% 
  mutate(UPDATED_TYPE_VISIT = case_when(SITE == "Zambia" & !M08_TYPE_VISIT %in% c(1,2,3,4,5,13) & M08_RBC_G6PD_LBORRES >= 0 ~ 13, 
                                     TRUE ~ M08_TYPE_VISIT)) %>% 
  filter((M08_RBC_G6PD_LBORRES >= 0 | RBC_G6PD_LBORRES_Interpret %in% c(1,2,3)) & UPDATED_TYPE_VISIT %in% c(1,2,3,4,5,13)) %>%
  mutate(M08_RBC_G6PD_LBSTDAT = as.Date(M08_RBC_G6PD_LBSTDAT, "%d-%b-%y"),
         M08_RBC_G6PD_LBTSTDAT = case_when(!is.na(M08_RBC_G6PD_LBSTDAT) ~ ymd(M08_RBC_G6PD_LBSTDAT),
                                           TRUE ~ ymd(M08_RBC_G6PD_LBTSTDAT))
  ) %>% 
  group_by(SITE, PREGID) %>% 
  mutate(n=n()) %>% 
  group_by(SITE, PREGID) %>%
  arrange(desc(M08_RBC_G6PD_LBTSTDAT)) %>%
  slice(1) %>%
  ungroup() %>% 
  select(-n, -M08_RBC_G6PD_LBSTDAT) %>% 
  rename(G6PD_TYPE_VISIT = M08_TYPE_VISIT)


MatData_Report_out <- MatData_Report %>%
  left_join(MatData_Screen_Enroll %>% select(SITE, MOMID, PREGID,ENDPREG_DAYS,MISCARRIAGE,SCREEN, ENROLL, ENROLL_DENOM), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(PREG_START_DATE  = ymd(PREG_START_DATE)) 


df_maternal <- MatData_Report_out %>%
  filter(ENROLL == 1) %>% 
  left_join(mat_enroll %>% select(SITE, MOMID, PREGID, REMAPP_ENROLL, ENROLL_SCRN_DATE), by = c("SITE", "MOMID", "PREGID")) %>% 
  ## add remapp launch date for each site since these are remapp criteria 
  mutate(REMAPP_ENROLL_CMC_FIX = case_when(SITE == "India-CMC" &
                                             ENROLL_SCRN_DATE >= "2023-06-20" & ENROLL_SCRN_DATE <= "2025-07-01" ~ 1, 
                                           TRUE ~ REMAPP_ENROLL)) %>% 
  mutate(REMAPP_ENROLL = case_when(SITE != "India-CMC" ~ REMAPP_ENROLL, 
                                   SITE == "India-CMC" ~ REMAPP_ENROLL_CMC_FIX, TRUE ~ REMAPP_ENROLL)) %>% 
  filter(REMAPP_ENROLL == 1) %>% 
  mutate(FERRITIN_LBORRES  = M08_FERRITIN_LBORRES_1)

## 18.2 merge in true missing data (site reported) ----
## pull df of IDs that are "true missing"
pak_c9_smoke_response <- read_xlsx("Z:/ReMAPP Healthy Cohort/Healthy_Cohort_Missingness_Responses/2025-09-05-Pakistan_ALL-Pending-Healthy-Cohort_response.xlsx",
                         sheet = "C9") %>% 
  mutate(c9_smoke_true_missing = case_when(c9_true_missing ==1 ~ 1, TRUE ~ 0)) %>% 
  select(MOMID, PREGID, c9_smoke_true_missing)

pak_c19_hemo_response <- read_xlsx("Z:/ReMAPP Healthy Cohort/Healthy_Cohort_Missingness_Responses/2025-09-05-Pakistan_ALL-Pending-Healthy-Cohort_response.xlsx",
                          sheet = "C19") %>% 
  mutate(c19_hemo_true_missing = case_when(c19_true_missing ==1 ~ 1, TRUE ~ 0)) %>% 
  select(MOMID, PREGID, c19_hemo_true_missing)

pak_c20_g6pd_response <- read_xlsx("Z:/ReMAPP Healthy Cohort/Healthy_Cohort_Missingness_Responses/2025-09-05-Pakistan_ALL-Pending-Healthy-Cohort_response.xlsx",
                          sheet = "C20") %>% 
  mutate(c20_g6pd_true_missing = case_when(c20_true_missing ==1 ~ 1, TRUE ~ 0)) %>% 
  select(MOMID, PREGID, c20_g6pd_true_missing)

cmc_response <- read_xlsx("Z:/ReMAPP Healthy Cohort/Healthy_Cohort_Missingness_Responses/2026-01-23-CMC_ALL-Pending-Healthy-Cohort_response.xlsx") %>% 
  select(MOMID, PREGID, contains("true_missing"))

true_missing_ids <- df_maternal %>%
  left_join(pak_c9_smoke_response, by = c("MOMID", "PREGID")) %>% 
  left_join(pak_c19_hemo_response, by = c("MOMID", "PREGID")) %>% 
  left_join(pak_c20_g6pd_response, by = c("MOMID", "PREGID")) %>% 
  left_join(cmc_response, by = c("MOMID", "PREGID")) %>%
  select(SITE, MOMID, PREGID, contains("true_missing"))

## 18.3 adjusting ferritin units ----
p50_ferritin_gh  <- median(df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Ghana"], na.rm = TRUE)
p50_ferritin_cmc  <- median(df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "India-CMC"], na.rm = TRUE)
p50_ferritin_sas  <- median(df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "India-SAS"], na.rm = TRUE)
p50_ferritin_ky  <- median(df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Kenya"], na.rm = TRUE)
p50_ferritin_pk  <- median(df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Pakistan"], na.rm = TRUE)
p50_ferritin_zm  <- median(df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Zambia"], na.rm = TRUE)

if (p50_ferritin_gh < 10 ) {
  df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "Ghana"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Ghana"] * 10
} else  { df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "Ghana"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Ghana"]
}


if (p50_ferritin_cmc < 10 ) {
  df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "India-CMC"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "India-CMC"] * 10
} else  { df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "India-CMC"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "India-CMC"]
}


if (p50_ferritin_sas < 10 ) {
  df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "India-SAS"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "India-SAS"] * 10
} else  { df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "India-SAS"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "India-SAS"]
}


if (p50_ferritin_ky < 15 ) {
  df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "Kenya"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Kenya"] * 10
} else  { df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "Kenya"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Kenya"]
}


if (p50_ferritin_pk < 10 ) {
  df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "Pakistan"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Pakistan"] * 10
} else  { df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "Pakistan"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Pakistan"]
}


if (p50_ferritin_zm < 10 ) {
  df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "Zambia"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Zambia"] * 10
} else  { df_maternal$FERRITIN_LBORRES[df_maternal$SITE == "Zambia"] <- df_maternal$M08_FERRITIN_LBORRES_1[df_maternal$SITE == "Zambia"]
}


## 18.4 assign healthy cohort criteria ----
#derive criteria
  #derive criteria
df_criteria <- df_maternal %>%
  left_join(mnh08_g6pd, by = c("SITE", "MOMID", "PREGID")) %>% 
  dplyr::select(
    SITE, SCRNID, MOMID, PREGID,ENROLL,REMAPP_ENROLL,
    M00_KNOWN_DOBYN_SCORRES, M00_BRTHDAT, M00_ESTIMATED_AGE, M00_SCHOOL_YRS_SCORRES, M00_SCHOOL_SCORRES,
    M02_SCRN_OBSSTDAT,
    M03_MARITAL_SCORRES_1, M03_SMOKE_OECOCCUR_1, M03_CHEW_BNUT_OECOCCUR_1, M03_CHEW_OECOCCUR_1, M03_DRINK_OECOCCUR_1,
    M04_PRETERM_RPORRES_1, M04_PH_PREV_RPORRES_1, M04_PH_PREVN_RPORRES_1, M04_PH_LIVE_RPORRES_1, 
    M04_MISCARRIAGE_RPORRES_1, M04_MISCARRIAGE_CT_RPORRES_1, M04_PH_OTH_RPORRES_1,M04_STILLBIRTH_RPORRES_1,
    M04_LOWBIRTHWT_RPORRES_1, M04_MALARIA_EVER_MHOCCUR_1, 
    M04_CANCER_EVER_MHOCCUR_1, M04_KIDNEY_EVER_MHOCCUR_1, M04_CARDIAC_EVER_MHOCCUR_1,
    M04_HIV_MHOCCUR_1, M04_HIV_EVER_MHOCCUR_1, M04_UNPL_CESARIAN_PROCCUR_1, M04_PREECLAMPSIA_RPORRES_1,
    M04_GEST_DIAB_RPORRES_1, M04_PREMATURE_RUPTURE_RPORRES_1,
    M04_MACROSOMIA_RPORRES_1, M04_OLIGOHYDRAMNIOS_RPORRES_1,
    M04_APH_RPORRES_1, M04_PPH_RPORRES_1,M05_ANT_PEDAT_1, M05_WEIGHT_PERES_1, M05_HEIGHT_PERES_1, M05_MUAC_PERES_1,
    M06_SINGLETON_PERES_1, 
    M06_BP_SYS_VSORRES_1_1, M06_BP_SYS_VSORRES_2_1, M06_BP_SYS_VSORRES_3_1,
    M06_BP_DIA_VSORRES_1_1, M06_BP_DIA_VSORRES_2_1, M06_BP_DIA_VSORRES_3_1,
    M06_MALARIA_POC_LBORRES_1, M06_MALARIA_POC_LBPERF_1, 
    M06_HBV_POC_LBORRES_1, M06_HBV_POC_LBPERF_1, M06_HCV_POC_LBORRES_1, M06_HCV_POC_LBPERF_1,
    M06_HIV_POC_LBORRES_1, M06_HIV_POC_LBPERF_1,M06_HB_POC_LBORRES_1,
    M08_MN_LBPERF_8_1, M08_FERRITIN_LBORRES_1, 
    M08_RBC_LBPERF_2_1, M08_RBC_THALA_LBORRES_1, M08_RBC_LBPERF_3_1,
    M08_MN_LBPERF_12_1, M08_CRP_LBORRES_1, M08_MN_LBPERF_13_1, M08_AGP_LBORRES_1,
    M08_RBC_G6PD_LBORRES_1,M08_RBC_G6PD_LBORRES, RBC_G6PD_LBORRES_Interpret, FERRITIN_LBORRES,
    contains("M08_RBC_THALA_"),M08_RBC_SPFY_THALA_1,M08_RBC_SICKLE_LBORRES_1,M08_MAT_VISIT_MNH08_1,
    num_range("M06_HB_POC_LBORRES_",1:12),
    num_range("M08_CBC_HB_LBORRES_",1:12),
    num_range("M08_LBSTDAT_",1:12),
    num_range("M09_INFANTID_INF",1:4,"_6"),
    num_range("M09_INFANTID_INF",1:4,"_6"),
    num_range("M09_BIRTH_DSTERM_INF",1:4,"_6"), 
    num_range("M09_DELIV_DSSTDAT_INF",1:4,"_6"), BOE_GA_DAYS_ENROLL

  ) %>% 
  left_join(true_missing_ids, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(
    # A. age at enrollment
    # Aged 18 to 34 years
    AGE_ENROLL = case_when(!M02_SCRN_OBSSTDAT %in% c(ymd("1907-07-07"), ymd("1905-05-05")) & !M00_BRTHDAT %in% c(ymd("1907-07-07"), ymd("1905-05-05"))~ 
                        as.numeric(ymd(M02_SCRN_OBSSTDAT) - ymd(M00_BRTHDAT))/365,
                    M00_ESTIMATED_AGE > 0 ~ M00_ESTIMATED_AGE,
                    TRUE ~ NA),
    
    CRIT_AGE = ifelse((AGE_ENROLL > 0 & AGE_ENROLL < 18) | AGE_ENROLL > 34, 0,
                      ifelse(AGE_ENROLL >= 18 & AGE_ENROLL <= 34, 1, 55)
    ),
    # B. GA at enrollment
    # gestational age at enrollment - Gestational age <14 weeks 
    BASELINE_GA_WKS = floor(BOE_GA_DAYS_ENROLL/7),
    CRIT_GA = ifelse(BASELINE_GA_WKS > 0 & BASELINE_GA_WKS < 14, 1,
                     ifelse(BASELINE_GA_WKS >= 14 & BASELINE_GA_WKS <=26, 0, 55)),
    
    # C. Pre-pregnancy or early pregnancy body mass index (BMI) of >18.5 and <30 kg/m2 AND mid-upper arm circumference (MUAC) > 23cm [45]
    # BMI
    BMI = case_when(
      M05_WEIGHT_PERES_1 > 0 & M05_HEIGHT_PERES_1 > 0 ~  M05_WEIGHT_PERES_1 / M05_HEIGHT_PERES_1 / M05_HEIGHT_PERES_1 * 10000, 
      TRUE ~ 55
    ),
    
    TEMP_BMI = ifelse(BMI <= 18.5 | BMI >= 30, 0, 
                      ifelse(BMI > 18.5 & BMI < 30, 1, 55)
    ),
    # MUAC mid-upper arm circumference - MUAC
    TEMP_MUAC = ifelse(M05_MUAC_PERES_1 > 0 & M05_MUAC_PERES_1 <= 23, 0, 
                       ifelse(M05_MUAC_PERES_1 > 23, 1, 55)
    ),
    CRIT_BMI_MUAC = case_when(
      TEMP_BMI == 1 & TEMP_MUAC == 1 ~ 1, 
      TEMP_BMI == 0 | TEMP_MUAC == 0 ~ 0, 
      TRUE ~ 55
    ),
    # D. Height b	%150 cm
    CRIT_HEIGHT = ifelse(M05_HEIGHT_PERES_1 > 0 & M05_HEIGHT_PERES_1 < 150, 0,
                         ifelse(M05_HEIGHT_PERES_1 >= 150, 1, 55)
    ),
    # E. Singleton pregnancy
    CRIT_SINGLEPREG = ifelse(M06_SINGLETON_PERES_1 == 0, 0,
                             ifelse(M06_SINGLETON_PERES_1 == 1, 1, 55)
    )) %>% 
  # F. no iron deficiency (not iron deficient: serum ferritin > 15 mcg/L(Ug/L)) 
  #convert unit from ug/dL to mcg/L
  #replace negative value to NA in order to use BRINDA package 
  mutate_at(vars(c(M08_FERRITIN_LBORRES_1, M08_CRP_LBORRES_1, M08_AGP_LBORRES_1, FERRITIN_LBORRES)), ~ replace(., . < 0, NA)) 
  # #!!! temp: check unit before run
  # mutate(
  #   FERRITIN_LBORRES = case_when(
  #     SITE %in% c("Ghana", "India-CMC", "Kenya") ~ 10*M08_FERRITIN_LBORRES_1, 
  #     TRUE ~ M08_FERRITIN_LBORRES_1 
  #   )) 

df_criteria <- df_criteria %>% 
  mutate(
  ## ask savannah to double check:
    # G. no subclinical inflammation (CRP<=5 and/or AGP<=1) check unit (mg/L for CRP and g/L for AGP in dd) double check the calculation before use
    CRIT_INFLAM = case_when(
      SITE == "India-CMC" & !M08_MAT_VISIT_MNH08_1 %in% c(1,2) ~ 0, ## true missing for refusals in cmc 
      (M08_CRP_LBORRES_1>5 & !is.na(M08_CRP_LBORRES_1)) | (M08_AGP_LBORRES_1>1 & !is.na(M08_AGP_LBORRES_1)) ~ 0, ## if high inflammation --> 0, inelgible
      M08_CRP_LBORRES_1 >= 0 & M08_CRP_LBORRES_1 <= 5 & M08_AGP_LBORRES_1 > 0 & M08_AGP_LBORRES_1 <= 1 ~ 1, ## normal inflammation --> 1, eligible
      M08_MN_LBPERF_12_1 == 0 | M08_MN_LBPERF_13_1 == 0 ~ 55,
      TRUE ~ 55
    ),

    # F. no iron deficiency (not iron deficient: serum ferritin > 15 mcg/L(Ug/L)) 
    CRIT_IRON = case_when(
      SITE == "India-CMC" & !M08_MAT_VISIT_MNH08_1 %in% c(1,2) ~ 0, ## true missing for refusals in cmc 
      (CRIT_INFLAM ==0 & FERRITIN_LBORRES <70) | (CRIT_INFLAM == 1 & FERRITIN_LBORRES<15) ~ 0, ## 0, ineligible if high inflammation & ferritin <70 OR normal inflammation & ferritin <15
      (CRIT_INFLAM ==0 & FERRITIN_LBORRES >=70) | (CRIT_INFLAM == 1 & FERRITIN_LBORRES>=15) ~ 1, ## 1, eligible
      CRIT_INFLAM == 55 ~  55, ## if inflammation status is pending, 55, pending
      TRUE ~ NA_real_
    )
  ) %>% 
  rowwise() %>% 
  mutate_at(vars(starts_with("M06_BP_")), ~ ifelse(. < 0, NA, .)) %>% 
  mutate(
    # H.a. blood pressure
    M06_BP_SYS_1 = mean(c(M06_BP_SYS_VSORRES_1_1, M06_BP_SYS_VSORRES_2_1, M06_BP_SYS_VSORRES_3_1), na.rm = TRUE),
    M06_BP_DIA_1 = mean(c(M06_BP_DIA_VSORRES_1_1, M06_BP_DIA_VSORRES_2_1, M06_BP_DIA_VSORRES_3_1), na.rm = TRUE),
    
    CRIT_BP = ifelse(M06_BP_SYS_1 > 0 & M06_BP_SYS_1 < 140 & M06_BP_DIA_1 > 0 & M06_BP_DIA_1 < 90, 1,
                     ifelse(M06_BP_SYS_1 >= 140 | M06_BP_DIA_1 >= 90, 0, 55)
    )) %>% 
  ungroup() %>% 
  mutate(
    # H.b. no previous low birth weight delivery
    CRIT_LBW = ifelse(M04_LOWBIRTHWT_RPORRES_1 == 1, 0,
                      ifelse(M04_PH_PREV_RPORRES_1 == 0 | M04_LOWBIRTHWT_RPORRES_1 == 0, 1,
                             ifelse(M04_LOWBIRTHWT_RPORRES_1 == 99, 0, 55))
    ),
    # H.c. No previous reported stillbirth
    CRIT_STILLBIRTH = ifelse(M04_STILLBIRTH_RPORRES_1 == 1, 0, #stillbirth,
                             ifelse(M04_PH_PREV_RPORRES_1 == 0 | 
                                      M04_PH_OTH_RPORRES_1 == 0 | #no fetal loss
                                      M04_STILLBIRTH_RPORRES_1 == 0, 1,
                                    ifelse(M04_STILLBIRTH_RPORRES_1 == 99, 0, 55))  
    ),
    # H.d. No previous reported unplanned cesarean delivery
    CRIT_UNPL_CESARIAN = case_when(
      M04_UNPL_CESARIAN_PROCCUR_1 == 1 ~ 0, 
      M04_PH_PREV_RPORRES_1 == 0 | M04_UNPL_CESARIAN_PROCCUR_1 == 0 ~ 1,
      M04_UNPL_CESARIAN_PROCCUR_1 == 99 ~ 0,
      TRUE ~ 55 
    ),
    # I. Normal glucose-6-phosphate dehydrogenase (%6.1 U/g Hb) [UPDATED 5/20 TO INCLUDE ALL VISITS AND NOT JUST ENROLLMENT]
    CRIT_G6PD = case_when(
      c20_g6pd_true_missing ==1 ~ 0, ## true missing as reported by sites will be set to "not eligible" in the monitoring report 
      SITE == "India-CMC" & !M08_MAT_VISIT_MNH08_1 %in% c(1,2) ~ 0, ## true missing for refusals in cmc 
      SITE == "Kenya" & RBC_G6PD_LBORRES_Interpret ==1  ~ 1,
      SITE == "Kenya" & RBC_G6PD_LBORRES_Interpret %in% c(2,3) ~ 0,
      M08_RBC_G6PD_LBORRES == 77 | RBC_G6PD_LBORRES_Interpret == 77 ~ 55, #temp solution for wrong default value 77. 
      M08_RBC_G6PD_LBORRES >= 6.1 ~ 1,
      (M08_RBC_G6PD_LBORRES >= 0 & M08_RBC_G6PD_LBORRES < 6.1)~ 0, 
      TRUE ~ 55
    ),
    # K. No reported cigarette smoking, tobacco chewing, or betel nut use during pregnancy
    CRIT_SMOKE = case_when(
      c9_smoke_true_missing ==1 ~ 0,
      SITE == "Zambia" & (M03_SMOKE_OECOCCUR_1 == 1 | M03_CHEW_OECOCCUR_1 == 1) ~ 0,
      SITE == "Zambia" & (M03_SMOKE_OECOCCUR_1 == 0 & M03_CHEW_OECOCCUR_1 == 0) ~ 1,
      M03_SMOKE_OECOCCUR_1 == 1 | M03_CHEW_BNUT_OECOCCUR_1 == 1 | M03_CHEW_OECOCCUR_1 == 1 ~ 0,
      M03_SMOKE_OECOCCUR_1 == 0 & M03_CHEW_BNUT_OECOCCUR_1 == 0 & M03_CHEW_OECOCCUR_1 == 0 ~ 1,
      TRUE ~ 55
    ),
    #L. No reported alcohol consumption during pregnancy
    CRIT_DRINK = ifelse(SITE == "Pakistan", 666,
                        ifelse(M03_DRINK_OECOCCUR_1 == 1, 0,
                               ifelse(M03_DRINK_OECOCCUR_1 == 0, 1,
                                      ifelse(M03_DRINK_OECOCCUR_1 == 66, 0,
                                             ifelse(M03_DRINK_OECOCCUR_1 == 77, 0, 55)))) 
    ), 
    #M. No known history or current chronic disease including cancer, kidney disease, and cardiac conditions
    CRIT_CHRONIC = ifelse(M04_CANCER_EVER_MHOCCUR_1 == 1 | M04_KIDNEY_EVER_MHOCCUR_1 == 1 | 
                            M04_CARDIAC_EVER_MHOCCUR_1 == 1, 0,
                          ifelse(M04_CANCER_EVER_MHOCCUR_1 == 0 & M04_KIDNEY_EVER_MHOCCUR_1 == 0 & 
                                   M04_CARDIAC_EVER_MHOCCUR_1 == 0, 1,
                                 ifelse(M04_CANCER_EVER_MHOCCUR_1 == 99 | M04_KIDNEY_EVER_MHOCCUR_1 == 99 | 
                                          M04_CARDIAC_EVER_MHOCCUR_1 == 99, 0, 55))
    ),
    #N. No known history or current HIV
    # if "Record HIV results" = positive, then CRIT_HIV=0 (ineligible) [M06_HIV_POC_LBORRES_1]
    CRIT_HIV = ifelse(M06_HIV_POC_LBORRES_1 == 1, 0, 
                      # if "Record HIV results" = negative, then CRIT_HIV=1 (eligible) [M06_HIV_POC_LBORRES_1]
                      ifelse(M06_HIV_POC_LBORRES_1 == 0, 1,
                             # if "Record HIV results" = 55, then CRIT_HIV=55 (pending) [M06_HIV_POC_LBORRES_1]
                             ifelse(M06_HIV_POC_LBORRES_1 == 55, 55,  
                                    # if "Have you ever been diagnosed with HIV?" = yes OR 
                                    # if "Have you had any of the following issues since becoming pregnant with the current pregnancy, HIV" = yes, then CRIT_HIV=0 (ineligible)
                                    ifelse(M04_HIV_EVER_MHOCCUR_1 == 1 |  M04_HIV_MHOCCUR_1 == 1, 0,
                                           # if "Have you ever been diagnosed with HIV?" = no AND 
                                           # if "Have you had any of the following issues since becoming pregnant with the current pregnancy, HIV" = no, then CRIT_HIV=1 (eligible)
                                           ifelse(M04_HIV_EVER_MHOCCUR_1 == 0 & M04_HIV_MHOCCUR_1 == 0, 1,
                                                  # if "Have you ever been diagnosed with HIV?" = 55, then CRIT_HIV=55 (pending) OR
                                                  # if "Have you had any of the following issues since becoming pregnant with the current pregnancy, HIV" = 55, then CRIT_HIV=55 (pending)
                                                  ifelse(M04_HIV_EVER_MHOCCUR_1 == 55 | M04_HIV_MHOCCUR_1 == 55, 55,  
                                                         # if "Have you ever been diagnosed with HIV?" = 77/99 AND 
                                                         # if "Have you had any of the following issues since becoming pregnant with the current pregnancy, HIV" = 77/99, then CRIT_HIV=55 (pending)
                                                         ifelse(M04_HIV_EVER_MHOCCUR_1 %in% c(0,99) & M04_HIV_MHOCCUR_1 %in% c(0,99), 0, 55)))))) 
    ),
    #O. No current malaria infection (per rapid diagnostic test)
    CRIT_MALARIA = case_when(
      M06_MALARIA_POC_LBORRES_1 == 1 ~ 0,
      M06_MALARIA_POC_LBORRES_1 == 0 | (SITE %in% c("India-CMC", "India-SAS", "Pakistan") & M06_MALARIA_POC_LBORRES_1 == 77)~ 1,
      TRUE ~ 55
    ),
    #P. No current Hepatitis B virus infection (per rapid diagnostic test)
    CRIT_HEPATITISB = ifelse(M06_HBV_POC_LBORRES_1 == 1, 0,
                             ifelse(M06_HBV_POC_LBORRES_1 == 0, 1, 55)
    ),
    #Q No current Hepatitis C virus infection (per rapid diagnostic test)
    CRIT_HEPATITISC = ifelse(M06_HCV_POC_LBORRES_1 == 1, 0,
                             ifelse(M06_HCV_POC_LBORRES_1 == 0, 1, 55))
  ) 

## 18.5 hemoglobinopathies coding ----
## RBC MORPHOLOGY
mnh08_raw  <- read.csv(paste0("D:/Users/stacie.loisate/Documents/import/", UploadDate, "/mnh08_merged.csv")) %>%
  select(SITE, MOMID, PREGID, M08_TYPE_VISIT,M08_LBSTDAT, contains("RBC"))
# mnh08_raw  <- m08_merged %>% 
#   select(SITE, MOMID, PREGID, M08_TYPE_VISIT,M08_LBSTDAT, contains("RBC")) 

#RBC Morphology 
rbc_morph_raw  <- mnh08_raw  %>% 
  select(SITE, MOMID, PREGID, M08_RBC_LBPERF_1, M08_RBC_LBPERF_2, M08_RBC_THALA_LBORRES, starts_with("M08_RBC_THALA"), 
         M08_RBC_SICKLE_LBORRES, M08_RBC_SPFY_THALA, M08_LBSTDAT, M08_TYPE_VISIT) %>%
  filter (M08_RBC_LBPERF_1 == 1 | M08_RBC_LBPERF_2 == 1) %>%
  mutate (
    # J. No hemoglobinopathies: SS, SC, SE, EE, CC, SD-Punjab, SN2thal, EN2thal, 
    #CN2thal, CD-Punjab, ED-Punjab, D-D-Punjab, D-PunjabN2thal, Thalassemia major, Thalassemia intermedia, or Alpha thalassemia
    CRIT_HEMOGLOBINOPATHIES = case_when(
      # Case 1: Any of the M08_RBC_THALA_x variables is 1 OR grepl() condition is met
      (M08_RBC_THALA_1 == 1 | M08_RBC_THALA_2 == 1 | M08_RBC_THALA_3 == 1 | M08_RBC_THALA_4 == 1 |
         M08_RBC_THALA_5 == 1 | M08_RBC_THALA_6 == 1 | M08_RBC_THALA_7 == 1 | M08_RBC_THALA_8 == 1 |
         M08_RBC_THALA_9 == 1 | M08_RBC_THALA_10 == 1 | M08_RBC_THALA_11 == 1 | M08_RBC_THALA_12 == 1 |
         M08_RBC_THALA_13 == 1 | M08_RBC_THALA_14 == 1 ) ~ 0,
      
      # Case 2: grepl condition with M08_RBC_THALA_19
      (grepl("Interme|Diseas|Major|HbD Punjab", M08_RBC_SPFY_THALA, ignore.case = TRUE) & M08_RBC_THALA_19 == 1) ~ 0,
      
      # Case 3: If M08_RBC_SICKLE_LBORRES is 1, assign 0
      M08_RBC_SICKLE_LBORRES == 1 ~ 0,
      
      # Case 4: All M08_RBC_THALA_x are 0, but M08_RBC_THALA_16, 17, or 18 is 1 OR thala test results are 0
      ((M08_RBC_THALA_1 %in% c(0,77) & M08_RBC_THALA_2 %in% c(0,77) & M08_RBC_THALA_3 %in% c(0,77) & M08_RBC_THALA_4 %in% c(0,77) &
          M08_RBC_THALA_5 %in% c(0,77) & M08_RBC_THALA_6 %in% c(0,77) & M08_RBC_THALA_7 %in% c(0,77) & M08_RBC_THALA_8 %in% c(0,77) &
          M08_RBC_THALA_9 %in% c(0,77) & M08_RBC_THALA_10 %in% c(0,77) & M08_RBC_THALA_11 %in% c(0,77) & M08_RBC_THALA_12 %in% c(0,77) &
          M08_RBC_THALA_13 %in% c(0,77) & M08_RBC_THALA_14 %in% c(0,77)) & 
         (M08_RBC_THALA_15 == 1 | M08_RBC_THALA_16 == 1 | M08_RBC_THALA_17 == 1 | M08_RBC_THALA_18 == 1)) |
        M08_RBC_THALA_LBORRES == 0 ~ 1,
      
      # Case 5: grepl condition with M08_RBC_THALA_19 if it has trait/any regular hemoglobanopathy without disease
      (grepl("TRAIT|AF|FC|AE|AS|HbG|Normal|HbJ|HbF|", M08_RBC_SPFY_THALA, ignore.case = TRUE) & M08_RBC_THALA_19 == 1) ~ 1,
      
      
      # Default case
      TRUE ~ 55
    ))

# Step 2: Remove duplicates (keep one row per participant)
# (Assume: no specific date available ??? pick the record where CRIT_HEMOGLOBINOPATHIES is not 55 first if possible)
rbc_morph_criteria <- rbc_morph_raw %>%
  group_by(SITE, MOMID, PREGID) %>%
  arrange(CRIT_HEMOGLOBINOPATHIES) %>%  # Prioritize 0/1 over 55
  slice(1) %>%
  ungroup()

df_criteria <- df_criteria %>% 
  left_join(rbc_morph_criteria %>% select(SITE, MOMID, PREGID, CRIT_HEMOGLOBINOPATHIES), by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(CRIT_HEMOGLOBINOPATHIES = case_when(c19_hemo_true_missing ==1 ~ 0,
                                             SITE == "India-CMC" & !M08_MAT_VISIT_MNH08_1 %in% c(1,2) ~ 0, ## true missing for refusals in cmc 
                                             CRIT_HEMOGLOBINOPATHIES ==1 ~ 1, 
                                             CRIT_HEMOGLOBINOPATHIES ==0 ~ 0,
                                             is.na(CRIT_HEMOGLOBINOPATHIES) | CRIT_HEMOGLOBINOPATHIES ==55 ~ 55,
                                              TRUE ~ NA)) 

## flag if any are NA
table(df_criteria$CRIT_HEMOGLOBINOPATHIES, useNA = "ifany")


## 18.6 confirm additional true missing (site reported) ----
df_criteria <- df_criteria %>%
  mutate(CRIT_HEPATITISB = case_when(c15_true_missing ==1 ~ 0, TRUE ~ CRIT_HEPATITISB),
         CRIT_HEPATITISC = case_when(c16_true_missing ==1 ~ 0, TRUE ~ CRIT_HEPATITISC),
         CRIT_IRON = case_when(c17_true_missing ==1 ~ 0, TRUE ~ CRIT_IRON),
         CRIT_INFLAM = case_when(c18_true_missing ==1 ~ 0, TRUE ~ CRIT_INFLAM),
         CRIT_HEMOGLOBINOPATHIES = case_when(c19_true_missing ==1 ~ 0, TRUE ~ CRIT_HEMOGLOBINOPATHIES),
         CRIT_G6PD = case_when(c20_true_missing ==1 ~ 0, TRUE ~ CRIT_G6PD)
         ) %>% 
  # update the 1 kenya id 
  mutate(across(.cols = starts_with("CRIT_"), 
                .fns = ~ if_else(PREGID == "KEARC00300_P1" & .x == 55, 0, .x)))

save(df_criteria, file= paste0(path_to_save, "df_criteria",".RData",sep = ""))
#**************************************************************************************
#*2. check eligibility and save df_healthy.rda
#**************************************************************************************
## 18.7 assign final eligibility indicator ----
#code 666 for any not applicable by site
healthyOutcome <- df_criteria %>% 
  rowwise() %>%
  mutate(HEALTHY_CHECK = sum(across(starts_with("CRIT_"), ~ .x %in% c(1, 0, 666)), na.rm = TRUE)) %>% 
  mutate(HEALTHY_ELIGIBLE = case_when(
    if_all(starts_with("CRIT_"), ~.x %in% c(1, 666)) ~ 1, #eligible
    if_any(starts_with("CRIT_"), ~.x == 0) ~ 0, #Not eligible
    if_any(starts_with("CRIT_"), ~.x %in% c(55, 99)) ~ 55, # pending
    HEALTHY_CHECK < 19 ~ 3 #19 criteria pending
  ) ) %>%
  mutate(HEALTHY_ELIGIBLE_14WK = case_when(
    if_all(starts_with("CRIT_") & !matches("CRIT_GA"), ~.x %in% c(1, 666)) ~ 1, #eligible
    if_any(starts_with("CRIT_"), ~.x == 0) ~ 0, #Not eligible
    if_any(starts_with("CRIT_"), ~.x %in% c(55, 99)) ~ 55, # pending
    HEALTHY_CHECK < 19 ~ 3 #19 criteria pending
  )) %>%
  ungroup() 

df_healthy <- healthyOutcome %>% 
  filter(HEALTHY_ELIGIBLE == 1)

save(healthyOutcome, file= paste0(path_to_save, "healthyOutcome",".RData",sep = ""))
save(df_healthy, file= paste0(path_to_save, "df_healthy",".RData",sep = ""))

#*****************************************************************************
#*3. data for eligiblity and missingness
#*****************************************************************************
df_eli <- df_criteria %>%
  dplyr::select(MOMID, PREGID, SITE, starts_with("CRIT_")) %>%
  mutate(
    C1 = ifelse(!is.na(CRIT_AGE),CRIT_AGE,55), 
    C2 = ifelse(!is.na(CRIT_GA),CRIT_GA,55), 
    C3 = ifelse(!is.na(CRIT_BMI_MUAC),CRIT_BMI_MUAC,55),
    C4 = ifelse(!is.na(CRIT_HEIGHT),CRIT_HEIGHT,55), 
    C5 = ifelse(!is.na(CRIT_SINGLEPREG),CRIT_SINGLEPREG,55), 
    C6 = ifelse(!is.na(CRIT_LBW),CRIT_LBW,55),
    C7 = ifelse(!is.na(CRIT_STILLBIRTH),CRIT_STILLBIRTH,55), 
    C8 = ifelse(!is.na(CRIT_UNPL_CESARIAN),CRIT_UNPL_CESARIAN,55), 
    C9 = ifelse(!is.na(CRIT_SMOKE),CRIT_SMOKE,55), 
    C10 = case_when(
      CRIT_DRINK == 1|CRIT_DRINK == 0 ~ CRIT_DRINK,
      CRIT_DRINK == 666 ~ 1,
      TRUE~55), 
    C11 = ifelse(!is.na(CRIT_CHRONIC),CRIT_CHRONIC,55),
    C12 = ifelse(!is.na(CRIT_HIV),CRIT_HIV,55), 
    C13 = ifelse(!is.na(CRIT_BP),CRIT_BP,55), 
    C14 = ifelse(!is.na(CRIT_MALARIA),CRIT_MALARIA,55), 
    C15 = ifelse(!is.na(CRIT_HEPATITISB),CRIT_HEPATITISB,55),
    C16 = ifelse(!is.na(CRIT_HEPATITISC),CRIT_HEPATITISC,55),
    C17 = ifelse(!is.na(CRIT_IRON),CRIT_IRON,55),
    C18 = ifelse(!is.na(CRIT_INFLAM),CRIT_INFLAM,55), 
    C19 = ifelse(!is.na(CRIT_HEMOGLOBINOPATHIES),CRIT_HEMOGLOBINOPATHIES,55),
    C20 = ifelse(!is.na(CRIT_G6PD),CRIT_G6PD,55)
  )

df_eli$C1 <- factor(
  df_eli$C1, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C2 <- factor(
  df_eli$C2, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C3 <- factor(
  df_eli$C3, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C4 <- factor(
  df_eli$C4, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C5 <- factor(
  df_eli$C5, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C6 <- factor(
  df_eli$C6, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C7 <- factor(
  df_eli$C7, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C8 <- factor(
  df_eli$C8, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C9 <- factor(
  df_eli$C9, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C10 <- factor(
  df_eli$C10, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C11 <- factor(
  df_eli$C11, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C12 <- factor(
  df_eli$C12, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)
df_eli$C13 <- factor(
  df_eli$C13, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)
df_eli$C14 <- factor(
  df_eli$C14, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)
df_eli$C15 <- factor(
  df_eli$C15, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)
df_eli$C16 <- factor(
  df_eli$C16, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)
df_eli$C17 <- factor(
  df_eli$C17, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C18 <- factor(
  df_eli$C18, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C19 <- factor(
  df_eli$C19, 
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli$C20 <- factor(
  df_eli$C20,
  levels = c(1,0,55),
  labels = c("Eligible", "Ineligible", "Pending")
)

df_eli_long <- df_eli %>% 
  dplyr::select(-starts_with("CRIT_")) %>% 
  pivot_longer(cols = -c(MOMID, PREGID, SITE), 
               names_to = "Variable", 
               values_to = "Value")

#order variable value
df_eli_long$Variable <- factor(
  df_eli_long$Variable,
  levels = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11",
             "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20"))

save(df_eli_long, file= paste0(path_to_save, "df_eli_long",".RData",sep = ""))

## 18.8 Optional query for healthy cohort criteria missingness ----

### Pull data query subsets of missing/pending criteria (and HAVE any ineligible)
## look at responses 
# pak_healthy_cohort <- df_eli %>%
#   # filter(SITE == "Pakistan") %>%
#   select(SITE, MOMID, PREGID, C1:C20)
# 
# c8_response <- read_xlsx("Z:/ReMAPP Healthy Cohort/Healthy_Cohort_Missingness_Responses/2025-09-05-Pakistan_ALL-Pending-Healthy-Cohort_response.xlsx",
#                          sheet = "C8")
# c9_response <- read_xlsx("Z:/ReMAPP Healthy Cohort/Healthy_Cohort_Missingness_Responses/2025-09-05-Pakistan_ALL-Pending-Healthy-Cohort_response.xlsx",
#                          sheet = "C9")
# c19_response <- read_xlsx("Z:/ReMAPP Healthy Cohort/Healthy_Cohort_Missingness_Responses/2025-09-05-Pakistan_ALL-Pending-Healthy-Cohort_response.xlsx",
#                           sheet = "C19")
# c20_response <- read_xlsx("Z:/ReMAPP Healthy Cohort/Healthy_Cohort_Missingness_Responses/2025-09-05-Pakistan_ALL-Pending-Healthy-Cohort_response.xlsx",
#                           sheet = "C20")
# 
# pak_healthy_cohort_filter <- pak_healthy_cohort %>%
#   left_join(c8_response %>% select(PREGID, Comments, contains("true_missing")) %>% rename(C8_comment = Comments), by = c("PREGID")) %>%
#   left_join(c9_response %>% select(PREGID, Comments, contains("true_missing")) %>% rename(C9_comment = Comments), by = c("PREGID")) %>%
#   left_join(c19_response %>% select(PREGID, Comments, contains("true_missing")) %>% rename(C19_comment = Comments), by = c("PREGID")) %>%
#   left_join(c20_response %>% select(PREGID, Comments, contains("true_missing")) %>% rename(C20_comment = Comments), by = c("PREGID")) %>%
#   mutate(C8 = case_when(c8_true_missing ==1 ~ "True Missing", TRUE ~ C8),
#          C9 = case_when(c9_true_missing ==1 ~ "True Missing", TRUE ~ C9),
#          C19 = case_when(c19_true_missing ==1 ~ "True Missing", TRUE ~ C19),
#          C20 = case_when(c20_true_missing ==1 ~ "True Missing", TRUE ~ C20)
#   ) %>%
#   select(SITE, MOMID, PREGID, C1:C20)
# # 
# df_healthy_long <- pak_healthy_cohort_filter %>%
#   pivot_longer(
#     cols = C1:C20,
#     names_to = "Variable",
#     values_to = "Value"
#   )
# 
# criteria_queries_ids <- df_healthy_long %>%
#   group_by(SITE, MOMID, PREGID) %>%
#   mutate(SUM_INELGIGIBLE = sum(Value == "Ineligible")) %>%
#   mutate(SUM_PENDING = sum(Value == "Pending")) %>%
#   filter(SUM_PENDING > 0)
# 
# table(criteria_queries_ids$Value)
# 
# sum_pending <- criteria_queries_ids %>% distinct(SITE, PREGID, SUM_PENDING)
# 
# criteria_queries <- df_eli %>% filter(PREGID %in% as.vector(criteria_queries_ids$PREGID)) %>%
#   arrange(across(C1:C20, desc)) %>%
#   relocate(C1:C20, .after = PREGID) %>%
#   relocate(SITE, .before = MOMID) %>%
#   select(SITE, MOMID, PREGID, C1:C20) %>%
#   left_join(sum_pending, by = c("SITE", "PREGID", "MOMID")) %>%
#   relocate(SUM_PENDING, .after = "PREGID")
# 
# vec_names <- c("India-SAS", "Ghana","India-CMC", "Kenya", "Pakistan",  "Zambia")
# 
# criteria_queries_export <- list()
# for (i in seq_along(vec_names)) {
#   site_name <- vec_names[i]  # Get site name
#   print(site_name)
# 
#   criteria_queries_export[[as.character(site_name)]] <- criteria_queries %>%
#     filter(SITE == site_name)
# 
# }
# 
# library(writexl)
# # Loop through each dataset in the list
# for (site_name in names(criteria_queries_export)) {
#   # Get the corresponding dataset
#   dataset <- criteria_queries_export[[site_name]]
# 
#   # Define the file name based on the site name
#   file_name <- paste0(UploadDate, "-", site_name, "_ALL-Pending-Healthy-Cohort", ".xlsx")
# 
#   # Write the dataset to the Excel file
#   write_xlsx(dataset, path = paste0("D:/Users/stacie.loisate/Documents/Output/Outcomes-Queries/healthy cohort/", file_name))
# 
#   # Optional: Print message to confirm export
#   print(paste0("Exported ", file_name))
#   print(paste0(site_name, " missing = ", dim(criteria_queries_export[[site_name]])[1]))
# 
# }
# 
