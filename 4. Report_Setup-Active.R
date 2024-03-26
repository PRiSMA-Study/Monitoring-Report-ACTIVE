#*****************************************************************************
#### MONITORING REPORT SETUP ####
#* Function: Merge all forms together in wide format to create a dataset with one row for each woman for each visit 
#* Input: .RData files for each form (generated from 1. data import code)
#* Last updated: 26 March 2024


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
## input: MatData_Screen_Enroll
## Includes all women who are enrolled who have entered the PNC period. Includes constructed variables relevent to PNC
## InfData_Pnc_Visits
## input: InfData_Report
## includes all infants. Includes constructed variables relevant to PNC period
## MatData_Hb_Visit
## input: MatData_Anc_Visits
## Includes relevant constructed variables for ReMAPP tables looking at testing completion for Hb at each visit
## MatData_Hb_GA_Visit
## input: MatData_Anc_Visits
## Includes relevant constructed variables for ReMAPP tables looking at Hb test outcomes by gestational age
## healthyOutcome
## input: MatData_Anc_Visits
## Includes relevant constructed variables for ReMAPP healthy criteria 
#1964
#*****************************************************************************

## load in data 
rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)

UploadDate = "2024-03-22"

load(paste0("~/Monitoring Report/data/cleaned/", UploadDate, "/", "MatData_Wide_", UploadDate, ".RData"))
load(paste0("~/Monitoring Report/data/cleaned/", UploadDate, "/", "InfData_Wide_", UploadDate, ".RData"))

# set path to save 
path_to_save <- "D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/"

setwd(paste0("D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/"))

## import all rda files 
rda_files = list.files(pattern="*.RData")
walk(rda_files, ~ load(.x, .GlobalEnv))

#*****************************************************************************
#* Extract variables for monitoring report 
#*****************************************************************************
#update/add/delete variable names in the varNames_sheet.xlsx (check if the var is multiple or singe when add)
MatNames_sheet <- read_excel("~/Monitoring Report/code/varNames_sheet.xlsx", sheet = "MaternalVars")
InfNames_sheet <- read_excel("~/Monitoring Report/code/varNames_sheet.xlsx", sheet = "InfantVars")

MatData_Report <- MatData_Wide %>% 
  select(matches(MatNames_sheet$varname), 
         US_GA_WKS_ENROLL, US_GA_DAYS_ENROLL,
         M02_SCRN_OBSSTDAT,EDD_US,
         EST_CONCEP_DATE,
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
         M00_KNOWN_DOBYN_SCORRES, DOB) 

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

#**************************************************************************************
#* PRiSMA Tables 1-4
#* Table 1: Pre-screening and enrollment numbers for PRiSMA MNH Study for the most recent one week 
#* Table 2: Cumulative pre-screening numbers for PRiSMA MNH Study 
#* Table 3: Cumulative enrollment numbers for PRiSMA MNH Study 
#* Table 4: Study Status for PRiSMA MNH Study 

#* Pre-screening, screen and enrollment info
#* Output = MatData_Screen_Enroll 
#**************************************************************************************

#study start date and cut date
MatData_Screen_Enroll <- MatData_Report %>% 
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
  ungroup() 


# enrollment criteria
MatData_Screen_Enroll <- MatData_Screen_Enroll %>% 
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
                         M02_AGE_IEORRES == 0 | M02_PC_IEORRES == 0 | M02_CATCHMENT_IEORRES == 0 |
                           M02_CATCH_REMAIN_IEORRES == 0  ~ 0,
                         TRUE ~ 99),
    CONSENT = case_when(!is.na(M02_CONSENT_IEORRES) ~ M02_CONSENT_IEORRES,
                        TRUE ~ 99),
    ENROLL = case_when(M02_CONSENT_IEORRES == 1 & !is.na(M02_FORMCOMPLDAT_MNH02) & ELIGIBLE == 1 ~ 1,
                       ELIGIBLE == 0 | M02_CONSENT_IEORRES == 0 | M02_CONSENT_IEORRES == 77 ~ 0,
                       TRUE ~ 77)) %>%
  ## Assign denominators
  mutate(PRESCREEN_DENOM = case_when(PRESCREEN == 1 ~ 1,
                                     TRUE ~ 0)) %>%      
  mutate(SCREEN_DENOM = case_when(SCREEN == 1 ~ 1,
                                  TRUE ~ 0)) %>%     
  mutate(ENROLL_DENOM = case_when(ENROLL == 1 ~ 1,
                                  TRUE ~ 0))      

## extract Reasons for exclusion (check if cases match within mnh00 and 02)
MatData_Screen_Enroll <- MatData_Screen_Enroll %>% 
  #reason for exclusion in pre-screen
  mutate(
    PRESCR_PREGSIGN = ifelse(M00_PREGNANT_IEORRES == 1, 1,
                             ifelse(M00_PREGNANT_IEORRES == 0, 0, 99)),
    
    ## if you answer 1 to PRESCR_PREGSIGN, you answer PRESCR_GA25
    PRESCR_GA25 = ifelse(M00_EGA_LT25_IEORRES == 1, 1,
                         ifelse(M00_EGA_LT25_IEORRES == 0 & M00_PREGNANT_IEORRES == 1, 0, 99)),
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25, you answer PRESCR_AGE
    PRESCR_AGE = ifelse(M00_AGE_IEORRES == 1, 1,
                        ifelse(M00_AGE_IEORRES == 0  & M00_PREGNANT_IEORRES == 1 & 
                                 M00_EGA_LT25_IEORRES == 1, 0, 99)),
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25 & PRESCR_AGE, you answer PRESCR_CATCHAREA
    PRESCR_CATCHAREA = ifelse(M00_CATCHMENT_IEORRES == 1, 1,
                              ifelse(M00_CATCHMENT_IEORRES == 0 & 
                                       M00_AGE_IEORRES == 1  & 
                                       M00_PREGNANT_IEORRES == 1 & 
                                       M00_EGA_LT25_IEORRES == 1, 0, 99)),
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25 & PRESCR_AGE & PRESCR_CATCHAREA, you answer PRESCR_OTHER
    PRESCR_OTHER = ifelse(M00_OTHR_IEORRES == 0, 1,
                          ifelse(M00_OTHR_IEORRES == 1 & 
                                   M00_CATCHMENT_IEORRES == 1 & 
                                   M00_AGE_IEORRES == 1  & 
                                   M00_PREGNANT_IEORRES == 1 & 
                                   M00_EGA_LT25_IEORRES == 1, 0, 99)), 
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25 & PRESCR_AGE & PRESCR_CATCHAREA & PRESCR_OTHER, you answer CONSENT_PRESCREEN
    CONSENT_PRESCREEN = ifelse(M00_CON_YN_DSDECOD == 1 | M00_ASSNT_YN_DSDECOD == 1| 
                                 M00_CON_LAR_YN_DSDECOD == 1, 1, 
                               ifelse((M00_CON_YN_DSDECOD == 0 | 
                                         M00_ASSNT_YN_DSDECOD == 0| M00_ASSNT_YN_DSDECOD == 0) & ## consent has "or" statements. if one method did not consent, then the rest are "77"
                                        (M00_CON_LAR_YN_DSDECOD == 0 | M00_CON_LAR_YN_DSDECOD == 77) & 
                                        M00_OTHR_IEORRES == 0 & 
                                        M00_CATCHMENT_IEORRES == 1 & 
                                        M00_AGE_IEORRES == 1  & 
                                        M00_PREGNANT_IEORRES == 1 & 
                                        M00_EGA_LT25_IEORRES == 1, 0, 99))) %>% 
  # eligible based on pre-screening
  mutate(PRESCR_ELIGIBLE = ifelse(M00_PREGNANT_IEORRES == 1 & 
                                    M00_EGA_LT25_IEORRES == 1 & 
                                    M00_AGE_IEORRES == 1 &
                                    M00_CATCHMENT_IEORRES == 1 & 
                                    M00_OTHR_IEORRES == 0 & 
                                    CONSENT_PRESCREEN == 1, 1,
                                  ifelse(M00_PREGNANT_IEORRES == 0 | M00_EGA_LT25_IEORRES == 0 | M00_AGE_IEORRES == 0 |
                                           M00_CATCHMENT_IEORRES == 0 | M00_OTHR_IEORRES == 1 | CONSENT_PRESCREEN == 0, 0, 99)
  )) %>% 
  
  #reason for exclusion in screen/enroll
  mutate(
    AGE = ifelse(M02_AGE_IEORRES == 1, 1,
                 ifelse(M02_AGE_IEORRES == 0, 0, 99)),
    GA20 = ifelse(M02_PC_IEORRES == 1, 1,
                  ifelse(M02_PC_IEORRES == 0 & 
                           M02_AGE_IEORRES == 1, 0, 99)),
    
    CATCHAREA = ifelse(M02_CATCHMENT_IEORRES == 1, 1,
                       ifelse(M02_CATCHMENT_IEORRES == 0 &
                                M02_PC_IEORRES == 1 & 
                                M02_AGE_IEORRES == 1, 0, 99)),
    CATCHREMAIN = ifelse(M02_CATCH_REMAIN_IEORRES == 1, 1, 
                         ifelse(M02_CATCH_REMAIN_IEORRES == 0 & 
                                  M02_CATCHMENT_IEORRES == 1 & 
                                  M02_PC_IEORRES == 1 & 
                                  M02_AGE_IEORRES == 1, 0, 99)), 
    SCRN_CONSENT = ifelse(M02_CONSENT_IEORRES == 1, 1, 
                          ifelse(M02_CONSENT_IEORRES == 0 & 
                                   M02_CATCH_REMAIN_IEORRES == 1 & 
                                   M02_CATCHMENT_IEORRES == 1 &
                                   M02_PC_IEORRES == 1 & 
                                   M02_AGE_IEORRES == 1, 0, 99))) %>% 
  ## generate variable with age at pregnancy end
  # first, pull all instances of miscarriage in MNH04 
  mutate(MISCARRIAGE = ifelse(M04_FETAL_LOSS_DSDECOD_1== 1 | M04_FETAL_LOSS_DSDECOD_2== 1 |  M04_FETAL_LOSS_DSDECOD_3== 1 |
                                M04_FETAL_LOSS_DSDECOD_4== 1 | M04_FETAL_LOSS_DSDECOD_5== 1, 1, 0)) %>% 
  # pull fetal loss date from MNH04 
  mutate(M04_FETAL_LOSS_DSSTDAT_1 = replace(M04_FETAL_LOSS_DSSTDAT_1, M04_FETAL_LOSS_DSSTDAT_1==ymd("1907-07-07"), NA), 
         M04_FETAL_LOSS_DSSTDAT_2 = replace(M04_FETAL_LOSS_DSSTDAT_2, M04_FETAL_LOSS_DSSTDAT_2==ymd("1907-07-07"), NA), 
         M04_FETAL_LOSS_DSSTDAT_3 = replace(M04_FETAL_LOSS_DSSTDAT_3, M04_FETAL_LOSS_DSSTDAT_3==ymd("1907-07-07"), NA), 
         M04_FETAL_LOSS_DSSTDAT_4 = replace(M04_FETAL_LOSS_DSSTDAT_4, M04_FETAL_LOSS_DSSTDAT_4==ymd("1907-07-07"), NA), 
         M04_FETAL_LOSS_DSSTDAT_5 = replace(M04_FETAL_LOSS_DSSTDAT_5, M04_FETAL_LOSS_DSSTDAT_5==ymd("1907-07-07"), NA)) %>% 
  mutate(MISCARRIAGE_DATE = pmin(M04_FETAL_LOSS_DSSTDAT_1, M04_FETAL_LOSS_DSSTDAT_2, 
                                 M04_FETAL_LOSS_DSSTDAT_3 ,M04_FETAL_LOSS_DSSTDAT_4, M04_FETAL_LOSS_DSSTDAT_5, na.rm = TRUE)) %>% 
# calculate preg end in days (MISCARRIAGE DATE-EST_CONCEP_DATE)
  mutate(ENDPREG_DAYS = ifelse(is.na(MISCARRIAGE), GESTAGE_AT_BIRTH_DAYS, 
                               ifelse(MISCARRIAGE==1, MISCARRIAGE_DATE-EST_CONCEP_DATE, GESTAGE_AT_BIRTH_DAYS))) %>%   # if miscarriage=1, use MISCARRIAGE_DATE as DOB 
  mutate(DOB = ifelse(is.na(MISCARRIAGE), as.character(DOB),
                    ifelse(MISCARRIAGE==1, as.character(MISCARRIAGE_DATE), 
                                  as.character(DOB))) )%>% 
  # convert DOB to date class 
  mutate(DOB = as.Date(DOB, format = "%Y-%m-%d"))


## Has woman closed out? 
MatData_Screen_Enroll <- MatData_Screen_Enroll %>% 
  mutate(M23_VISIT_COMPLETE = ifelse(M23_CLOSE_DSDECOD == 1 | M23_CLOSE_DSDECOD == 2 | M23_CLOSE_DSDECOD == 3 |
                                       M23_CLOSE_DSDECOD == 4 | M23_CLOSE_DSDECOD == 5 | M23_CLOSE_DSDECOD == 9,1,0)) %>% 
  ## CLOSEOUT YES/NO 
  mutate(MATERNAL_CLOSEOUT_YN = ifelse(M23_CLOSE_DSDECOD == 1 | M23_CLOSE_DSDECOD == 2 | 
                                         M23_CLOSE_DSDECOD == 3 | M23_CLOSE_DSDECOD == 4 | 
                                         M23_CLOSE_DSDECOD == 5 | M23_CLOSE_DSDECOD == 6, 1, 0)) 

# out <- MatData_Screen_Enroll %>% filter(SITE == "India-SAS") %>% select(SITE, SCRNID, MOMID, PREGID, 
#                                                                         M00_EGA_LT25_IEORRES,M00_AGE_IEORRES,
#                                                                         M00_CATCHMENT_IEORRES, M00_OTHR_IEORRES,
#                                                                         M00_OTHR_REASON_IEORRES, M00_CON_YN_DSDECOD,
#                                                                         M00_CON_LAR_YN_DSDECOD, M00_ASSNT_YN_DSDECOD,
#                                                                         M00_CON_WITNESS_SIGNATURE_PRYN,
#                                                                         PRESCR_ELIGIBLE, PRESCR_PREGSIGN, 
#                                                                         PRESCR_GA25, PRESCR_AGE,
#                                                                         PRESCR_CATCHAREA, PRESCR_OTHER,
#                                                                         CONSENT_PRESCREEN) %>% 
#   filter(PRESCR_ELIGIBLE==0 & M00_ASSNT_YN_DSDECOD ==1)
#   ## people who have all eligibility criteria but have default value for consent
#   # filter(PRESCR_ELIGIBLE==0 & PRESCR_PREGSIGN==1 & PRESCR_GA25==1 & PRESCR_AGE==1 & PRESCR_CATCHAREA==1 &PRESCR_OTHER==0)

## export 
save(MatData_Screen_Enroll, file= paste0(path_to_save, "MatData_Screen_Enroll",".RData",sep = ""))
#**************************************************************************************
### MatData_Anc_Visits
# input: MatData_Screen_Enroll
# Includes all women who are enrolled. Includes constructed variables relevant to ANC 

#**************************************************************************************
MatData_Anc_Visits <- MatData_Screen_Enroll %>% 
  filter(ENROLL == 1) %>% 
  # filter(M01_US_OHOSTDAT_1 != ymd("1907-07-07")) %>%   
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
         ANC20_OVERDUE = ifelse(UploadDate>ANC20_LATE & is.na(M04_VISIT_COMPLETE_2) & US_GA_WKS_ENROLL >17, 1, 0),
         ANC28_OVERDUE = ifelse(UploadDate>ANC28_LATE & is.na(M04_VISIT_COMPLETE_3), 1, 0),
         ANC32_OVERDUE = ifelse(UploadDate>ANC32_LATE & is.na(M04_VISIT_COMPLETE_4), 1, 0),
         ANC36_OVERDUE = ifelse(UploadDate>ANC36_LATE & is.na(M04_VISIT_COMPLETE_5), 1, 0)) %>%
  ## CALCULATE INDICATOR VARIALBE FOR ANY VISIT TYPE = i
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
  ## GENERATE INDICATOR VARIALBE FOR CENSORING 
  ## 1. generate indicator variable if closeout was during ANC or PNC period 
  mutate(M23_CLOSEOUT_PERIOD = ifelse(!(is.na(DOB)), "PNC", "ANC")) %>% 
  ## 2. calculate GA window at closeout
  mutate(M23_AGE_VISIT_DAYS = as.numeric(ymd(M23_CLOSE_DSSTDAT)-EST_CONCEP_DATE),
         M23_AGE_VISIT_WKS = as.numeric(ymd(M23_CLOSE_DSSTDAT)-EST_CONCEP_DATE) %/% 7) %>% 
  ## 3. assign indicator variable for closeout window (what ga window did the woman closeout in)
  mutate(M23_CLOSEOUT_WINDOW =   ifelse(M23_CLOSEOUT_PERIOD == "ANC" &  M23_AGE_VISIT_DAYS >= 126 & M23_AGE_VISIT_DAYS <= 181 & US_GA_WKS_ENROLL <= 17, 2, ## only participants who are <= 17wks at enrollment will have this visit 
                                        ifelse(M23_CLOSEOUT_PERIOD == "ANC" & M23_AGE_VISIT_DAYS <=139, 1, 
                                               ifelse(M23_CLOSEOUT_PERIOD == "ANC" & M23_AGE_VISIT_DAYS >= 182 & M23_AGE_VISIT_DAYS <= 216, 3, 
                                                      ifelse(M23_CLOSEOUT_PERIOD == "ANC" & M23_AGE_VISIT_DAYS >= 217 & M23_AGE_VISIT_DAYS <= 237, 4, ## MIGHT BE UPDATING THE 237 NUMBER 
                                                             ## Since all visit types are assigned based on the GA at the time of assessment, we don't need to worry about 4 vs 5. 
                                                             ## if ANC32 is missed, it is conducted at ANC36 visit
                                                             ifelse(M23_CLOSEOUT_PERIOD == "ANC" & M23_AGE_VISIT_DAYS >= 238 & M23_AGE_VISIT_DAYS <= 322, 5, 88)))))) 

## export 
save(MatData_Anc_Visits, file= paste0(path_to_save, "MatData_Anc_Visits",".RData",sep = ""))


##  WOMAN WITH BIRTH AT 35 WEEKS SHOULD NOT HAVE AN ANC36 (ANC36_PASS_LATE == 0)
# test <- MatData_Anc_Visits %>% filter(ENDPREG_DAYS==270 ) %>% select(SITE, MOMID,ENDPREG_DAYS,
#                                                                     ENROLL_ONTIME, ENROLL_PASS,
#                                                                     ANC20_ONTIME, ANC20_PASS,
#                                                                     ANC28_ONTIME, ANC28_PASS,
#                                                                     ANC32_ONTIME, ANC32_PASS,
#                                                                     ANC36_ONTIME, ANC36_PASS)

# if someone delievered at 213 days (30wks); NO anc28/ anc32/  anc36
# if someone delievered at 54 days (7wks); NO ANC20 / anc28/ anc32/  anc36
# if someone delievered at 310 days (44wks);  ANC20 / anc28/ anc32/  anc36
# if someone delievered at 270 days (38wks);  NO anc36
#**************************************************************************************
### MatData_Ipc_Visits
# input: MatData_Screen_Enroll
# Includes all women who are enrolled 
#**************************************************************************************
# extract unique MOM/INFANT pair from infant wide data  - one row for each MOM/INFANT pair
InfData_Report_Mat <- InfData_Report %>% distinct(SITE, MOMID, PREGID, .keep_all = TRUE )


MatData_Ipc_Visits_Mat <- MatData_Screen_Enroll %>% 
  select(SITE, MOMID, PREGID, ENROLL, DOB, EDD_US,EST_CONCEP_DATE, ENDPREG_DAYS,M09_TYPE_VISIT_6, M23_CLOSE_DSSTDAT,
         M10_TYPE_VISIT_6, M09_VISIT_COMPLETE_6, M10_VISIT_COMPLETE_6, M09_INFANTS_FAORRES_6,M23_CLOSE_DSDECOD, 
         contains("GESTAGE_AT_VISIT_DAYS")) %>% 
  filter(ENROLL == 1,
         (is.na(ENDPREG_DAYS) | ENDPREG_DAYS > 139)) %>% ## filter for anyone who is enrolled and has delivered (DOB is not NA) 
  # join with InfData_Report_Mom
  left_join(InfData_Report_Mat[c("SITE", "MOMID", "PREGID", "INFANTID", "M11_TYPE_VISIT_6", "M11_VISIT_COMPLETE_6")], 
            by = c("SITE", "MOMID", "PREGID")) %>% 
  ## CALCULATE LATE IPC WINDOWS
  mutate(IPC_LATE = (EDD_US - as.difftime(280, unit="days")) + as.difftime(300, unit="days")) %>% 
  ## using upload date
  mutate(IPC_PASS = ifelse(IPC_LATE<UploadDate, 1, 0)) %>%  
  ## CALCULATE INDICATOR VARIALBE FOR ANY VISIT TYPE = i
  mutate(ANY_TYPE_VISIT_COMPLETE_6 = ifelse((M09_TYPE_VISIT_6 == 6 & M09_VISIT_COMPLETE_6 == 1) | 
                                              (M10_TYPE_VISIT_6 == 6 & M10_VISIT_COMPLETE_6 == 1) |
                                              (M11_TYPE_VISIT_6 == 6 & M11_VISIT_COMPLETE_6 == 1), 1, 0)) %>% 
  ## GENERATE INDICATOR VARIABLE FOR PARTICPANTS WHO HAVE CLOSED OUT EITHER DUE TO LTFU (M23_CLOSE_DSDECOD==4), 
      # WITHDREW (M23_CLOSE_DSDECOD==5), OR INVESTIGATOR CLOSED (M23_CLOSE_DSDECOD==6)
  mutate(CLOSED_OUT =  ifelse(is.na(M23_CLOSE_DSDECOD), 0, 
                              ifelse(M23_CLOSE_DSDECOD == 4 | M23_CLOSE_DSDECOD == 5 | M23_CLOSE_DSDECOD == 6, 1, 0)))  %>% 
  ## CALCULATE INDICATOR VARIALBE FOR DENOMINATOR
  mutate(IPC_DENOM = ifelse((ANY_TYPE_VISIT_COMPLETE_6==1 | IPC_PASS==1) & 
                              ((M23_CLOSE_DSSTDAT > IPC_LATE) | is.na(M23_CLOSE_DSSTDAT)) &  # only include participants who have not yet closed out
                              CLOSED_OUT != 1, 1, 0)) %>% ## remove any participant who has been lost to follow up NEW 
  ## CALCULATE INDICATOR VARIALBE FOR MISSING ALL VISIT TYPE = 6
  mutate(GA42_MISSING_IPC = ifelse((IPC_DENOM ==1) & is.na(ANY_TYPE_VISIT_COMPLETE_6), 1, 0)) 
  

## export 
save(MatData_Ipc_Visits_Mat, file= paste0(path_to_save, "MatData_Ipc_Visits_Mat",".RData",sep = ""))

## extract any momids that are missing ipc forms and have passed window 
MISSING_IPC_MOMIDS <- MatData_Ipc_Visits_Mat %>% filter(GA42_MISSING_IPC==1) %>% pull(MOMID)
MISSING_IPC_MOMIDS_EXTRACT <- MatData_Screen_Enroll %>%  select(SITE, MOMID, PREGID, EST_CONCEP_DATE,M23_CLOSE_DSDECOD, M23_CLOSE_DSSTDAT,EDD_US, contains("_VISIT_DATE_"), ) %>% 
  mutate(IPC_LATE = (EDD_US - as.difftime(280, unit="days")) + as.difftime(300, unit="days")) %>% 
  mutate(CLOSED_OUT =  ifelse(is.na(M23_CLOSE_DSDECOD), 0, 
                              ifelse(M23_CLOSE_DSDECOD == 4 | M23_CLOSE_DSDECOD == 5 | 
                                       M23_CLOSE_DSDECOD == 6, 1, 0))) %>% 
  select(-EDD_US, -M23_CLOSE_DSDECOD) %>% 
  relocate(IPC_LATE, .after=4) %>%
  relocate(CLOSED_OUT, .after = 5) %>% 
  pivot_longer(cols = -c(1:7), 
               values_to = "VISIT_DATE", 
               names_to = "FORM") %>% 
  filter(MOMID %in% MISSING_IPC_MOMIDS, CLOSED_OUT!=1) %>% 
  group_by(SITE, MOMID, PREGID, EST_CONCEP_DATE,IPC_LATE,CLOSED_OUT, M23_CLOSE_DSSTDAT) %>% 
  # generate variable with date last seen
  summarise(DATE_LAST_SEEN = max(VISIT_DATE, na.rm= TRUE)) %>% 
  ungroup() %>% 
  # generate variable with age last seen
  mutate(AGE_LAST_SEEN_DAYS = as.numeric(DATE_LAST_SEEN - EST_CONCEP_DATE)) %>% 
  filter(!is.na(AGE_LAST_SEEN_DAYS)) %>% 
  # generate variable for upload date
  mutate(UPLOADDATE = ymd(UploadDate)) %>% 
  # calculate the age of particpant TODAY (at date of upload)
  mutate(AGE_AT_UPLOAD_DAYS = as.numeric(UPLOADDATE - EST_CONCEP_DATE)) %>% 
## generate new weeks variable that includes 1 decimal point for gestational ages that represent the days 
  mutate(AGE_LAST_SEEN_WKS_FLOOR = AGE_LAST_SEEN_DAYS %/% 7, 
         AGE_LAST_SEEN_WKS = as.numeric(paste0(AGE_LAST_SEEN_WKS_FLOOR, ".", (AGE_LAST_SEEN_DAYS-(AGE_LAST_SEEN_WKS_FLOOR*7))))) %>% 
  mutate(AGE_AT_UPLOAD_WKS_FLOOR = AGE_AT_UPLOAD_DAYS %/% 7, 
         AGE_AT_UPLOAD_WKS = as.numeric(paste0(AGE_AT_UPLOAD_WKS_FLOOR, ".", (AGE_AT_UPLOAD_DAYS-(AGE_AT_UPLOAD_WKS_FLOOR*7))))) %>% 
  select(SITE, MOMID, PREGID, DATE_LAST_SEEN,AGE_LAST_SEEN_WKS,  AGE_AT_UPLOAD_WKS, IPC_LATE, M23_CLOSE_DSSTDAT)

# # generate table of contents for excel sheet to send to sites 
# tab_contents <- data.frame("varname" = names(MISSING_IPC_MOMIDS_EXTRACT),
#                               "definition" = c("site", 
#                                                "momid",
#                                                "pregnancy id",
#                                                "date the last participant was seen. generated by extracting the latest visit date reported for the particpant.",
#                                                "gestational age at the last visit particpant was seen with the decimal representing the number of days. For example, 16.6 represents 16 weeks and 6 days.",
#                                                "calculated gestational age at the time of data upload with the decimal representing the number of days. For example, 16.6 represents 16 weeks and 6 days.",
#                                                "date IPC late window closed for a particpant. ",
#                                                "closeout date for the partipant. if a particpant does not have a closeout form, this cell will be empty."))
# 
# 
# # export
# site_vec = as.vector(unique(MISSING_IPC_MOMIDS_EXTRACT$SITE))
# for (i in site_vec) {
#   df_out <- MISSING_IPC_MOMIDS_EXTRACT %>% filter(SITE == i)
#   list_of_datasets <- list("Table of Contents" = tab_contents, "Report" = df_out)
#   
#   write.xlsx(list_of_datasets, file = paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/","PRISMA_", i, "_IDs_missing_IPC_", UploadDate, ".xlsx"),
#              headerStyle = createStyle(textDecoration = "Bold"))
# }
# 

# maternal protocol compliance -- only maternal for mnh09/10 - one row for each mom 
MatData_Ipc_Visits_Compliance_Mat <- MatData_Screen_Enroll %>% 
  select(SITE, MOMID, PREGID, ENROLL, DOB,EST_CONCEP_DATE, EDD_US, ENDPREG_DAYS,M09_TYPE_VISIT_6, 
         M10_TYPE_VISIT_6, M09_VISIT_COMPLETE_6, M10_VISIT_COMPLETE_6, M09_INFANTS_FAORRES_6) %>% 
  filter(ENROLL == 1,
         (is.na(ENDPREG_DAYS) | ENDPREG_DAYS > 139)) %>% ## filter for anyone who is enrolled and has delivered (DOB is not NA) 
  ## CALCULATE INDICATOR VARIALBE FOR ANY VISIT TYPE = i
  mutate(ANY_TYPE_VISIT_COMPLETE_6 = ifelse((M09_TYPE_VISIT_6 == 6 & M09_VISIT_COMPLETE_6 == 1) | 
                                              (M10_TYPE_VISIT_6 == 6 & M10_VISIT_COMPLETE_6 == 1), 1, 0)) 

## export 
save(MatData_Ipc_Visits_Compliance_Mat, file= paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/MatData_Ipc_Visits_Compliance_Mat",".RData",sep = ""))

# infant protocol compliance -- only infant for mnh11 - one row for each infant 
MatData_Ipc_Visits_Compliance_Inf <- InfData_Report %>% 
  select(SITE, INFANTID, PREGID, M11_TYPE_VISIT_6, M11_VISIT_COMPLETE_6) %>% 
  full_join(MatData_Screen_Enroll[c("SITE", "MOMID", "PREGID", "ENROLL", "DOB", "EDD_US", "ENDPREG_DAYS","M09_TYPE_VISIT_6", 
                                    "M10_TYPE_VISIT_6", "M09_VISIT_COMPLETE_6", "M10_VISIT_COMPLETE_6", "M09_INFANTS_FAORRES_6")],
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

#**************************************************************************************

## MatData_Pnc_Visits
MatData_Pnc_Visits <- MatData_Screen_Enroll %>% 
  select(SITE,SCRNID, MOMID, PREGID, ENROLL,US_GA_WKS_ENROLL, DOB, EST_CONCEP_DATE,ENDPREG_DAYS,MISCARRIAGE,
         ends_with("_7"),  ends_with("_8"),  ends_with("_9"), 
         ends_with("_10"), ends_with("_11"), ends_with("_12"),starts_with("M09_BIRTH_DSTERM_INF"), contains("M23_")) %>% 
  ## filter participants with a delivery outcome (exclude NA age at pregnancy end)
  #filter(!is.na(ENDPREG_DAYS)) %>% 
  ## CALCULATE ON TIME AND LATE PNC WINDOWS 
  mutate(PNC0_ONTIME = DOB + as.difftime(5, unit="days"),
         PNC0_LATE = DOB + as.difftime(5, unit="days"),
         PNC1_ONTIME = DOB + as.difftime(14, unit="days"),
         PNC1_LATE = DOB + as.difftime(14, unit="days"),
         PNC4_ONTIME = DOB + as.difftime(35, unit="days"),
         PNC4_LATE = DOB + as.difftime(35, unit="days"),
         PNC6_ONTIME = DOB + as.difftime(55, unit="days"),
         PNC6_LATE = DOB + as.difftime(104, unit="days"),
         PNC26_ONTIME = DOB + as.difftime(202, unit="days"),
         PNC26_LATE = DOB + as.difftime(279, unit="days"),
         PNC52_ONTIME = DOB + as.difftime(384, unit="days"),
         PNC52_LATE = DOB + as.difftime(454, unit="days")) %>%
  ## CALCULATE INDICATOR VARIABLES for missed PNC visits -- on time windows not active (using late for now)
  ## max date has to be less than the upload date
  # mutate(PNC0_OVERDUE = ifelse(UploadDate>PNC0_ONTIME & is.na(M12_VISIT_COMPLETE_7), 1, 0),
  #        PNC1_OVERDUE = ifelse(UploadDate>PNC1_ONTIME & is.na(M12_VISIT_COMPLETE_8), 1, 0),
  #        PNC4_OVERDUE = ifelse(UploadDate>PNC4_ONTIME & is.na(M12_VISIT_COMPLETE_9), 1, 0),
  #        PNC6_OVERDUE = ifelse(UploadDate>PNC6_ONTIME & is.na(M12_VISIT_COMPLETE_10), 1, 0),
  #        PNC26_OVERDUE = ifelse(UploadDate>PNC26_ONTIME & is.na(M12_VISIT_COMPLETE_11), 1, 0),
  #        PNC52_OVERDUE = ifelse(UploadDate>PNC52_ONTIME & is.na(M12_VISIT_COMPLETE_12), 1, 0)) %>% 
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
                                     M09_BIRTH_DSTERM_INF4_6 == 1 | M09_BIRTH_DSTERM_INF4_6 == 2, 1, 0))  %>%
  ## GENERATE INDICATOR VARIALBE FOR CENSORING 
  ## 1. generate indicator variable if closeout was during ANC or PNC period 
  mutate(M23_CLOSEOUT_PERIOD = ifelse(!is.na(DOB), "PNC", "ANC"))


## export 
save(MatData_Pnc_Visits, file= paste0(path_to_save, "MatData_Pnc_Visits",".RData",sep = ""))
#**************************************************************************************
#### VISIT COMPLETION #### 
#* Are forms completed with visit status = 1 or 2? 
#* Table 5

#* Num: Any form with visit type = i AND visit status = 1 or 2 
#* Denom: Passed window for visit type = i
#**************************************************************************************
## ANC
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
                                      (ENDPREG_DAYS>160 | is.na(ENDPREG_DAYS)), 1, 0),
         
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
                                   ((M23_CLOSE_DSSTDAT > ANC20_PASS) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),  ## closed out after window or has not yet closed out
         VC_ANC28_DENOM = ifelse(ANC28_PASS==1  &
                                   (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS))& ## birth outcome after window or has not yet delivered
                                   ((M23_CLOSE_DSSTDAT > ANC28_PASS) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),  ## closed out after window or has not yet closed out
         VC_ANC32_DENOM = ifelse(ANC32_PASS==1  & 
                                   (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & ## birth outcome after window or has not yet delivered
                                 ((M23_CLOSE_DSSTDAT > ANC32_PASS) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),  ## closed out after window or has not yet closed out
         VC_ANC36_DENOM = ifelse(ANC36_PASS==1  &
                                   (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS)) & ## birth outcome after window or has not yet delivered
                                 ((M23_CLOSE_DSSTDAT > ANC28_PASS) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),  ## closed out after window or has not yet closed out 
  ) %>% 
  ## generate denominators - LATE
  mutate(VC_ENROLL_DENOM_LATE = ifelse(ENROLL_PASS_LATE == 1, 1, 0), 
         VC_ANC20_DENOM_LATE = ifelse(ANC20_PASS_LATE == 1 & US_GA_WKS_ENROLL <= 17 &
                                        (ENDPREG_DAYS>160 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC20_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),  ## closed out after window or has not yet closed out
         VC_ANC28_DENOM_LATE = ifelse(ANC28_PASS_LATE == 1  &
                                        (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC28_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0), 
         VC_ANC32_DENOM_LATE = ifelse(ANC32_PASS_LATE == 1  & 
                                        (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC32_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0), 
         VC_ANC36_DENOM_LATE = ifelse(ANC36_PASS_LATE == 1  & 
                                        (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS))  & 
                                        ((M23_CLOSE_DSSTDAT > ANC36_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)
  )

## export 
save(Visit_Complete_Anc, file= paste0(path_to_save, "Visit_Complete_Anc",".RData",sep = ""))

# PNC
## i filtered out anyone who has an endpreg and then
# for pnc visits where we do not expect those with miscarriages, i filter 
Visit_Complete_Pnc <- MatData_Pnc_Visits %>%
  filter(!is.na(ENDPREG_DAYS)) %>% ## only include participants with a birth outcome
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"),
         contains("_PASS"), contains("_ONTIME"), contains("_LATE"), M23_CLOSE_DSSTDAT, DOB, ENDPREG_DAYS) %>%
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
         VC_PNC26_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_11 == 1 & PNC26_PASS_LATE == 1 &
                                      ((M23_CLOSE_DSSTDAT > PNC26_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC52_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_12 == 1 & PNC52_PASS_LATE == 1 &
                                      ((M23_CLOSE_DSSTDAT > PNC52_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)) %>%
  ## exclude particpants who do not have DOB -- we are not able to calculate their windows - exclude from num and denom
  mutate(VC_PNC0_NUM_LATE = ifelse(is.na(PNC0_PASS_LATE), NA, VC_PNC0_NUM_LATE),
         VC_PNC1_NUM_LATE = ifelse(is.na(PNC1_PASS_LATE), NA, VC_PNC1_NUM_LATE),
         VC_PNC4_NUM_LATE = ifelse(is.na(PNC4_PASS_LATE), NA, VC_PNC4_NUM_LATE),
         VC_PNC6_NUM_LATE = ifelse(is.na(PNC6_PASS_LATE), NA, VC_PNC6_NUM_LATE),
         VC_PNC26_NUM_LATE = ifelse(is.na(PNC26_PASS_LATE), NA, VC_PNC26_NUM_LATE),
         VC_PNC52_NUM_LATE = ifelse(is.na(PNC52_PASS_LATE), NA, VC_PNC52_NUM_LATE)) %>%
  ## generate denominators - ontime
  mutate(VC_PNC0_DENOM = ifelse(PNC0_PASS==1 &
                                  ((M23_CLOSE_DSSTDAT > PNC0_ONTIME) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC1_DENOM = ifelse(PNC1_PASS==1 &
                                  ((M23_CLOSE_DSSTDAT > PNC1_ONTIME) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC4_DENOM = ifelse(PNC4_PASS==1 &
                                  ((M23_CLOSE_DSSTDAT > PNC4_ONTIME) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC6_DENOM = ifelse(PNC6_PASS==1 & 
                                  ((M23_CLOSE_DSSTDAT > PNC6_ONTIME) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         
         VC_PNC26_DENOM = ifelse(PNC26_PASS==1 & ENDPREG_DAYS>139 & 
                                   ((M23_CLOSE_DSSTDAT > PNC26_ONTIME) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         
         VC_PNC52_DENOM = ifelse(PNC52_PASS==1 & ENDPREG_DAYS>139 & 
                                   ((M23_CLOSE_DSSTDAT > PNC52_ONTIME) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)
  ) %>%
  ## generate denominators - late
  mutate(VC_PNC0_DENOM_LATE = ifelse(PNC0_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC0_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC1_DENOM_LATE = ifelse(PNC1_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC1_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC4_DENOM_LATE = ifelse(PNC4_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC4_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC6_DENOM_LATE = ifelse(PNC6_PASS_LATE==1 & 
                                       ((M23_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         
         VC_PNC26_DENOM_LATE = ifelse(PNC26_PASS_LATE==1 & ENDPREG_DAYS>139 & 
                                        ((M23_CLOSE_DSSTDAT > PNC26_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0),
         
         VC_PNC52_DENOM_LATE = ifelse(PNC52_PASS_LATE==1 & ENDPREG_DAYS>139 & 
                                        ((M23_CLOSE_DSSTDAT > PNC52_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)
  )



## export 
save(Visit_Complete_Pnc, file= paste0(path_to_save, "Visit_Complete_Pnc",".RData",sep = ""))
#**************************************************************************************
#### PROTOCOL COMPLIANCE #### 
#* Are all expected forms for the visit complete? 
# Table 6 (ANC) [dataframe: Prot_Compliance_Anc]
# Table 8 (PNC) [dataframe: Prot_Compliance_Pnc]
# ReMAPP Table 1 (MNH23/MNH26) [dataframe: Prot_Compliance_MNH25]

#* Num (hard code in monitoring report rmd): For each form: visit type = i AND visit status = 1 or 2 AND passed window for visit type = i 
#* Denom: Any form with visit type = i AND visit status = 1 or 2 AND passed window for visit type = i 
#**************************************************************************************
## ANC
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

## PNC 
## CALCULATE DENOMINATORS FOR PROTOCOL COMPLIANCE  
Prot_Compliance_Pnc <- MatData_Pnc_Visits %>% 
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), ENDPREG_DAYS,
         contains("_VISIT_COMPLETE_"), BIRTH_OUTCOME_YN, contains("M23_"), contains("_ONTIME")) %>% 
  #filter(BIRTH_OUTCOME_YN == 1) %>% 
  filter(!is.na(ENDPREG_DAYS)) %>% ## exclude anyone without a birth outcome (missing ENDPREG_DAYS)
  # DENOMINATOR for protocol compliance
  mutate(PC_PNC0_DENOM =ifelse(ANY_TYPE_VISIT_COMPLETE_7 == 1 & PNC0_PASS_LATE == 1, 1, 0),
         PC_PNC1_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_8 == 1 & PNC1_PASS_LATE == 1, 1, 0),
         PC_PNC4_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_9 == 1 & PNC4_PASS_LATE == 1, 1, 0),
         PC_PNC6_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_10 == 1 & PNC6_PASS_LATE == 1 , 1, 0),
         PC_PNC26_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_11 == 1 & PNC26_PASS_LATE == 1, 1, 0),
         PC_PNC52_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_12 == 1 & PNC52_PASS_LATE == 1, 1, 0)) 


## export 
save(Prot_Compliance_Pnc, file= paste0(path_to_save, "Prot_Compliance_Pnc",".RData",sep = ""))


## MNH25 protocol compliance 
MNH25_Pnc <- MatData_Pnc_Visits %>% select(SITE, MOMID, PREGID, DOB,M25_VISIT_COMPLETE_10, ANY_TYPE_VISIT_COMPLETE_10, PNC6_PASS_LATE)

Prot_Compliance_MNH25 <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, PREGID, M02_SCRN_OBSSTDAT,US_GA_WKS_ENROLL,  ENDPREG_DAYS,M23_CLOSE_DSSTDAT,
         ANY_TYPE_VISIT_COMPLETE_1,ANY_TYPE_VISIT_COMPLETE_2, ANY_TYPE_VISIT_COMPLETE_4, ANY_TYPE_VISIT_COMPLETE_5,
         ENROLL_PASS_LATE, ANC20_PASS_LATE, ANC32_PASS_LATE, ANC36_PASS_LATE,
         M25_VISIT_COMPLETE_1, M25_VISIT_COMPLETE_2, M25_VISIT_COMPLETE_4, M25_VISIT_COMPLETE_5) %>% 
  # NUMERATOR for protocol compliance 
  mutate(M25_ANCLESS20_NUM =ifelse(((M25_VISIT_COMPLETE_1 == 1 & ANC20_PASS_LATE == 1) | 
                                     (M25_VISIT_COMPLETE_2 == 1 & ANC20_PASS_LATE == 1)) & 
                                     (ENDPREG_DAYS>160 | is.na(ENDPREG_DAYS)) & 
                                     ((M23_CLOSE_DSSTDAT > ANC32_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)),1,0),
         M25_ANCOVER31_NUM =ifelse(((M25_VISIT_COMPLETE_4 == 1 & ANC36_PASS_LATE == 1)|
                                     (M25_VISIT_COMPLETE_5 == 1 & ANC36_PASS_LATE == 1)) & 
                                     (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & 
                                     ((M23_CLOSE_DSSTDAT > ANC32_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)),1,0)
  ) %>% 
  # DENOMINATOR for protocol compliance
  mutate(PC_ANCLESS20_DENOM = ifelse(ANC20_PASS_LATE == 1 &
                                       (ENDPREG_DAYS>160 | is.na(ENDPREG_DAYS)) & 
                                       ((M23_CLOSE_DSSTDAT > ANC32_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0), 
         PC_ANCOVER31_DENOM = ifelse(ANC36_PASS_LATE == 1 & 
                                       (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS))  & 
                                       ((M23_CLOSE_DSSTDAT > ANC32_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)) %>% 
  left_join(MNH25_Pnc, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(PC_PNC6_DENOM = ifelse(PNC6_PASS_LATE == 1 & 
                                  ((M23_CLOSE_DSSTDAT > PNC6_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)) 

# Export
save(Prot_Compliance_MNH25, file= paste0(path_to_save, "Prot_Compliance_MNH25",".RData",sep = ""))

## MNH26 protocol compliance 
MNH26_Pnc <- MatData_Pnc_Visits %>% select(SITE, MOMID, PREGID, DOB, M26_VISIT_COMPLETE_10, ANY_TYPE_VISIT_COMPLETE_10, PNC6_PASS_LATE)

Prot_Compliance_MNH26 <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, PREGID, M02_SCRN_OBSSTDAT,US_GA_WKS_ENROLL, ENDPREG_DAYS,M23_CLOSE_DSSTDAT,
         ANY_TYPE_VISIT_COMPLETE_1,ANY_TYPE_VISIT_COMPLETE_2, ANY_TYPE_VISIT_COMPLETE_4, ANY_TYPE_VISIT_COMPLETE_5,
         ENROLL_PASS_LATE, ANC20_PASS_LATE, ANC32_PASS_LATE, ANC36_PASS_LATE,
         M26_VISIT_COMPLETE_1, M26_VISIT_COMPLETE_2, M26_VISIT_COMPLETE_4, M26_VISIT_COMPLETE_5) %>% 
  ## add remapp launch date for each site since these are remapp criteria 
  mutate(REMAPP_LAUNCH = ifelse((SITE == "Kenya" & M02_SCRN_OBSSTDAT >= "2023-04-10") |
                                  (SITE == "Pakistan" & M02_SCRN_OBSSTDAT >= "2022-09-22") |
                                  (SITE == "Ghana" & M02_SCRN_OBSSTDAT >= "2022-12-28") | 
                                  (SITE == "Zambia" & M02_SCRN_OBSSTDAT >= "2022-12-15") | 
                                  (SITE == "India-CMC" & M02_SCRN_OBSSTDAT >= "2023-07-18") |
                                  (SITE == "India-SAS" & M02_SCRN_OBSSTDAT >= "2023-12-01"), 1, 0)) %>%
  filter(REMAPP_LAUNCH ==1)  %>% 
  # NUMERATOR for protocl compliance 
  mutate(M26_ANCLESS20_NUM =ifelse(((M26_VISIT_COMPLETE_1 == 1 & ANC20_PASS_LATE == 1) | 
                                      (M26_VISIT_COMPLETE_2 == 1 & ANC20_PASS_LATE == 1)) & 
                                     (ENDPREG_DAYS>160 | is.na(ENDPREG_DAYS)) & 
                                     ((M23_CLOSE_DSSTDAT > ANC32_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)),1,0),
         M26_ANCOVER31_NUM =ifelse(((M26_VISIT_COMPLETE_4 == 1 & ANC36_PASS_LATE == 1)|
                                      (M26_VISIT_COMPLETE_5 == 1 & ANC36_PASS_LATE == 1)) & 
                                     (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & 
                                     ((M23_CLOSE_DSSTDAT > ANC32_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)),1,0)
  ) %>% 
  # DENOMINATOR for protocol compliance
  mutate(PC_ANCLESS20_DENOM = ifelse(ANC20_PASS_LATE == 1 &
                                       (ENDPREG_DAYS>160 | is.na(ENDPREG_DAYS)) & 
                                       ((M23_CLOSE_DSSTDAT > ANC32_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0), 
         PC_ANCOVER31_DENOM = ifelse(ANC36_PASS_LATE == 1 & 
                                       (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS))  & 
                                       ((M23_CLOSE_DSSTDAT > ANC32_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)) %>% 
  left_join(MNH26_Pnc, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(PC_PNC6_DENOM = ifelse(PNC6_PASS_LATE == 1 & 
                                  ((M23_CLOSE_DSSTDAT > PNC6_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT)), 1, 0)) 

# Export
save(Prot_Compliance_MNH26, file= paste0(path_to_save, "Prot_Compliance_MNH26",".RData",sep = ""))
#**************************************************************************************
####  FORM COMPLETION #### 
#* Are forms completed regardless of visit status? 
# Table 7 (ANC) [dataframe: Form_Completion_Anc]
# Table 9 (PNC) [dataframe: Form_Completion_Pnc]

#* Num (hard code in monitoring report rmd): for each form: visit type = i AND have any visit status AND passed window for visit type = i 
#* Denom: Any form with visit type = i AND have any visit status AND passed window for visit type = i
#**************************************************************************************

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

## PNC 
Form_Completion_Pnc <- MatData_Pnc_Visits %>% 
  select(SITE, MOMID, PREGID, ENDPREG_DAYS,contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), 
         contains("_VISIT_COMPLETE_"),contains("_TYPE_VISIT_"), BIRTH_OUTCOME_YN, M23_CLOSE_DSDECOD) %>%
  #filter(BIRTH_OUTCOME_YN == 1) %>% 
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
         FC_PNC52_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_12 == 1 & PNC52_PASS_LATE == 1, 1, 0)) 

## export 
save(Form_Completion_Pnc, file= paste0(path_to_save, "Form_Completion_Pnc",".RData",sep = ""))
#**************************************************************************************
#### HEAT MAPS #### 
#* Are forms completed with visit status = 1 or 2 for EACH visit? 
#* PRISMA figure 3a/3b

#* Num: By form: visit type = i AND visit status = 1 or 2 AND passed late window 
#* Denom: Passed window for visit type = i AND didn't closeout AND has not yet delivered
#**************************************************************************************
## ANC
Heat_Maps_Anc <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, ENROLL, PREGID, US_GA_WKS_ENROLL, contains("TYPE_VISIT_"), contains("_VISIT_COMPLETE_"),
         contains("_LATE"),ENDPREG_DAYS, M23_CLOSE_DSSTDAT) %>% 
  # indicator for any type visit = i with any visit status 
  mutate(TYPE_VISIT_ANY_STATUS_1 = ifelse((M01_TYPE_VISIT_1 == 1 & !is.na(M01_VISIT_COMPLETE_1)) | 
                                            (M03_TYPE_VISIT_1 == 1 & !is.na(M03_VISIT_COMPLETE_1)) |
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
  ## generate denominators - LATE
    # has the late window passed AND has not delivered AND has not closed out OR has a form in this visit 
  mutate(HM_ENROLL_DENOM_LATE = ifelse(ENROLL_PASS_LATE == 1 | TYPE_VISIT_ANY_STATUS_1 == 1, 1, 0), 
         HM_ANC20_DENOM_LATE = ifelse((ANC20_PASS_LATE == 1 & US_GA_WKS_ENROLL <= 17 &
                                        (ENDPREG_DAYS>160 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC20_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT))) | 
                              TYPE_VISIT_ANY_STATUS_2 == 1, 1, 0),  ## closed out after window or has not yet closed out
         HM_ANC28_DENOM_LATE = ifelse((ANC28_PASS_LATE == 1  &
                                        (ENDPREG_DAYS>216 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC28_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT))) | 
                                        TYPE_VISIT_ANY_STATUS_3 == 1, 1, 0), 
         HM_ANC32_DENOM_LATE = ifelse((ANC32_PASS_LATE == 1  & 
                                        (ENDPREG_DAYS>237 | is.na(ENDPREG_DAYS)) & 
                                        ((M23_CLOSE_DSSTDAT > ANC32_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT))) | 
                                        TYPE_VISIT_ANY_STATUS_4 == 1, 1, 0), 
         HM_ANC36_DENOM_LATE = ifelse((ANC36_PASS_LATE == 1  & 
                                        (ENDPREG_DAYS>272 | is.na(ENDPREG_DAYS))  & 
                                        ((M23_CLOSE_DSSTDAT > ANC36_PASS_LATE) | is.na(M23_CLOSE_DSSTDAT))) | 
                                        TYPE_VISIT_ANY_STATUS_5 == 1, 1, 0)
  ) 

## export 
save(Heat_Maps_Anc, file= paste0(path_to_save, "Heat_Maps_Anc",".RData",sep = ""))

# PNC
## i filtered out anyone who has an endpreg and then
# for pnc visits where we do not expect those with miscarriages, i filter 
Heat_Maps_Pnc <- MatData_Pnc_Visits %>%
  filter(!is.na(ENDPREG_DAYS)) %>% ## only include participants with a birth outcome
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("TYPE_VISIT"),
         contains("VISIT_COMPLETE"),
         contains("_PASS"), contains("_ONTIME"), contains("_LATE"), M23_CLOSE_DSSTDAT, DOB, ENDPREG_DAYS) %>%
  # indicator for any type visit = i with any visit status 
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
                                             (M12_TYPE_VISIT_10 == 10 & !is.na(M12_VISIT_COMPLETE_10)), 1, 0),
         ## might need to add some detail here of who we expect after the 42 day fu (only women who have had a live birth)
         TYPE_VISIT_ANY_STATUS_11 = ifelse((M05_TYPE_VISIT_11 == 11 & !is.na(M05_VISIT_COMPLETE_11)) |
                                             (M06_TYPE_VISIT_11 == 11 & !is.na(M06_VISIT_COMPLETE_11)) |
                                             (M07_TYPE_VISIT_11 == 11 & !is.na(M07_VISIT_COMPLETE_11)) |
                                             (M08_TYPE_VISIT_11 == 11 & !is.na(M08_VISIT_COMPLETE_11)) | 
                                             (M12_TYPE_VISIT_11 == 11 & !is.na(M12_VISIT_COMPLETE_11)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_12 = ifelse((M05_TYPE_VISIT_12 == 12 & !is.na(M05_VISIT_COMPLETE_12)) |
                                             (M06_TYPE_VISIT_12 == 12 & !is.na(M06_VISIT_COMPLETE_12)) |
                                             (M12_TYPE_VISIT_12 == 12 & !is.na(M12_VISIT_COMPLETE_12)), 1, 0)) %>% 
  ## generate denominators - LATE
  # has the late window passed AND has not delivered AND has not closed out OR has a form in this visit 
  mutate(HM_PNC0_DENOM_LATE = ifelse((PNC0_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC0_LATE) | is.na(M23_CLOSE_DSSTDAT))) | 
                                       TYPE_VISIT_ANY_STATUS_7 == 1, 1, 0),
         HM_PNC1_DENOM_LATE = ifelse((PNC1_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC1_LATE) | is.na(M23_CLOSE_DSSTDAT))) | 
                                       TYPE_VISIT_ANY_STATUS_8 == 1, 1, 0),
         HM_PNC4_DENOM_LATE = ifelse((PNC4_PASS_LATE==1 &
                                       ((M23_CLOSE_DSSTDAT > PNC4_LATE) | is.na(M23_CLOSE_DSSTDAT))) | 
                                       TYPE_VISIT_ANY_STATUS_9 == 1, 1, 0),
         HM_PNC6_DENOM_LATE = ifelse((PNC6_PASS_LATE==1 & 
                                       ((M23_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M23_CLOSE_DSSTDAT))) | 
                                       TYPE_VISIT_ANY_STATUS_10 == 1, 1, 0),
         
         HM_PNC26_DENOM_LATE = ifelse((PNC26_PASS_LATE==1 & ENDPREG_DAYS>139 & 
                                        ((M23_CLOSE_DSSTDAT > PNC26_LATE) | is.na(M23_CLOSE_DSSTDAT))) | 
                                        TYPE_VISIT_ANY_STATUS_11 == 1, 1, 0),
         
         HM_PNC52_DENOM_LATE = ifelse((PNC52_PASS_LATE==1 & ENDPREG_DAYS>139 & 
                                        ((M23_CLOSE_DSSTDAT > PNC52_LATE) | is.na(M23_CLOSE_DSSTDAT))) | 
                                        TYPE_VISIT_ANY_STATUS_12 == 1, 1, 0)
  )


## export 
save(Heat_Maps_Pnc, file= paste0(path_to_save, "Heat_Maps_Pnc",".RData",sep = ""))

#**************************************************************************************
#* ReMAPP Table 4: Hemoglobin measurements for participants in ReMAPP per visit 
#* Only looking at those who are enrolled 
#* Output = MatData_Hb_VISIT
#* Input = MatData_Anc_Visits, MatData_Pnc_Visits
#**************************************************************************************
MatData_Hb_Visit <- MatData_Anc_Visits %>% 
  left_join(MatData_Pnc_Visits[c("SITE", "MOMID", "PREGID","PNC6_OVERDUE")], 
            by = c("SITE", "MOMID", "PREGID")) %>% 
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
  # # replace outliars with NA
  mutate(M08_CBC_HB_LBORRES_1 = ifelse(M08_CBC_HB_LBORRES_1 < 1 | M08_CBC_HB_LBORRES_1 > 20, NA, M08_CBC_HB_LBORRES_1),
         M08_CBC_HB_LBORRES_2 = ifelse(M08_CBC_HB_LBORRES_2 < 1 | M08_CBC_HB_LBORRES_2 > 20, NA, M08_CBC_HB_LBORRES_2),
         M08_CBC_HB_LBORRES_3 = ifelse(M08_CBC_HB_LBORRES_3 < 1 | M08_CBC_HB_LBORRES_3 > 20, NA, M08_CBC_HB_LBORRES_3),
         M08_CBC_HB_LBORRES_4 = ifelse(M08_CBC_HB_LBORRES_4 < 1 | M08_CBC_HB_LBORRES_4 > 20, NA, M08_CBC_HB_LBORRES_4),
         M08_CBC_HB_LBORRES_5 = ifelse(M08_CBC_HB_LBORRES_5 < 1 | M08_CBC_HB_LBORRES_5 > 20, NA, M08_CBC_HB_LBORRES_5),
         M08_CBC_HB_LBORRES_10 = ifelse(M08_CBC_HB_LBORRES_10 < 1 | M08_CBC_HB_LBORRES_10 > 20, NA, M08_CBC_HB_LBORRES_10)) %>%
  ## generate denominators 
  mutate(DenHBV1 = ifelse((M08_VISIT_COMPLETE_1 == 1 & M08_TYPE_VISIT_1 == 1) |  ENROLL_OVERDUE == 1, 1, 0),
         DenHBV2 = ifelse((M08_VISIT_COMPLETE_2 == 1 & M08_TYPE_VISIT_2 == 2 & US_GA_WKS_ENROLL <= 17) |  ANC20_OVERDUE == 1, 1, 0), # 
         DenHBV3 = ifelse((M08_VISIT_COMPLETE_3 == 1 & M08_TYPE_VISIT_3 == 3) |  ANC28_OVERDUE == 1, 1, 0),
         DenHBV4 = ifelse((M08_VISIT_COMPLETE_4 == 1 & M08_TYPE_VISIT_4 == 4) |  ANC32_OVERDUE == 1, 1, 0),
         DenHBV5 = ifelse((M08_VISIT_COMPLETE_5 == 1 & M08_TYPE_VISIT_5 == 5) |  ANC36_OVERDUE == 1, 1, 0),
         DenHBV10 = ifelse((M08_VISIT_COMPLETE_10 == 1 & M08_TYPE_VISIT_10 == 10) |  PNC6_OVERDUE == 1, 1, 0)) %>% 
  mutate(REMAPP_LAUNCH = ifelse((SITE == "Kenya" & M02_SCRN_OBSSTDAT >= "2023-04-10") |
                                  (SITE == "Pakistan" & M02_SCRN_OBSSTDAT >= "2022-09-22") |
                                  (SITE == "Ghana" & M02_SCRN_OBSSTDAT >= "2022-12-28") | 
                                  (SITE == "Zambia" & M02_SCRN_OBSSTDAT >= "2022-12-15") | 
                                  (SITE == "India-CMC" & M02_SCRN_OBSSTDAT >= "2023-07-18") |
                                  (SITE == "India-SAS" & M02_SCRN_OBSSTDAT >= "2023-12-01"), 1, 0)) %>%
  filter(ENROLL == 1 & REMAPP_LAUNCH ==1)

## export 
save(MatData_Hb_Visit, file= paste0(path_to_save, "MatData_Hb_Visit",".RData",sep = ""))
#**************************************************************************************
#* ReMAPP Figure 1: Hemoglobin measures by gestational age for participants enrolled in PRiSMA MNH 
#* Only looking at those who are enrolled 
#* Output = MatData_HB_GA_Visit
#* Input = MatData_Anc_Visits
#* We need: 
#* GA at each visit 
#* M08_CBC_HB_LBORRES_1
#* M08_CBC_HB_LBORRES_1
#* M06_SPHB_VSSTAT -- Was non-invasive total hemoglobin (SpHb) measured at this visit?
#* M06_SPHB_LBORRES -- Record non-invasive total hemoglobin (SpHb): . g/dL
#* M06_HB_POC_LBPERF -- Was point-of-care hemoglobin test performed at this visit?
#* M06_HB_POC_LBORRES -- Record point-of-care hemoglobin : . g/dL
#* 
#**************************************************************************************
MatData_Anc_Remapp <- MatData_Anc_Visits %>% 
  mutate(REMAPP_LAUNCH = ifelse((SITE == "Kenya" & M02_SCRN_OBSSTDAT >= "2023-04-10") |
                                  (SITE == "Pakistan" & M02_SCRN_OBSSTDAT >= "2022-09-22") |
                                  (SITE == "Ghana" & M02_SCRN_OBSSTDAT >= "2022-12-28") | 
                                  (SITE == "Zambia" & M02_SCRN_OBSSTDAT >= "2022-12-15") | 
                                  (SITE == "India-CMC" & M02_SCRN_OBSSTDAT >= "2023-07-18") | 
                                  (SITE == "India-SAS" & M02_SCRN_OBSSTDAT >= "2023-12-01"), 1, 0)) %>%
  filter(ENROLL == 1 & REMAPP_LAUNCH ==1) 

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
           GA_WKS > 0 & GA_WKS <= 13 ~ 1, 
           GA_WKS > 13 & GA_WKS<=26 ~ 2, 
           GA_WKS >26 & GA_WKS<=40 ~ 3, 
           TRUE ~ NA
         ))


## export 
save(MatData_Hb_GA_Visit, file= paste0(path_to_save, "MatData_Hb_GA_Visit",".RData",sep = ""))

#**************************************************************************************
#*ReMAPP healthy cohort criteria 
# Table 2 + 3
#*Output: healthyOutcome.rda
 #*includes: CRIT_s, HEALTHY_ELIGIBLE
#**************************************************************************************

df_maternal <- MatData_Screen_Enroll %>%
  filter(M02_CONSENT_IEORRES == 1) %>% 
  mutate(REMAPP_LAUNCH = ifelse((SITE == "Kenya" & M02_SCRN_OBSSTDAT >= "2023-04-10") | 
                                  (SITE == "Pakistan" & M02_SCRN_OBSSTDAT >= "2022-09-22") |
                                  (SITE == "Ghana" & M02_SCRN_OBSSTDAT >= "2022-12-28") | 
                                  (SITE == "Zambia" & M02_SCRN_OBSSTDAT >= "2022-12-15") |
                                  (SITE == "India-CMC" & M02_SCRN_OBSSTDAT >= "2023-07-18") | 
                                  (SITE == "India-SAS" & M02_SCRN_OBSSTDAT >= "2023-12-01"), 1, 0)) %>% 
  filter(REMAPP_LAUNCH == 1)

# save(df_maternal, file = "derived_data/df_maternal.rda")

#derive criteria
df_criteria <- df_maternal %>%
  dplyr::select(
    SCRNID, MOMID, PREGID, SITE, M00_KNOWN_DOBYN_SCORRES,ENROLL,
    M00_BRTHDAT, M00_ESTIMATED_AGE, M00_SCHOOL_YRS_SCORRES, 
    M00_SCHOOL_SCORRES,
    US_GA_DAYS_ENROLL,EST_CONCEP_DATE,
    M02_SCRN_OBSSTDAT, M02_CONSENT_IEORRES,
    M03_MARITAL_SCORRES_1,
    M03_SMOKE_OECOCCUR_1, M03_CHEW_BNUT_OECOCCUR_1, M03_CHEW_OECOCCUR_1, M03_DRINK_OECOCCUR_1,
    M05_ANT_PEDAT_1, M05_WEIGHT_PERES_1, M05_HEIGHT_PERES_1, M05_MUAC_PERES_1,
    M04_PRETERM_RPORRES_1, M04_PH_PREV_RPORRES_1, M04_PH_PREVN_RPORRES_1, M04_PH_LIVE_RPORRES_1, 
    M04_MISCARRIAGE_RPORRES_1, M04_MISCARRIAGE_CT_RPORRES_1, M04_PH_OTH_RPORRES_1,M04_STILLBIRTH_RPORRES_1,
    M04_LOWBIRTHWT_RPORRES_1, M04_MALARIA_EVER_MHOCCUR_1, 
    M04_CANCER_EVER_MHOCCUR_1, M04_KIDNEY_EVER_MHOCCUR_1, M04_CARDIAC_EVER_MHOCCUR_1,
    M04_HIV_MHOCCUR_1, M04_HIV_EVER_MHOCCUR_1, M04_UNPL_CESARIAN_PROCCUR_1, M04_PREECLAMPSIA_RPORRES_1,
    M04_GEST_DIAB_RPORRES_1, M04_PREMATURE_RUPTURE_RPORRES_1,
    M04_MACROSOMIA_RPORRES_1, M04_OLIGOHYDRAMNIOS_RPORRES_1,
    M04_APH_RPORRES_1, M04_PPH_RPORRES_1,
    M06_SINGLETON_PERES_1, 
    M06_BP_SYS_VSORRES_1_1, M06_BP_SYS_VSORRES_2_1, M06_BP_SYS_VSORRES_3_1,
    M06_BP_DIA_VSORRES_1_1, M06_BP_DIA_VSORRES_2_1, M06_BP_DIA_VSORRES_3_1,
    M06_MALARIA_POC_LBORRES_1, M06_MALARIA_POC_LBPERF_1, 
    M06_HBV_POC_LBORRES_1, M06_HBV_POC_LBPERF_1, M06_HCV_POC_LBORRES_1, M06_HCV_POC_LBPERF_1,
    M06_HIV_POC_LBORRES_1, M06_HIV_POC_LBPERF_1,
    num_range("M06_HB_POC_LBORRES_",1:12),
    num_range("M08_CBC_HB_LBORRES_",1:12),
    num_range("M08_LBSTDAT_",1:12),
    M08_MN_LBPERF_8_1, M08_FERRITIN_LBORRES_1, 
    M08_RBC_LBPERF_2_1, M08_RBC_THALA_LBORRES_1, M08_RBC_LBPERF_3_1, M08_RBC_GLUC6_LBORRES_1,
    M08_MN_LBPERF_12_1, M08_CRP_LBORRES_1, M08_MN_LBPERF_13_1, M08_AGP_LBORRES_1, 
    num_range("M09_INFANTID_INF",1:4,"_6"),
    num_range("M09_INFANTID_INF",1:4,"_6"),
    num_range("M09_BIRTH_DSTERM_INF",1:4,"_6"), 
    num_range("M09_DELIV_DSSTDAT_INF",1:4,"_6")
  ) %>%
  mutate(
    # A. age at enrollment
    # Aged 18 to 34 years
    AGE_ENROLL = ifelse(M00_KNOWN_DOBYN_SCORRES == 1 &  M00_BRTHDAT != "1907-07-07", 
                        as.numeric(difftime(M02_SCRN_OBSSTDAT,M00_BRTHDAT, units = "days")/365),
                        ifelse(M00_KNOWN_DOBYN_SCORRES == 0 & M00_ESTIMATED_AGE != -7, M00_ESTIMATED_AGE, 99)),
    
    CRIT_AGE = ifelse((AGE_ENROLL > 0 & AGE_ENROLL < 18) | AGE_ENROLL > 34, 0,
                      ifelse(AGE_ENROLL >= 18 & AGE_ENROLL <= 34, 1, 55)
    ),
    # B. GA at enrollment
    # gestational age at enrollment - Gestational age <14 weeks 
    BASELINE_GA_WKS = floor(US_GA_DAYS_ENROLL/7),
    CRIT_GA = ifelse(BASELINE_GA_WKS > 0 & BASELINE_GA_WKS < 14, 1,
                     ifelse(BASELINE_GA_WKS >= 14 & BASELINE_GA_WKS <=26, 0,
                            ifelse(BASELINE_GA_WKS == -7 | is.na(BASELINE_GA_WKS), NA, 77))),
    
    # C. Pre-pregnancy or early pregnancy body mass index (BMI) of >18.5 and <30 kg/m2 AND mid-upper arm circumference (MUAC) > 23cm [45]
    # BMI
    BMI = M05_WEIGHT_PERES_1 / M05_HEIGHT_PERES_1 / M05_HEIGHT_PERES_1 * 10000,
    
    TEMP_BMI = ifelse(BMI <= 18.5 | BMI >= 30, 0, 
                      ifelse(BMI > 18.5 & BMI < 30, 1, 55)
    ),
    # MUAC mid-upper arm circumference - MUAC
    TEMP_MUAC = ifelse(M05_MUAC_PERES_1 <= 23, 0, 
                       ifelse(M05_MUAC_PERES_1 > 23, 1, 55)
    ),
    CRIT_BMI_MUAC = case_when(
      TEMP_BMI == 1 & TEMP_MUAC == 1 ~ 1, 
      TEMP_BMI == 0 | TEMP_MUAC == 0 ~ 0, 
      TRUE ~ 55
    ),
    # D. Height ≥150 cm
    CRIT_HEIGHT = ifelse(M05_HEIGHT_PERES_1 < 150, 0,
                         ifelse(M05_HEIGHT_PERES_1 >= 150, 1, 55)
    ),
    # E. Singleton pregnancy
    CRIT_SINGLEPREG = ifelse(M06_SINGLETON_PERES_1 == 0, 0,
                             ifelse(M06_SINGLETON_PERES_1 == 1, 1, 55)
    ),
    # F. no iron deficiency (not iron deficient: serum ferritin > 15 mcg/L) data unit is ??g/dL couble check before use
    #convert unit from ug/dL to mcg/L
    FERRITIN_LBORRES = case_when(
      SITE != "Zambia" ~ 10*M08_FERRITIN_LBORRES_1, #Ghana and Kenya 
      SITE == "Zambia" ~ M08_FERRITIN_LBORRES_1
    ),
    CRIT_IRON = ifelse(FERRITIN_LBORRES > 15, 1,
                       ifelse(FERRITIN_LBORRES >0 & FERRITIN_LBORRES <= 15, 0,
                              ifelse(FERRITIN_LBORRES == 0, 0, 55))
    ),
    # G. no subclinical inflammation 
    CRIT_INFLAM = case_when(
      M08_CRP_LBORRES_1 > 0 & M08_CRP_LBORRES_1 <= 5 & M08_AGP_LBORRES_1 >0 & M08_AGP_LBORRES_1 <= 1 ~ 1,
      M08_CRP_LBORRES_1 > 5 | M08_AGP_LBORRES_1 > 1 ~ 0,
      M08_MN_LBPERF_12_1 == 0 | M08_MN_LBPERF_13_1 == 0 ~ 55,
      TRUE ~ 55
    )
  ) %>% 
  rowwise() %>% 
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
    CRIT_STILLBIRTH = ifelse(M04_STILLBIRTH_RPORRES_1 == 1, 0, 
                             ifelse(M04_PH_PREV_RPORRES_1 == 0 | 
                                      M04_PH_OTH_RPORRES_1 == 0 | 
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
    # I. No hemoglobinopathies: SS, SC, SE, EE, CC, SD-Punjab, Sβthal, Eβthal, Cβthal, CD-Punjab, ED-Punjab, D-D-Punjab, 
    # D-Punjabβthal, Thalassemia major, Thalassemia intermedia, glucose-6-phosphate dehydrogenase deficiency, or Alpha thalassemia
    CRIT_HEMOGLOBINOPATHIES = ifelse(M08_RBC_THALA_LBORRES_1 == 0 & M08_RBC_GLUC6_LBORRES_1 == 0, 1,
                                     ifelse(M08_RBC_THALA_LBORRES_1 == 1 | M08_RBC_GLUC6_LBORRES_1 == 1, 0,
                                            ifelse(M08_RBC_LBPERF_2_1 == 0 | M08_RBC_LBPERF_3_1 == 0, 55, 55))
    ),
    #J. No reported cigarette smoking, tobacco chewing, or betel nut use during pregnancy
    CRIT_SMOKE = case_when(
      SITE == "Zambia" & (M03_SMOKE_OECOCCUR_1 == 1 | M03_CHEW_OECOCCUR_1 == 1) ~ 0,
      SITE == "Zambia" & (M03_SMOKE_OECOCCUR_1 == 0 & M03_CHEW_OECOCCUR_1 == 0) ~ 1,
      M03_SMOKE_OECOCCUR_1 == 1 | M03_CHEW_BNUT_OECOCCUR_1 == 1 | M03_CHEW_OECOCCUR_1 == 1 ~ 0,
      M03_SMOKE_OECOCCUR_1 == 0 & M03_CHEW_BNUT_OECOCCUR_1 == 0 & M03_CHEW_OECOCCUR_1 == 0 ~ 1,
      TRUE ~ 55
    ),
    #K. No reported alcohol consumption during pregnancy
    CRIT_DRINK = ifelse(SITE == "Pakistan", 666,
                        ifelse(M03_DRINK_OECOCCUR_1 == 1, 0,
                               ifelse(M03_DRINK_OECOCCUR_1 == 0, 1,
                                      ifelse(M03_DRINK_OECOCCUR_1 == 66, 0,
                                             ifelse(M03_DRINK_OECOCCUR_1 == 77, 0, 55)))) #temporary code for Kenya, check for other country
    ), 
    #L. No known history or current chronic disease including cancer, kidney disease, and cardiac conditions
    CRIT_CHRONIC = ifelse(M04_CANCER_EVER_MHOCCUR_1 == 1 | M04_KIDNEY_EVER_MHOCCUR_1 == 1 | 
                            M04_CARDIAC_EVER_MHOCCUR_1 == 1, 0,
                          ifelse(M04_CANCER_EVER_MHOCCUR_1 == 0 & M04_KIDNEY_EVER_MHOCCUR_1 == 0 & 
                                   M04_CARDIAC_EVER_MHOCCUR_1 == 0, 1,
                                 ifelse(M04_CANCER_EVER_MHOCCUR_1 == 99 | M04_KIDNEY_EVER_MHOCCUR_1 == 99 | 
                                          M04_CARDIAC_EVER_MHOCCUR_1 == 99, 0, 55))
    ),
    #M. No known history or current HIV
               # if "Record HIV results" = positive, then 0 (ineligible) [M06_HIV_POC_LBORRES_1]
    CRIT_HIV = ifelse(M06_HIV_POC_LBORRES_1 == 1, 0,#Record HIV results (1,0)
                      
                      # if "Record HIV results" = negative, then 1 (eligible) [M06_HIV_POC_LBORRES_1]
                      ifelse(M06_HIV_POC_LBORRES_1 == 0, 1, 
                             
                             # if "Have you ever been diagnosed with HIV?" = yes or "HIV?" = yes, then 0 (ineligible) [M04_HIV_EVER_MHOCCUR_1, M04_HIV_MHOCCUR_1]
                             ifelse(M04_HIV_EVER_MHOCCUR_1 == 1 | #Have you ever been diagnosed with HIV? (1,0,99)
                                      M04_HIV_MHOCCUR_1 == 1, 0, #had HIV since becoming pregnant with the current pregnancy (1,0,99)
                                    
                                    # if "Have you ever been diagnosed with HIV?" = no AND [M04_HIV_EVER_MHOCCUR_1]
                                    # "HIV?" = no AND [M04_HIV_MHOCCUR_1]
                                    #  "Was point-of-care HIV test performed at this visit?" = no, then then 1 (eligible) [M06_HIV_POC_LBPERF_1]
                                    ifelse(M04_HIV_EVER_MHOCCUR_1 == 0 & M04_HIV_MHOCCUR_1 == 0 & M06_HIV_POC_LBPERF_1 == 0, 1,
                                           
                                           # if "Have you ever been diagnosed with HIV?" = don't know OR [M04_HIV_EVER_MHOCCUR_1]
                                           # "HIV?" = don't know OR [M04_HIV_MHOCCUR_1]
                                           #  "Was point-of-care HIV test performed at this visit?" = no, then then 0 (ineligible) [M06_HIV_POC_LBPERF_1]
                                           ifelse(M04_HIV_EVER_MHOCCUR_1 == 99 | M04_HIV_MHOCCUR_1 == 99 | 
                                                    M06_HIV_POC_LBPERF_1 == 0, 0, #Was point-of-care HIV test performed at this visit? (1,0)
                                                  
                                                  # if "Have you ever been diagnosed with HIV?" = 77/NA OR [M04_HIV_EVER_MHOCCUR_1]
                                                  # "HIV?" = 77/NA OR [M04_HIV_MHOCCUR_1]
                                                  #  "Was point-of-care HIV test performed at this visit?" = 77/NA OR [M06_HIV_POC_LBPERF_1]
                                                  # "Record HIV results" = 77/NA, then then 0 (ineligible) [M06_HIV_POC_LBORRES_1]
                                                  ifelse(M04_HIV_EVER_MHOCCUR_1 == 77 | M04_HIV_MHOCCUR_1 == 77 | 
                                                           M06_HIV_POC_LBPERF_1 == 77 | M06_HIV_POC_LBORRES_1 == 77, 55, 55))))) #Was point-of-care HIV test performed at this visit? (1,0)
    ),
    #N. No current malaria infection (per rapid diagnostic test)
    CRIT_MALARIA = case_when(
      M06_MALARIA_POC_LBORRES_1 == 1 ~ 0,
      M06_MALARIA_POC_LBORRES_1 == 0 ~ 1,
      M06_MALARIA_POC_LBPERF_1 == 0 ~ 0,
      TRUE ~ 55
    ),
    #O. No current Hepatitis B virus infection (per rapid diagnostic test)
    CRIT_HEPATITISB = ifelse(M06_HBV_POC_LBORRES_1 == 1, 0,
                             ifelse(M06_HBV_POC_LBORRES_1 == 0, 1,
                                    ifelse(M06_HBV_POC_LBPERF_1 == 0, 55, 55))
    ),
    #P. No current Hepatitis C virus infection (per rapid diagnostic test)
    CRIT_HEPATITISC = ifelse(M06_HCV_POC_LBORRES_1 == 1, 0,
                             ifelse(M06_HCV_POC_LBORRES_1 == 0, 1,
                                    ifelse(M06_HCV_POC_LBPERF_1 == 0, 55, 55)))
  ) 

test <- df_criteria %>% filter(SITE == "India-SAS") %>% select(contains("HIV"))
table(test$CRIT_HIV)

## QUESTION: 
# do they need to have both mnh04 and mnh06 criteria? for example, is sas has no hiv by mnho4 but 77 for all mnh06 variables - 
  ## the code right now is to have both form criteria met; is this necessary?

save(df_criteria, file= paste0(path_to_save, "df_criteria",".RData",sep = ""))
#**************************************************************************************
#*2. check eligibility and save df_healthy.rda
#**************************************************************************************
#code 666 for any not applicable by site
healthyOutcome <- df_criteria %>% 
  rowwise() %>%
  mutate(HEALTHY_CHECK = sum(across(starts_with("CRIT_"), ~ .x %in% c(1, 0, 666)), na.rm = TRUE)) %>% 
  mutate(HEALTHY_ELIGIBLE = case_when(
    if_all(starts_with("CRIT_"), ~.x %in% c(1, 666)) ~ 1, #eligible
    if_any(starts_with("CRIT_"), ~.x == 0) ~ 0, #Not eligible
    HEALTHY_CHECK < 19 ~ 3 #19 criteria
  ) ) %>%
  ungroup() 

df_healthy <- healthyOutcome %>% 
  filter(HEALTHY_ELIGIBLE == 1)
# table(df_healthy$SITE)

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
    C19 = ifelse(!is.na(CRIT_HEMOGLOBINOPATHIES),CRIT_HEMOGLOBINOPATHIES,55)
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

df_eli_long <- df_eli %>% 
  dplyr::select(-starts_with("CRIT_")) %>% 
  pivot_longer(cols = -c(MOMID, PREGID, SITE), 
               names_to = "Variable", 
               values_to = "Value")

#order variable value
df_eli_long$Variable <- factor(
  df_eli_long$Variable,
  levels = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", 
             "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19"))

save(df_eli_long, file= paste0(path_to_save, "df_eli_long",".RData",sep = ""))
