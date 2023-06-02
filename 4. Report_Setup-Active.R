#*****************************************************************************
#### MONITORING REPORT SETUP ####
## items to UPDATE EACH RUN: 
# read in correct data 
# upload date for MNH04 
# upload date for MNH12 
# upload date for MNH13
# upload date for MNH09 

## load in data 
rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)

UploadDate = "2023-05-26"

load(paste0("~/Monitoring Report/data/cleaned/", UploadDate, "/", "MatData_Wide_", UploadDate, ".RData"))
load(paste0("~/Monitoring Report/data/cleaned/", UploadDate, "/", "InfData_Wide_", UploadDate, ".RData"))


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


#*****************************************************************************
#* Extract variables for monitoring report 
#*****************************************************************************
#update/add/delete variable names in the varNames_sheet.xlsx (check if the var is multiple or singe when add)
MatNames_sheet <- read_excel("~/Monitoring Report/code/varNames_sheet.xlsx", sheet = "MaternalVars")
InfNames_sheet <- read_excel("~/Monitoring Report/code/varNames_sheet.xlsx", sheet = "InfantVars")

MatData_Report <- MatData_Wide %>% 
  select(matches(MatNames_sheet$varname), 
         GA_US_DAYS_1,BASELINEDATE_1,
         contains("_TYPE_VISIT_"), 
         contains("_VISIT_COMPLETE"), 
         contains("M04_ANC_OBSSTDAT_"),
         contains("M12_VISIT_OBSSTDAT_"),
         contains("_GA_AT_VISIT_DAYS_"),
         contains("_GA_LMP_WEEKS_SCORRES_"),
         contains("_GA_LMP_DAYS_SCORRES_"),
         M00_KNOWN_DOBYN_SCORRES_1) 

 setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
 save(MatData_Report, file= paste("MatData_Report","_",UploadDate, ".RData",sep = ""))
 
 InfData_Report <- InfData_Wide %>% 
   select(matches(InfNames_sheet$varname), 
          contains("_TYPE_VISIT_"), 
          contains("_VISIT_COMPLETE"),
          contains("_PNC_AT_VISIT_DAYS"),
          contains("_PNC_AT_VISIT_WKS")) %>% 
   mutate(DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")) 

 setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
 #dt=format(Sys.time(), "%Y-%m-%d")
 save(InfData_Report, file= paste("InfData_Report","_",UploadDate, ".RData",sep = ""))
 
 #**************************************************************************************
 #* PRiSMA Tables 1-3
 #* Table 1a: Pre-screening numbers for PRiSMA MNH Study for the most recent one week 
 #* Table 1b: Cumulative pre-screening numbers for PRiSMA MNH Study 
 #* Table 2a: Enrollment numbers for PRiSMA MNH Study for the most recent one week
 #* Table 2b: Cumulative enrollment numbers for PRiSMA MNH Study 
 #* Table 3: Study Status for PRiSMA MNH Study 
 
 #* Pre-screening, screen and enrollment info
 #* Output = MatData_Screen_Enroll 
 #**************************************************************************************
 #study start date and cut date
MatData_Screen_Enroll <- MatData_Report %>% 
  rowwise() %>% 
  mutate(START_DATE_MOM = (M02_SCRN_OBSSTDAT_1),
         START_DATE_MOM_MTH = floor_date(START_DATE_MOM, unit = "month"),
         START_DATE_MOM_WK = floor_date(START_DATE_MOM, unit = "week", week_start = getOption("lubridate.week.start",1)),
         START_DATE_MOM_DAY = floor_date(START_DATE_MOM, unit = "day")
  ) %>% 
  ungroup() %>% 
  #group_by("SITE") %>% 
  mutate(START_DATE=min(START_DATE_MOM, na.rm = TRUE),
         START_DATE_MTH = floor_date(START_DATE, unit = "month"),
         START_DATE_WK = floor_date(START_DATE, unit = "week", week_start = getOption("lubridate.week.start",1))) %>% 
  mutate(CUT_DATE = max((M02_SCRN_OBSSTDAT_1), na.rm = TRUE),
         CUT_DATE_MTH = floor_date(CUT_DATE, unit = "month"),
         CUT_DATE_WK = floor_date(CUT_DATE, unit = "week", week_start = getOption("lubridate.week.start",1))) %>% 
  ungroup() 

# enrollment criteria
MatData_Screen_Enroll <- MatData_Screen_Enroll %>% 
   # four types of consent
  mutate(
    # participant consent (has both willingness & written consent)
    CONSENT_PART = case_when(
      M00_CON_YN_DSDECOD_1 == 1 & !is.na(M00_CON_DSSTDAT_1) ~ 1,
      M00_CON_YN_DSDECOD_1 == 0 & !is.na(M00_CON_DSSTDAT_1) ~ 0,
      TRUE ~ 77),
    # legal representative consent (has both willingness & written consent) *optional
    CONSENT_LAR = case_when(
      M00_CON_LAR_YN_DSDECOD_1 == 1 & !is.na(M00_CON_LAR_SIGNDAT_1) ~ 1,  
      M00_CON_LAR_YN_DSDECOD_1 == 0 & !is.na(M00_CON_LAR_SIGNDAT_1) ~ 0,
      TRUE ~ 77),
    # participant assent (has both willingness & written assent) *optional
    ASSNT_PART = case_when(
      M00_ASSNT_YN_DSDECOD_1 == 1 & !is.na(M00_ASSNT_DSSTDAT_1) ~ 1,  
      M00_ASSNT_YN_DSDECOD_1 == 0 & !is.na(M00_ASSNT_DSSTDAT_1) ~ 0,
      TRUE ~ 77),
    # witness consent (has written consent) *optional
    CONSENT_WITNESS = case_when(
      M00_CON_WITNESS_SIGNATURE_PRYN_1 == 1 & !is.na(M00_CON_WITNESS_SIGNDAT_1) ~ 1,  
      M00_CON_WITNESS_SIGNATURE_PRYN_1 == 0 & !is.na(M00_CON_WITNESS_SIGNDAT_1) ~ 0,
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
      !is.na(M00_SCRN_OBSSTDAT_1) ~ 1, 
      TRUE ~ 0
    ),
    #gap between no other reason to exclude and provide consent info
    GAP_CONSENT = ifelse(M00_OTHR_IEORRES_1 == 0 & M00_CON_YN_DSDECOD_1 == 77, 1, 0),
    # expected enrollment
    EXPECT = case_when(
      SITE == "Pakistan" | SITE == "India" ~ 6000/1095, 
      SITE == "Ghana" | SITE == "Kenya" ~ 3000/1095, 
      SITE == "Zambia" ~ 2250/1095) ,
    # binary variable indicating if a woman was screened 
    SCREEN = case_when(!is.na(M02_FORMCOMPLDAT_MNH02_1) ~ 1, 
                       TRUE ~ 0),
    
    # eligible & enrolled 
    ## M02_AGE_IEORRES_1 = meet age requirement?
    ## M02_PC_IEORRES_1 = <20wks gestation?
    ## M02_CATCHMENT_IEORRES_1 = live in catchment area?
    ## M02_CATCH_REMAIN_IEORRES_1 = stay in catchment area?
    ELIGIBLE = case_when(M02_AGE_IEORRES_1 == 1 & M02_PC_IEORRES_1 == 1 & M02_CATCHMENT_IEORRES_1 == 1 &
                           M02_CATCH_REMAIN_IEORRES_1 == 1  ~ 1,
                           M02_AGE_IEORRES_1 == 0 | M02_PC_IEORRES_1 == 0 | M02_CATCHMENT_IEORRES_1 == 0 |
                           M02_CATCH_REMAIN_IEORRES_1 == 0  ~ 0,
                          TRUE ~ 99),
    CONSENT = case_when(!is.na(M02_CONSENT_IEORRES_1) ~ M02_CONSENT_IEORRES_1,
                        TRUE ~ 99),
    ENROLL = case_when(M02_CONSENT_IEORRES_1 == 1 & !is.na(M02_FORMCOMPLDAT_MNH02_1) & ELIGIBLE == 1 ~ 1, 
                       ELIGIBLE == 0 | M02_CONSENT_IEORRES_1 == 0 | M02_CONSENT_IEORRES_1 == 77 ~ 0,
                       TRUE ~ 77)
  ) %>% 
  ## Assign denominators 
  mutate(a = case_when(PRESCREEN == 1 ~ 1, 
                       TRUE ~ 0)) %>%      ## a = denominator for n all pre-screened 
  mutate(b = case_when(SCREEN == 1 ~ 1, 
                       TRUE ~ 0)) %>%      ## b = denominator for n all screened 
  mutate(c = case_when(ENROLL == 1 ~ 1, 
                       TRUE ~ 0))          ## c = denominator for all n enrolled 
  


## extract gestational age at enrollment 
MatData_Screen_Enroll <- MatData_Screen_Enroll %>% 
  rowwise() %>% 
  mutate(DIFFDAYS = as.numeric(difftime(M02_SCRN_OBSSTDAT_1, M01_US_OHOSTDAT_1, units = "days"))) %>% ## extract the difference in number of days between screening US and enrollment visit
  mutate(DIFFDAYS = ifelse(DIFFDAYS < 0, 0, DIFFDAYS)) %>%  ## if the difference is negative, replace with 0 (just use date of ultrasound screening)
  mutate(GA_ENROLL_WKS = floor(as.numeric((DIFFDAYS + GA_US_DAYS_1)/7)), 
         GA_ENROLL_DAYS = floor(as.numeric(DIFFDAYS + GA_US_DAYS_1)))

## extract Reasons for exclusion (check if cases match within mnh00 and 02)
MatData_Screen_Enroll <- MatData_Screen_Enroll %>% 
  #reason for exclusion in pre-screen
  mutate(
    PRESCR_PREGSIGN = ifelse(M00_PREGNANT_IEORRES_1 == 1, 1,
                             ifelse(M00_PREGNANT_IEORRES_1 == 0, 0, 99)),
    
    ## if you answer 1 to PRESCR_PREGSIGN, you answer PRESCR_GA25
    PRESCR_GA25 = ifelse(M00_EGA_LT25_IEORRES_1 == 1, 1,
                             ifelse(M00_EGA_LT25_IEORRES_1 == 0 & M00_PREGNANT_IEORRES_1 == 1, 0, 99)),
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25, you answer PRESCR_AGE
    PRESCR_AGE = ifelse(M00_AGE_IEORRES_1 == 1, 1,
                         ifelse(M00_AGE_IEORRES_1 == 0  & M00_PREGNANT_IEORRES_1 == 1 & 
                                M00_EGA_LT25_IEORRES_1 == 1, 0, 99)),
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25 & PRESCR_AGE, you answer PRESCR_CATCHAREA
    PRESCR_CATCHAREA = ifelse(M00_CATCHMENT_IEORRES_1 == 1, 1,
                        ifelse(M00_CATCHMENT_IEORRES_1 == 0 & 
                                 M00_AGE_IEORRES_1 == 1  & 
                                 M00_PREGNANT_IEORRES_1 == 1 & 
                                 M00_EGA_LT25_IEORRES_1 == 1, 0, 99)),
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25 & PRESCR_AGE & PRESCR_CATCHAREA, you answer PRESCR_OTHER
    PRESCR_OTHER = ifelse(M00_OTHR_IEORRES_1 == 0, 1,
                              ifelse(M00_OTHR_IEORRES_1 == 1 & 
                                       M00_CATCHMENT_IEORRES_1 == 1 & 
                                       M00_AGE_IEORRES_1 == 1  & 
                                       M00_PREGNANT_IEORRES_1 == 1 & 
                                       M00_EGA_LT25_IEORRES_1 == 1, 0, 99)), 
    ## if you answer 1 to PRESCR_PREGSIGN & PRESCR_GA25 & PRESCR_AGE & PRESCR_CATCHAREA & PRESCR_OTHER, you answer CONSENT_PRESCREEN
    CONSENT_PRESCREEN = ifelse(M00_CON_YN_DSDECOD_1 == 1, 1, 
                               ifelse(M00_CON_YN_DSDECOD_1 == 0 & 
                                        M00_OTHR_IEORRES_1 == 0 & 
                                        M00_CATCHMENT_IEORRES_1 == 1 & 
                                        M00_AGE_IEORRES_1 == 1  & 
                                        M00_PREGNANT_IEORRES_1 == 1 & 
                                        M00_EGA_LT25_IEORRES_1 == 1, 0, 99))) %>% 
  # eligible based on pre-screening
  mutate(PRESCR_ELIGIBLE = ifelse(M00_PREGNANT_IEORRES_1 == 1 & 
                                M00_EGA_LT25_IEORRES_1 == 1 & 
                                M00_AGE_IEORRES_1 == 1 &
                                M00_CATCHMENT_IEORRES_1 == 1 & 
                                M00_OTHR_IEORRES_1 == 0 & 
                                M00_CON_YN_DSDECOD_1 == 1, 1,
                            ifelse(M00_PREGNANT_IEORRES_1 == 0 | M00_EGA_LT25_IEORRES_1 == 0 | M00_AGE_IEORRES_1 == 0 |
                                  M00_CATCHMENT_IEORRES_1 == 0 | M00_OTHR_IEORRES_1 == 1 | M00_CON_YN_DSDECOD_1 == 0, 0, 99)
                              )) %>% 
    
  #reason for exclusion in screen/enroll
  mutate(
    AGE = ifelse(M02_AGE_IEORRES_1 == 1, 1,
                 ifelse(M02_AGE_IEORRES_1 == 0, 0, 99)),
    GA20 = ifelse(M02_PC_IEORRES_1 == 1, 1,
                 ifelse(M02_PC_IEORRES_1 == 0 & 
                        M02_AGE_IEORRES_1 == 1, 0, 99)),
    
    CATCHAREA = ifelse(M02_CATCHMENT_IEORRES_1 == 1, 1,
                  ifelse(M02_CATCHMENT_IEORRES_1 == 0 &
                           M02_PC_IEORRES_1 == 1 & 
                           M02_AGE_IEORRES_1 == 1, 0, 99)),
    CATCHREMAIN = ifelse(M02_CATCH_REMAIN_IEORRES_1 == 1, 1, 
                         ifelse(M02_CATCH_REMAIN_IEORRES_1 == 0 & 
                                  M02_CATCHMENT_IEORRES_1 == 1 &
                                  M02_PC_IEORRES_1 == 1 & 
                                  M02_AGE_IEORRES_1 == 1, 0, 99)), 
    SCRN_CONSENT = ifelse(M02_CONSENT_IEORRES_1 == 1, 1, 
                          ifelse(M02_CONSENT_IEORRES_1 == 0 & 
                                   M02_CATCH_REMAIN_IEORRES_1 == 1 & 
                                   M02_CATCHMENT_IEORRES_1 == 1 &
                                   M02_PC_IEORRES_1 == 1 & 
                                   M02_AGE_IEORRES_1 == 1, 0, 99))) 

## Has woman closed out? 
MatData_Screen_Enroll <- MatData_Screen_Enroll %>% 
  mutate(M23_VISIT_COMPLETE = ifelse(M23_CLOSE_DSDECOD == 1 | M23_CLOSE_DSDECOD == 2 | M23_CLOSE_DSDECOD == 3 |
                                     M23_CLOSE_DSDECOD == 4 | M23_CLOSE_DSDECOD == 5 | M23_CLOSE_DSDECOD == 9,1,0)) %>% 
  ## CLOSEOUT YES/NO 
  mutate(MATERNAL_CLOSEOUT_YN = ifelse(M23_CLOSE_DSDECOD == 1 | M23_CLOSE_DSDECOD == 2 | 
                                         M23_CLOSE_DSDECOD == 3 | M23_CLOSE_DSDECOD == 4 | 
                                         M23_CLOSE_DSDECOD == 5 | M23_CLOSE_DSDECOD == 6, 1, 0)) 
  

## export 
setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
save(MatData_Screen_Enroll, file= paste("MatData_Screen_Enroll","_",UploadDate, ".RData",sep = ""))

#**************************************************************************************
## INFANT DATA PNC VISITS

#* Input = InfData_Report &  
#* Output = InfData_Pnc_Visits 
#**************************************************************************************
# ## INFANT DATA PNC VISITS 
# ## get latest M13 Date 
# ## get vector of the latest visit date for MNH12 visits 
M13_VISIT_VEC <- c("M13_VISIT_OBSSTDAT_7", "M13_VISIT_OBSSTDAT_8", "M13_VISIT_OBSSTDAT_9","M13_VISIT_OBSSTDAT_10", "M13_VISIT_OBSSTDAT_11", "M13_VISIT_OBSSTDAT_12")
out_pnc_visits <- list()
for(i in M13_VISIT_VEC){
  out_pnc_visits[[i]] <- InfData_Report %>%
    select(i) %>%
    pivot_longer(everything()) %>%
    filter(value < '2023-05-12') %>% ## UPDATE EACH RUN
    summarise(x = max(value, na.rm = TRUE)) %>% distinct() %>% pull()
}


InfData_Report$LATEST_M13_V7DATE = out_pnc_visits[["M13_VISIT_OBSSTDAT_7"]]
InfData_Report$LATEST_M13_V8DATE = out_pnc_visits[["M13_VISIT_OBSSTDAT_8"]]
InfData_Report$LATEST_M13_V9DATE = out_pnc_visits[["M13_VISIT_OBSSTDAT_9"]]
InfData_Report$LATEST_M13_V10DATE = out_pnc_visits[["M13_VISIT_OBSSTDAT_10"]]
InfData_Report$LATEST_M13_V11DATE = out_pnc_visits[["M13_VISIT_OBSSTDAT_11"]]
InfData_Report$LATEST_M13_V12DATE = out_pnc_visits[["M13_VISIT_OBSSTDAT_12"]]

# assign late windows x 
# generate overdue vars 
# get denominators 
InfData_Pnc_Visits <- InfData_Report %>% 
  filter(M11_INF_DSTERM == 1) %>% ## only want live births 
  ## CALCULATE ON TIME AND LATE PNC WINDOWS 
  mutate(PNC0_ONTIME = DELIVERY_DATETIME + as.difftime(5, unit="days"),
         PNC0_LATE = DELIVERY_DATETIME + as.difftime(5, unit="days"),
         PNC1_ONTIME = DELIVERY_DATETIME + as.difftime(14, unit="days"),
         PNC1_LATE = DELIVERY_DATETIME + as.difftime(14, unit="days"),
         PNC4_ONTIME = DELIVERY_DATETIME + as.difftime(35, unit="days"),
         PNC4_LATE = DELIVERY_DATETIME + as.difftime(35, unit="days"),
         PNC6_ONTIME = DELIVERY_DATETIME + as.difftime(55, unit="days"),
         PNC6_LATE = DELIVERY_DATETIME + as.difftime(104, unit="days"),
         PNC26_ONTIME = DELIVERY_DATETIME + as.difftime(202, unit="days"),
         PNC26_LATE = DELIVERY_DATETIME + as.difftime(279, unit="days"),
         PNC52_ONTIME = DELIVERY_DATETIME + as.difftime(384, unit="days"),
         PNC52_LATE = DELIVERY_DATETIME + as.difftime(454, unit="days")) %>%
  ## CALCULATE INDICATOR VARIABLES for missed PNC visits 
  mutate(PNC0_OVERDUE = ifelse(LATEST_M13_V7DATE>PNC0_ONTIME & M13_VISIT_COMPLETE_7 == 0, 1, 0),
         PNC1_OVERDUE = ifelse(LATEST_M13_V8DATE>PNC1_ONTIME & is.na(M13_VISIT_COMPLETE_8), 1, 0),
         PNC4_OVERDUE = ifelse(LATEST_M13_V9DATE>PNC4_ONTIME & is.na(M13_VISIT_COMPLETE_9), 1, 0),
         PNC6_OVERDUE = ifelse(LATEST_M13_V10DATE>PNC6_ONTIME & is.na(M13_VISIT_COMPLETE_10), 1, 0),
         PNC26_OVERDUE = ifelse(LATEST_M13_V11DATE>PNC26_ONTIME & is.na(M13_VISIT_COMPLETE_11), 1, 0),
         PNC52_OVERDUE = ifelse(LATEST_M13_V12DATE>PNC52_ONTIME & is.na(M13_VISIT_COMPLETE_12), 1, 0)) %>% 
  ## CALCULATE PNC DENOMINATORS 
  mutate( allbirths = ifelse(M11_INF_DSTERM == 1, 1, 0), ## PNC0 
          h = ifelse((M13_VISIT_COMPLETE_7 == 1 & M13_TYPE_VISIT_7 == 7) |  PNC1_OVERDUE == 1, 1, 0),
          i = ifelse((M13_VISIT_COMPLETE_8 == 1 & M13_TYPE_VISIT_8 == 8) |  PNC1_OVERDUE == 1, 1, 0),
          j = ifelse((M13_VISIT_COMPLETE_9 == 1 & M13_TYPE_VISIT_9 == 9) |  PNC4_OVERDUE == 1, 1, 0),
          k = ifelse((M13_VISIT_COMPLETE_10 == 1 & M13_TYPE_VISIT_10 == 10) |  PNC6_OVERDUE == 1, 1, 0),
          l = ifelse((M13_VISIT_COMPLETE_11 == 1 & M13_TYPE_VISIT_11 == 11) |  PNC26_OVERDUE == 1, 1, 0),
          m = ifelse((M13_VISIT_COMPLETE_12 == 1 & M13_TYPE_VISIT_12 == 12) |  PNC52_OVERDUE == 1, 1, 0))

setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
dt=format(Sys.time(), "%Y-%m-%d")
save(InfData_Pnc_Visits, file= paste("InfData_Pnc_Visits","_", UploadDate, ".RData",sep = ""))



#**************************************************************************************
### MatData_Anc_Visits
# input: MatData_Screen_Enroll
# Includes all women who are enrolled. Includes constructed variables relevant to ANC 

#**************************************************************************************
## Extract minimum EDD 
MatData_Anc_Visits <- MatData_Screen_Enroll %>% 
  filter(ENROLL == 1) %>% 
  rowwise() %>% 
  ## get min EDD 
  mutate(M01_US_EDD_BRTHDAT_FTS1_1 = replace(M01_US_EDD_BRTHDAT_FTS1_1, M01_US_EDD_BRTHDAT_FTS1_1== ymd("1907-07-07"), NA), 
         M01_US_EDD_BRTHDAT_FTS2_1 = replace(M01_US_EDD_BRTHDAT_FTS2_1, M01_US_EDD_BRTHDAT_FTS2_1==ymd("1907-07-07"), NA),
         M01_US_EDD_BRTHDAT_FTS3_1 = replace(M01_US_EDD_BRTHDAT_FTS3_1, M01_US_EDD_BRTHDAT_FTS3_1== ymd("1907-07-07"), NA), 
         M01_US_EDD_BRTHDAT_FTS4_1 = replace(M01_US_EDD_BRTHDAT_FTS4_1, M01_US_EDD_BRTHDAT_FTS4_1==ymd("1907-07-07"), NA)) %>% 
  mutate(EDD = pmin(M01_US_EDD_BRTHDAT_FTS1_1, M01_US_EDD_BRTHDAT_FTS2_1, 
                    M01_US_EDD_BRTHDAT_FTS3_1, M01_US_EDD_BRTHDAT_FTS4_1,  na.rm = TRUE))  %>% 
  ## CALCULATE ON TIME AND LATE ANC WINDOWS 
  mutate(ENROLL_ONTIME = (EDD - as.difftime(280, unit="days")) + as.difftime(139, unit="days"), 
         ENROLL_LATE = (EDD - as.difftime(280, unit="days")) + as.difftime(139, unit="days"), 
         ANC20_ONTIME = (EDD - as.difftime(280, unit="days")) + as.difftime(160, unit="days"), 
         ANC20_LATE = (EDD - as.difftime(280, unit="days")) + as.difftime(181, unit="days"),
         ANC28_ONTIME = (EDD - as.difftime(280, unit="days")) + as.difftime(216, unit="days"), 
         ANC28_LATE = (EDD - as.difftime(280, unit="days")) + as.difftime(216, unit="days"), 
         ANC32_ONTIME = (EDD - as.difftime(280, unit="days")) + as.difftime(237, unit="days"), 
         ANC32_LATE = (EDD - as.difftime(280, unit="days")) + as.difftime(244, unit="days"), ## will need to ask about this
         ANC36_ONTIME = (EDD - as.difftime(280, unit="days")) + as.difftime(272, unit="days"), 
         ANC36_LATE = (EDD - as.difftime(280, unit="days")) + as.difftime(300, unit="days")) %>% 
  ## CALCULATE INDICATOR VARIABLES for missed ANC visits ON-TIME WINDOWS
  ## max date has to be less than the upload date
  mutate(ENROLL_OVERDUE = ifelse(UploadDate>ENROLL_ONTIME & is.na(M04_VISIT_COMPLETE_1), 1, 0),
         ANC20_OVERDUE = ifelse(UploadDate>ANC20_ONTIME & is.na(M04_VISIT_COMPLETE_2) & GA_ENROLL_WKS <= 17, 1, 0),
         ANC28_OVERDUE = ifelse(UploadDate>ANC28_ONTIME & is.na(M04_VISIT_COMPLETE_3), 1, 0),
         ANC32_OVERDUE = ifelse(UploadDate>ANC32_ONTIME & is.na(M04_VISIT_COMPLETE_4), 1, 0),
         ANC36_OVERDUE = ifelse(UploadDate>ANC36_ONTIME & is.na(M04_VISIT_COMPLETE_5), 1, 0)) %>% 
  ## CALCULATE INDICATOR VARIABLES for passed ON-TIME ANC WINDOWS - same as overdue code, but exclude the visit completion piece
  ## using upload date
  mutate(ENROLL_PASS = ifelse(ENROLL_ONTIME<UploadDate, 1, 0),
         ANC20_PASS = ifelse(ANC20_ONTIME<UploadDate & GA_ENROLL_WKS <= 17, 1, 0),
         ANC28_PASS = ifelse(ANC28_ONTIME<UploadDate, 1, 0),
         ANC32_PASS = ifelse(ANC32_ONTIME<UploadDate, 1, 0),
         ANC36_PASS = ifelse(ANC36_ONTIME<UploadDate, 1, 0)) %>% 
  ## CALCULATE INDICATOR VARIABLES for missed PASSED LATE WINDOWS
  mutate(ENROLL_PASS_LATE = ifelse(ENROLL_LATE<UploadDate, 1, 0),
         ANC20_PASS_LATE = ifelse(ANC20_LATE<UploadDate & GA_ENROLL_WKS <= 17, 1, 0),
         ANC28_PASS_LATE = ifelse(ANC28_LATE<UploadDate, 1, 0),
         ANC32_PASS_LATE = ifelse(ANC32_LATE<UploadDate, 1, 0),
         ANC36_PASS_LATE = ifelse(ANC36_LATE<UploadDate, 1, 0)) %>% 
  ## CALCULATE INDICATOR VARIALBE FOR ANY VISIT TYPE = i
  mutate(ANY_TYPE_VISIT_COMPLETE_1 = ifelse((M01_TYPE_VISIT_1 == 1 & M01_VISIT_COMPLETE_1 == 1) | 
                                              (M03_TYPE_VISIT_1 == 1 & M03_VISIT_COMPLETE_1 == 1) |
                                              (M04_TYPE_VISIT_1 == 1 & M04_VISIT_COMPLETE_1 == 1) |
                                              (M05_TYPE_VISIT_1 == 1 & M05_VISIT_COMPLETE_1 == 1) |
                                              (M06_TYPE_VISIT_1 == 1 & M06_VISIT_COMPLETE_1 == 1) |
                                              (M07_TYPE_VISIT_1 == 1 & M07_VISIT_COMPLETE_1 == 1) |
                                              (M08_TYPE_VISIT_1 == 1 & M08_VISIT_COMPLETE_1 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_2 = ifelse((M04_TYPE_VISIT_2 == 2 & M04_VISIT_COMPLETE_2 == 1 & GA_ENROLL_WKS <= 17) |
                                              (M05_TYPE_VISIT_2 == 2 & M05_VISIT_COMPLETE_2 == 1 & GA_ENROLL_WKS <= 17) |
                                              (M06_TYPE_VISIT_2 == 2 & M06_VISIT_COMPLETE_2 == 1 & GA_ENROLL_WKS <= 17) |
                                              (M07_TYPE_VISIT_2 == 2 & M07_VISIT_COMPLETE_2 == 1 & GA_ENROLL_WKS <= 17) |
                                              (M08_TYPE_VISIT_2 == 2 & M08_VISIT_COMPLETE_2 == 1 & GA_ENROLL_WKS <= 17) | 
                                              (M25_TYPE_VISIT_2 == 2 & M25_VISIT_COMPLETE_2 == 1 & GA_ENROLL_WKS <= 17) | 
                                              (M26_TYPE_VISIT_2 == 2 & M26_VISIT_COMPLETE_2 == 1 & GA_ENROLL_WKS <= 17), 1, 0),
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
                                              (M08_TYPE_VISIT_5 == 5 & M08_VISIT_COMPLETE_5 == 1), 1, 0))

setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
dt=format(Sys.time(), "%Y-%m-%d")
save(MatData_Anc_Visits, file= paste("MatData_Anc_Visits","_", UploadDate, ".RData",sep = ""))


#**************************************************************************************
### MatData_Pnc_Visits
# input: MatData_Screen_Enroll
# Includes all women who are enrolled. Includes constructed variables relevant to PNC 

#**************************************************************************************

## MatData_Pnc_Visits
MatData_Pnc_Visits <- MatData_Screen_Enroll %>% 
  select(SITE,SCRNID, MOMID, PREGID, ENROLL, DOB, GA_ENROLL_WKS, GA_ENROLL_DAYS, 
         ends_with("_7"),  ends_with("_8"),  ends_with("_9"), 
         ends_with("_10"), ends_with("_11"), ends_with("_12"),starts_with("M09_BIRTH_DSTERM_INF"), M23_CLOSE_DSDECOD) %>% 
  #filter(!is.na(DOB)) %>% ## only include participants who have a DOB 
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
  ## CALCULATE INDICATOR VARIABLES for missed PNC visits 
  ## max date has to be less than the upload date
  mutate(PNC0_OVERDUE = ifelse(UploadDate>PNC0_ONTIME & is.na(M12_VISIT_COMPLETE_7) == 0, 1, 0),
         PNC1_OVERDUE = ifelse(UploadDate>PNC1_ONTIME & is.na(M12_VISIT_COMPLETE_8), 1, 0),
         PNC4_OVERDUE = ifelse(UploadDate>PNC4_ONTIME & is.na(M12_VISIT_COMPLETE_9), 1, 0),
         PNC6_OVERDUE = ifelse(UploadDate>PNC6_ONTIME & is.na(M12_VISIT_COMPLETE_10), 1, 0),
         PNC26_OVERDUE = ifelse(UploadDate>PNC26_ONTIME & is.na(M12_VISIT_COMPLETE_11), 1, 0),
         PNC52_OVERDUE = ifelse(UploadDate>PNC52_ONTIME & is.na(M12_VISIT_COMPLETE_12), 1, 0)) %>% 
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
  ## EXTRACT BIRTHOUTCOME
  mutate(BIRTH_OUTCOME_YN = ifelse(M09_BIRTH_DSTERM_INF1_6 == 1 | M09_BIRTH_DSTERM_INF1_6 == 2 |
                                     M09_BIRTH_DSTERM_INF2_6 == 1 | M09_BIRTH_DSTERM_INF2_6 == 2 |
                                     M09_BIRTH_DSTERM_INF3_6 == 1 | M09_BIRTH_DSTERM_INF3_6 == 2 |
                                     M09_BIRTH_DSTERM_INF4_6 == 1 | M09_BIRTH_DSTERM_INF4_6 == 2, 1, 0))  

setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
dt=format(Sys.time(), "%Y-%m-%d")
save(MatData_Pnc_Visits, file= paste("MatData_Pnc_Visits","_", UploadDate, ".RData",sep = ""))

#**************************************************************************************
#* ReMAPP Table 3 + 4: Hemoglobin measurements for participants in ReMAPP per visit 
#* Only looking at those who are enrolled 
#* Output = MatData_Hb_VISIT
#* Input = MatData_Anc_Visits
#**************************************************************************************
MatData_Hb_Visit <- MatData_Anc_Visits %>% 
  rowwise() %>% 
  mutate(HB_COMPLETED_1 = ifelse(M08_CBC_LBPERF_1_1 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_1 == 0, 0, 99)),
         HB_COMPLETED_2 = ifelse(M08_CBC_LBPERF_1_2 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_2 == 0, 0, 99)),
         HB_COMPLETED_3 = ifelse(M08_CBC_LBPERF_1_3 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_3 == 0, 0, 99)),
         HB_COMPLETED_4 = ifelse(M08_CBC_LBPERF_1_4 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_4 == 0, 0, 99)),
         HB_COMPLETED_5 = ifelse(M08_CBC_LBPERF_1_5 == 1, 1, 
                                 ifelse(M08_CBC_LBPERF_1_5 == 0, 0, 99))) %>% 
  # # replace outliars with NA
  mutate(M08_CBC_HB_LBORRES_1 = ifelse(M08_CBC_HB_LBORRES_1 < 1 | M08_CBC_HB_LBORRES_1 > 20, NA, M08_CBC_HB_LBORRES_1),
         M08_CBC_HB_LBORRES_2 = ifelse(M08_CBC_HB_LBORRES_2 < 1 | M08_CBC_HB_LBORRES_2 > 20, NA, M08_CBC_HB_LBORRES_2),
         M08_CBC_HB_LBORRES_3 = ifelse(M08_CBC_HB_LBORRES_3 < 1 | M08_CBC_HB_LBORRES_3 > 20, NA, M08_CBC_HB_LBORRES_3),
         M08_CBC_HB_LBORRES_4 = ifelse(M08_CBC_HB_LBORRES_4 < 1 | M08_CBC_HB_LBORRES_4 > 20, NA, M08_CBC_HB_LBORRES_4),
         M08_CBC_HB_LBORRES_5 = ifelse(M08_CBC_HB_LBORRES_5 < 1 | M08_CBC_HB_LBORRES_5 > 20, NA, M08_CBC_HB_LBORRES_5)) %>%
  # HB_COMPLETED_10 = case_when(M08_CBC_LBPERF_10 == 1 ~ 1 ## add in once we have data
  #                            M08_CBC_LBPERF_10 == 0 ~ 0,
  #                            TRUE ~ 99)) 
  ## generate denominators 
  mutate(DenHBV1 = ifelse((M08_VISIT_COMPLETE_1 == 1 & M08_TYPE_VISIT_1 == 1) |  ENROLL_OVERDUE == 1, 1, 0),
         DenHBV2 = ifelse((M08_VISIT_COMPLETE_2 == 1 & M08_TYPE_VISIT_2 == 2 & GA_ENROLL_WKS <= 17) |  ANC20_OVERDUE == 1, 1, 0),
         DenHBV3 = ifelse((M08_VISIT_COMPLETE_3 == 1 & M08_TYPE_VISIT_3 == 3) |  ANC28_OVERDUE == 1, 1, 0),
         DenHBV4 = ifelse((M08_VISIT_COMPLETE_4 == 1 & M08_TYPE_VISIT_4 == 4) |  ANC32_OVERDUE == 1, 1, 0),
         DenHBV5 = ifelse((M08_VISIT_COMPLETE_5 == 1 & M08_TYPE_VISIT_5 == 5) |  ANC36_OVERDUE == 1, 1, 0)) %>% 
  mutate(REMAPP_LAUNCH = ifelse((SITE == "Kenya" & M02_SCRN_OBSSTDAT_1 >= "2023-04-10") |
                                  (SITE == "Pakistan" & M02_SCRN_OBSSTDAT_1 >= "2022-09-22") |
                                  (SITE == "Ghana" & M02_SCRN_OBSSTDAT_1 >= "2022-12-28"), 1, 0)) %>%
  filter(ENROLL == 1 & REMAPP_LAUNCH ==1)


setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
save(MatData_Hb_Visit, file= paste("MatData_Hb_Visits","_", UploadDate, ".RData",sep = ""))

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
  mutate(REMAPP_LAUNCH = ifelse((SITE == "Kenya" & M02_SCRN_OBSSTDAT_1 >= "2023-04-10") |
                                  (SITE == "Pakistan" & M02_SCRN_OBSSTDAT_1 >= "2022-09-22") |
                                  (SITE == "Ghana" & M02_SCRN_OBSSTDAT_1 >= "2022-12-28"), 1, 0)) %>%
  filter(ENROLL == 1 & REMAPP_LAUNCH ==1) 

MatData_Hb_GA_Visit_1 <- MatData_Anc_Remapp %>% 
  select(SITE, MOMID, PREGID, M08_CBC_HB_LBORRES_1,
         M06_SPHB_LBORRES_1,  M06_HB_POC_LBORRES_1, M06_GA_AT_VISIT_DAYS_1) %>% 
  mutate(TYPE_VISIT = 1) %>% 
  mutate(CBC = replace(M08_CBC_HB_LBORRES_1, M08_CBC_HB_LBORRES_1 == -7, NA ),
         SPHB = replace(M06_SPHB_LBORRES_1, M06_SPHB_LBORRES_1 == -7, NA ),
         POC = replace(M06_HB_POC_LBORRES_1, M06_HB_POC_LBORRES_1 == -7, NA ),
         GA = M06_GA_AT_VISIT_DAYS_1) %>% 
  select(SITE, MOMID, PREGID,TYPE_VISIT, CBC, SPHB, POC, GA)

MatData_Hb_GA_Visit_2 <- MatData_Anc_Remapp %>% 
  select(SITE, MOMID, PREGID, M08_CBC_HB_LBORRES_2,
         M06_SPHB_LBORRES_2,  M06_HB_POC_LBORRES_2, M06_GA_AT_VISIT_DAYS_2) %>% 
  mutate(TYPE_VISIT = 2) %>% 
  mutate(CBC = replace(M08_CBC_HB_LBORRES_2, M08_CBC_HB_LBORRES_2 == -7, NA ),
         SPHB = replace(M06_SPHB_LBORRES_2, M06_SPHB_LBORRES_2 == -7, NA ),
         POC = replace(M06_HB_POC_LBORRES_2, M06_HB_POC_LBORRES_2 == -7, NA ),
         GA = M06_GA_AT_VISIT_DAYS_2) %>% 
  select(SITE, MOMID, PREGID,TYPE_VISIT, CBC, SPHB, POC, GA)

MatData_Hb_GA_Visit_3 <- MatData_Anc_Remapp %>% 
  select(SITE, MOMID, PREGID, M08_CBC_HB_LBORRES_3,
         M06_SPHB_LBORRES_3,  M06_HB_POC_LBORRES_3, M06_GA_AT_VISIT_DAYS_3) %>% 
  mutate(TYPE_VISIT = 3) %>% 
  mutate(CBC = replace(M08_CBC_HB_LBORRES_3, M08_CBC_HB_LBORRES_3 == -7, NA ),
         SPHB = replace(M06_SPHB_LBORRES_3, M06_SPHB_LBORRES_3 == -7, NA ),
         POC = replace(M06_HB_POC_LBORRES_3, M06_HB_POC_LBORRES_3 == -7, NA ),
         GA = M06_GA_AT_VISIT_DAYS_3) %>% 
  select(SITE, MOMID, PREGID,TYPE_VISIT, CBC, SPHB, POC, GA)

MatData_Hb_GA_Visit_4 <- MatData_Anc_Remapp %>% 
  select(SITE, MOMID, PREGID, M08_CBC_HB_LBORRES_4,
         M06_SPHB_LBORRES_4,  M06_HB_POC_LBORRES_4, M06_GA_AT_VISIT_DAYS_4) %>% 
  mutate(TYPE_VISIT = 4) %>% 
  mutate(CBC = replace(M08_CBC_HB_LBORRES_4, M08_CBC_HB_LBORRES_4 == -7, NA ),
         SPHB = replace(M06_SPHB_LBORRES_4, M06_SPHB_LBORRES_4 == -7, NA ),
         POC = replace(M06_HB_POC_LBORRES_4, M06_HB_POC_LBORRES_4 == -7, NA ),
         GA = M06_GA_AT_VISIT_DAYS_4) %>% 
  select(SITE, MOMID, PREGID,TYPE_VISIT, CBC, SPHB, POC, GA)

MatData_Hb_GA_Visit_5 <- MatData_Anc_Remapp %>% 
  select(SITE, MOMID, PREGID, M08_CBC_HB_LBORRES_5,
         M06_SPHB_LBORRES_5,  M06_HB_POC_LBORRES_5, M06_GA_AT_VISIT_DAYS_5) %>% 
  mutate(TYPE_VISIT = 5) %>% 
  mutate(CBC = replace(M08_CBC_HB_LBORRES_5, M08_CBC_HB_LBORRES_5 == -7, NA ),
         SPHB = replace(M06_SPHB_LBORRES_5, M06_SPHB_LBORRES_5 == -7, NA ),
         POC = replace(M06_HB_POC_LBORRES_5, M06_HB_POC_LBORRES_5 == -7, NA ),
         GA = M06_GA_AT_VISIT_DAYS_5) %>% 
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


setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
save(MatData_Hb_GA_Visit, file= paste("MatData_Hb_GA_Visit","_", UploadDate, ".RData",sep = ""))

#**************************************************************************************
#*ReMAPP 
#*Output: healthyOutcome.rda
#*includes: CRIT_s, HEALTHY_ELIGIBLE
#**************************************************************************************
#*Create Variables for Aim 1 Criteria (21 vars)

vars_criteria <- MatData_Anc_Visits %>%
  select(
    SCRNID, MOMID, PREGID, SITE, ENROLL, M00_KNOWN_DOBYN_SCORRES_1,
    M00_BRTHDAT_1, M00_ESTIMATED_AGE_1, M02_SCRN_OBSSTDAT_1, M02_CONSENT_IEORRES_1,
    M03_SMOKE_OECOCCUR_1, M03_CHEW_BNUT_OECOCCUR_1, M03_CHEW_OECOCCUR_1, M03_DRINK_OECOCCUR_1,
    M05_ANT_PEDAT_1, M05_WEIGHT_PERES_1, M05_HEIGHT_PERES_1, M05_MUAC_PERES_1,
    GA_US_DAYS_1,
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
    M06_HBV_POC_LBORRES_1, M06_HBV_POC_LBPERF_1, M06_HCV_POC_LBORRES_1, M06_HCV_POC_LBPERF_1,
    M06_HIV_POC_LBORRES_1, M06_HIV_POC_LBPERF_1,
    M08_MN_LBPERF_8_1, M08_FERRITIN_LBORRES_1, 
    M08_RBC_LBPERF_2_1, M08_RBC_THALA_LBORRES_1, M08_RBC_LBPERF_3_1, M08_RBC_GLUC6_LBORRES_1,
    M08_MN_LBPERF_12_1, M08_CRP_LBORRES_1, M08_MN_LBPERF_13_1, M08_AGP_LBORRES_1, EDD,BASELINEDATE_1
  ) %>%
# mutate(REMAPP_LAUNCH = ifelse((SITE == "Kenya" & M02_SCRN_OBSSTDAT_1 >= "2023-04-10") |
#                                (SITE == "Pakistan" & M02_SCRN_OBSSTDAT_1 >= "2022-09-22") |
#                                (SITE == "Ghana" & M02_SCRN_OBSSTDAT_1 >= "2022-12-28"), 1, 0)) %>%
  filter(ENROLL == 1) %>%  #& REMAPP_LAUNCH ==1) %>%  
  mutate(
    # 1. age at enrollment
    # age at enrollment
    AGE_ENROLL = ifelse(M00_KNOWN_DOBYN_SCORRES_1 == 1 &  M00_BRTHDAT_1 != "1907-07-07", 
                        as.numeric(difftime(M02_SCRN_OBSSTDAT_1,M00_BRTHDAT_1, units = "days")/365),
                 ifelse(M00_KNOWN_DOBYN_SCORRES_1 == 0 & M00_ESTIMATED_AGE_1 != -7, M00_ESTIMATED_AGE_1, 99)),
    
    CRIT_AGE = ifelse((AGE_ENROLL > 0 & AGE_ENROLL < 18) | AGE_ENROLL > 34, 0,
               ifelse(AGE_ENROLL >= 18 & AGE_ENROLL <= 34, 1, 55)
    ),
    
    # 2. GA at enrollment
    # gestational age at enrollment 
    BASELINE_GA_WKS = floor(GA_US_DAYS_1/7),
    
    CRIT_GA = ifelse(BASELINE_GA_WKS > 0 & BASELINE_GA_WKS <= 13, 1, 
              ifelse(BASELINE_GA_WKS > 13 & BASELINE_GA_WKS <=26, 0,
              ifelse(BASELINE_GA_WKS == -7 | is.na(BASELINE_GA_WKS), NA, 77))),
    # 3. BMI 
    # BMI
    BMI = M05_WEIGHT_PERES_1 / M05_HEIGHT_PERES_1 / M05_HEIGHT_PERES_1 * 10000,
    
    CRIT_BMI = ifelse(BMI <= 18.5 | BMI >= 30, 0, 
               ifelse(BMI > 18.5 & BMI < 30, 1, 55)
    ),
    # 4. MUAC mid-upper arm circumference 
    # MUAC
    CRIT_MUAC = ifelse(M05_MUAC_PERES_1 <= 23, 0, 
                ifelse(M05_MUAC_PERES_1 > 23, 1, 55)
    ),
    # 5. height
    CRIT_HEIGHT = ifelse(M05_HEIGHT_PERES_1 <= 153, 0,
                  ifelse(M05_HEIGHT_PERES_1 > 153, 1, 55)
    ),
    # 6. single fetus
    CRIT_SINGLEPREG = ifelse(M06_SINGLETON_PERES_1 == 0, 0,
                      ifelse(M06_SINGLETON_PERES_1 == 1, 1, 55)
    )) %>% 
  rowwise() %>% 
  mutate(
    # 7. blood pressure
    M06_BP_SYS_1 = mean(c(M06_BP_SYS_VSORRES_1_1, M06_BP_SYS_VSORRES_2_1, M06_BP_SYS_VSORRES_3_1), na.rm = TRUE),
    M06_BP_DIA_1 = mean(c(M06_BP_DIA_VSORRES_1_1, M06_BP_DIA_VSORRES_2_1, M06_BP_DIA_VSORRES_3_1), na.rm = TRUE),
    
    CRIT_BP = ifelse(M06_BP_SYS_1 > 0 & M06_BP_SYS_1 < 140 & M06_BP_DIA_1 > 0 & M06_BP_DIA_1 < 90, 1,
              ifelse(M06_BP_SYS_1 >= 140 | M06_BP_DIA_1 >= 90, 0, 55)
    )) %>% 
  ungroup() %>% 
  mutate( 
    # 8. <= 1 miscarriage in two consecutive pregnancies (any previous pregnancy) skip patter not follow correctly
    CRIT_PREV_MISCARR = ifelse(M04_MISCARRIAGE_RPORRES_1 == 1 & M04_MISCARRIAGE_CT_RPORRES_1 > 1, 0,
                        ifelse(M04_PH_PREV_RPORRES_1 == 0 | # no previous pregnancy (Have you ever been pregnant? Include all live births, stillbirths, miscarriages, or abortions. Do not include the current pregnancy.)
                               M04_PH_OTH_RPORRES_1 == 0 | # How many of these pregnancies ended in a loss? This includes pregnancies that ended in a stillbirth, miscarriage, or an abortion? 
                               M04_MISCARRIAGE_RPORRES_1 == 0 | #(During any of your previous pregnancies or births, did you experience a spontaneous miscarriage (pregnancy loss before 20 weeks GA)
                              (M04_MISCARRIAGE_RPORRES_1 == 1 & M04_MISCARRIAGE_CT_RPORRES_1 <= 1), 1, 55)#(Specify total # of spontaneous miscarriage: )
    ),
    # # 9. no previous preterm or low birth weight delivery
    CRIT_PREV_PRETERM_LBW = ifelse(M04_PRETERM_RPORRES_1 == 1 | M04_LOWBIRTHWT_RPORRES_1 == 1, 0,
                            ifelse(M04_PH_PREV_RPORRES_1 == 0 | (M04_PRETERM_RPORRES_1 == 0 & M04_LOWBIRTHWT_RPORRES_1 == 0), 1,
                            ifelse(M04_PRETERM_RPORRES_1 == 99 | M04_LOWBIRTHWT_RPORRES_1 == 99, 0, 55))
    ),
    ## to include below once we finalize the healthy cohort 
    # # 10. no previous preterm 
    # CRIT_PRETERM = ifelse(M04_PRETERM_RPORRES_1 == 1, 0,
    #                ifelse(M04_PH_PREV_RPORRES_1 == 0 | M04_PRETERM_RPORRES_1 == 0,1,
    #                ifelse(M04_PRETERM_RPORRES_1 == 99, 0, 55))
    # ),
    # # 11. no previous low birth weight delivery
    # CRIT_LBW = ifelse(M04_LOWBIRTHWT_RPORRES_1 == 1,0,
    #            ifelse(M04_PH_PREV_RPORRES_1 == 0 | M04_LOWBIRTHWT_RPORRES_1 == 0, 1,
    #            ifelse(M04_LOWBIRTHWT_RPORRES_1 == 99, 0, 55))
    # ),
    # 12. No previous neonatal or fetal death (stillbirth/neonatal death within first 28 days of live)
      ## update naming from  CRIT_PREV_NEODEATH to CRIT_FETALDEATH
    CRIT_FETALDEATH = ifelse(M04_PH_OTH_RPORRES_1 > 0 | M04_STILLBIRTH_RPORRES_1 == 1, 0, #stillbirth,miscarriage or an abortion>0 or experienced a still birth
                         ifelse(M04_PH_PREV_RPORRES_1 == 0 | #never pregnant
                               (M04_PH_PREVN_RPORRES_1 >= 0 & M04_PH_LIVE_RPORRES_1 >=0 & M04_PH_PREVN_RPORRES_1 == M04_PH_LIVE_RPORRES_1) | #preg # = live preg #
                                M04_PH_OTH_RPORRES_1 == 0, 1, 55)  #stillbirth,miscarriage or an abortion = 0 
    ),
    # 13. combine complication
    CRIT_COMPLICATION = ifelse(M04_UNPL_CESARIAN_PROCCUR_1 == 1 | M04_PREECLAMPSIA_RPORRES_1== 1 |  
                               M04_GEST_DIAB_RPORRES_1 == 1 | M04_PREMATURE_RUPTURE_RPORRES_1 == 1 |  
                               M04_MACROSOMIA_RPORRES_1 == 1 | M04_OLIGOHYDRAMNIOS_RPORRES_1 == 1 |  
                               M04_APH_RPORRES_1 == 1 |  M04_PPH_RPORRES_1 == 1, 0,
                        ifelse(M04_PH_PREV_RPORRES_1 == 0 |
                              (M04_UNPL_CESARIAN_PROCCUR_1 == 0 & M04_PREECLAMPSIA_RPORRES_1 == 0 & 
                               M04_GEST_DIAB_RPORRES_1 == 0 & M04_PREMATURE_RUPTURE_RPORRES_1 == 0 & 
                               M04_MACROSOMIA_RPORRES_1 == 0 & M04_OLIGOHYDRAMNIOS_RPORRES_1 == 0 & 
                               M04_APH_RPORRES_1 == 0 & M04_PPH_RPORRES_1 == 0), 1,
                        ifelse(M04_UNPL_CESARIAN_PROCCUR_1 == 99 | M04_PREECLAMPSIA_RPORRES_1 == 99 |  
                               M04_GEST_DIAB_RPORRES_1 == 99 | M04_PREMATURE_RUPTURE_RPORRES_1== 99 |    
                               M04_MACROSOMIA_RPORRES_1== 99 | M04_OLIGOHYDRAMNIOS_RPORRES_1 == 99 |    
                               M04_APH_RPORRES_1 == 99 | M04_PPH_RPORRES_1 == 99, 0, 55))
    ),
    # 14. no cigarette smoking
    CRIT_SMOKE = ifelse(M03_SMOKE_OECOCCUR_1 == 1 | M03_CHEW_BNUT_OECOCCUR_1 == 1 | M03_CHEW_OECOCCUR_1 == 1, 0,
                 ifelse(M03_SMOKE_OECOCCUR_1 == 0 & M03_CHEW_BNUT_OECOCCUR_1 == 0 & M03_CHEW_OECOCCUR_1 == 0, 1, 55)
    ),
    # 15. no heavy alcohol use (>3 drinks per day, or >7 drinks per week)
    CRIT_DRINK = ifelse(SITE == "Pakistan", 666,
                 ifelse(M03_DRINK_OECOCCUR_1 == 1, 0,
                 ifelse(M03_DRINK_OECOCCUR_1 == 0, 1,
                 ifelse(M03_DRINK_OECOCCUR_1 == 66, 0,
                 ifelse(M03_DRINK_OECOCCUR_1 == 77, 0, 55)))) #temporary code for Kenya, check for other country
    ),
    # 16. no HIV M04_HIV_MHOCCUR_1(1,0.99): M06_HIV_POC_LBPERF_V, M06_HIV_POC_LBORRES_1(1.0)
    CRIT_HIV = ifelse(M06_HIV_POC_LBORRES_1 == 1, 0,#Record HIV results (1,0)
               ifelse(M06_HIV_POC_LBORRES_1 == 0, 1, 
               ifelse(M04_HIV_EVER_MHOCCUR_1 == 1 | #Have you ever been diagnosed with HIV? (1,0,99)
                      M04_HIV_MHOCCUR_1 == 1, 0, #had HIV since becoming pregnant with the current pregnancy (1,0,99)
               ifelse(M04_HIV_EVER_MHOCCUR_1 == 0 & M04_HIV_MHOCCUR_1 == 0 & M06_HIV_POC_LBPERF_1 == 0, 1,
               ifelse(M04_HIV_EVER_MHOCCUR_1 == 99 | M04_HIV_MHOCCUR_1 == 99 | 
                      M06_HIV_POC_LBPERF_1 == 0, 0, #Was point-of-care HIV test performed at this visit? (1,0)
               ifelse(M04_HIV_EVER_MHOCCUR_1 == 77 | M04_HIV_MHOCCUR_1 == 77 | 
                      M06_HIV_POC_LBPERF_1 == 77 | M06_HIV_POC_LBORRES_1 == 77, 0, 55))))) #Was point-of-care HIV test performed at this visit? (1,0)
    ),
    # 17. no cancer, kidney disease, cardiac disease
    CRIT_CHRONIC = ifelse(M04_CANCER_EVER_MHOCCUR_1 == 1 | M04_KIDNEY_EVER_MHOCCUR_1 == 1 | 
                          M04_CARDIAC_EVER_MHOCCUR_1 == 1, 0,
                   ifelse(M04_CANCER_EVER_MHOCCUR_1 == 0 & M04_KIDNEY_EVER_MHOCCUR_1 == 0 & 
                          M04_CARDIAC_EVER_MHOCCUR_1 == 0, 1,
                   ifelse(M04_CANCER_EVER_MHOCCUR_1 == 99 | M04_KIDNEY_EVER_MHOCCUR_1 == 99 | 
                          M04_CARDIAC_EVER_MHOCCUR_1 == 99, 0, 55))
    ),
    # 18. no current malaria infection (per RDT)
    CRIT_MALARIA = ifelse(M04_MALARIA_EVER_MHOCCUR_1 == 0, 1,
                   ifelse(M04_MALARIA_EVER_MHOCCUR_1 == 1, 0,
                   ifelse(M04_MALARIA_EVER_MHOCCUR_1 == 99, 99, 55))
    ),
    # 19. no Hepatitis B virus infection
    CRIT_HEPATITISB = ifelse(M06_HBV_POC_LBORRES_1 == 1, 0,
                      ifelse(M06_HBV_POC_LBORRES_1 == 0, 1,
                      ifelse(M06_HBV_POC_LBPERF_1 == 0, 0, 55))
    ),
    # 20. no Hepatitis C virus infection
    CRIT_HEPATITISC = ifelse(M06_HCV_POC_LBORRES_1 == 1, 0,
                      ifelse(M06_HCV_POC_LBORRES_1 == 0, 1,
                      ifelse(M06_HCV_POC_LBPERF_1 == 0, 0, 55))
    ),
    # 21. no hemoglobinopathies (include glucose-6-phosphate dehydrogenase deficiency or not?)
    CRIT_HEMOGLOBINOPATHIES = ifelse(M08_RBC_THALA_LBORRES_1 == 0 & M08_RBC_GLUC6_LBORRES_1 == 0, 1,
                              ifelse(M08_RBC_THALA_LBORRES_1 == 1 | M08_RBC_GLUC6_LBORRES_1 == 1, 0,
                              ifelse(M08_RBC_LBPERF_2_1 == 0 | M08_RBC_LBPERF_3_1 == 0, 0, 55))
    ),
    # 22. no iron deficiency (not iron deficient: serum ferritin > 15 mcg/L) data unit is ??g/dL couble check before use
    CRIT_IRON = ifelse(M08_FERRITIN_LBORRES_1 > 15*10, 1,
                ifelse(M08_FERRITIN_LBORRES_1 >0 & M08_FERRITIN_LBORRES_1 <= 15*10, 0,
                ifelse(M08_MN_LBPERF_8_1 == 0, 0, 55))
    ),
    # 23. no subclinical inflammation (CRP???5 and/or AGP???1) ??? check unit (mg/L for CRP and g/L for AGP in dd) double check the calculation before use
    CRIT_INFLAM = ifelse(M08_CRP_LBORRES_1 > 0 & M08_CRP_LBORRES_1 <= 5 & 
                         M08_AGP_LBORRES_1 >0 & M08_AGP_LBORRES_1 <= 1, 1,
                  ifelse(M08_CRP_LBORRES_1 > 5 | M08_AGP_LBORRES_1 > 1, 0,
                  ifelse(M08_MN_LBPERF_12_1 == 0 | M08_MN_LBPERF_13_1 == 0, 0, 55)) 
    ))# %>%
 # select(SCRNID, MOMID, PREGID, SITE, ENROLL, #REMAPP_LAUNCH,
#      starts_with("CRIT_"))

setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
save(vars_criteria, file= paste("vars_criteria_FullData","_", UploadDate, ".RData",sep = ""))

vars_criteria <- vars_criteria %>% 
          mutate(REMAPP_LAUNCH = ifelse((SITE == "Kenya" & M02_SCRN_OBSSTDAT_1 >= "2023-04-10") |
                                          (SITE == "Pakistan" & M02_SCRN_OBSSTDAT_1 >= "2022-09-22") |
                                          (SITE == "Ghana" & M02_SCRN_OBSSTDAT_1 >= "2022-12-28"), 1, 0)) %>%
            filter(REMAPP_LAUNCH ==1)  
  

setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
save(vars_criteria, file= paste("vars_criteria","_", UploadDate, ".RData",sep = ""))

#**************************************************************************************
#*check eligibility and save data
#**************************************************************************************
#code 666 for any not applicable by site
healthyOutcome <- vars_criteria %>% 
  rowwise() %>%
  mutate(HEALTHY_CHECK = sum(across(starts_with("CRIT_"), ~ .x %in% c(1, 0, 666)), na.rm = TRUE)) %>% 
  mutate(
    HEALTHY_ELIGIBLE = case_when(
      if_all(starts_with("CRIT_"), ~.x %in% c(1, 666)) ~ 1, #eligible
      if_any(starts_with("CRIT_"), ~.x == 0) ~ 0, #Not eligible
      HEALTHY_CHECK < 21 ~ 3 #pending (21) 
    ) )%>%
  ungroup() 


setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
save(healthyOutcome, file= paste("healthyOutcome","_", UploadDate, ".RData",sep = ""))

# save as full cohort when we don't exclude particpants enrolled following remapp launch
save(healthyOutcome, file= paste("healthyOutcome","_FullCohort_", UploadDate, ".RData",sep = ""))

#**************************************************************************************
#### VISIT COMPLETION #### 
#* Are forms completed with visit status = 1 or 2? 

#* Num: Any form with visit type = i AND visit status = 1 or 2 
#* Denom: Passed window for visit type = i
#**************************************************************************************

## ANC
Visit_Complete_Anc <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS")) %>% 
  ## ONTIME WINDOWS 
  # Numerator for ANC Visit Completion
  mutate(VC_ENROLL_NUM =ifelse(ANY_TYPE_VISIT_COMPLETE_1 == 1 & ENROLL_PASS == 1, 1, 0),
         VC_ANC20_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_2 == 1 & ANC20_PASS == 1, 1, 0),
         VC_ANC28_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_3 == 1 & ANC28_PASS == 1, 1, 0),
         VC_ANC32_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_4 == 1 & ANC32_PASS == 1, 1, 0),
         VC_ANC36_NUM = ifelse(ANY_TYPE_VISIT_COMPLETE_5 == 1 & ANC36_PASS == 1, 1, 0)) %>% 
  ## exclude particpants who do not have EDD -- we are not able to calculate their windows - exclude from num and denom
  mutate(VC_ENROLL_NUM = ifelse(is.na(ENROLL_PASS), NA, VC_ENROLL_NUM),
         VC_ANC20_NUM = ifelse(is.na(ANC20_PASS), NA, VC_ANC20_NUM),
         VC_ANC28_NUM = ifelse(is.na(ANC28_PASS), NA, VC_ANC28_NUM),
         VC_ANC32_NUM = ifelse(is.na(ANC32_PASS), NA, VC_ANC32_NUM),
         VC_ANC36_NUM = ifelse(is.na(ANC36_PASS), NA, VC_ANC36_NUM)) %>% 
  ## LATE WINDOWS
  # Numerator for ANC Visit Completion
  mutate(VC_ENROLL_NUM_LATE =ifelse(ANY_TYPE_VISIT_COMPLETE_1 == 1 & ENROLL_PASS_LATE == 1, 1, 0),
         VC_ANC20_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_2 == 1 & ANC20_PASS_LATE == 1, 1, 0),
         VC_ANC28_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_3 == 1 & ANC28_PASS_LATE == 1, 1, 0),
         VC_ANC32_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_4 == 1 & ANC32_PASS_LATE == 1, 1, 0),
         VC_ANC36_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_5 == 1 & ANC36_PASS_LATE == 1, 1, 0)) %>% 
  ## exclude particpants who do not have EDD -- we are not able to calculate their windows - exclude from num and denom
  mutate(VC_ENROLL_NUM_LATE = ifelse(is.na(ENROLL_PASS_LATE), NA, VC_ENROLL_NUM_LATE),
         VC_ANC20_NUM_LATE = ifelse(is.na(ANC20_PASS_LATE), NA, VC_ANC20_NUM_LATE),
         VC_ANC28_NUM_LATE = ifelse(is.na(ANC28_PASS_LATE), NA, VC_ANC28_NUM_LATE),
         VC_ANC32_NUM_LATE = ifelse(is.na(ANC32_PASS_LATE), NA, VC_ANC32_NUM_LATE),
         VC_ANC36_NUM_LATE = ifelse(is.na(ANC36_PASS_LATE), NA, VC_ANC36_NUM_LATE)) 
  

save(Visit_Complete_Anc, file= paste("Visit_Complete_Anc","_", UploadDate, ".RData",sep = ""))

## PNC 

Visit_Complete_Pnc <- MatData_Pnc_Visits %>% 
  filter(BIRTH_OUTCOME_YN == 1) %>% 
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS")) %>% 
  ## ON TIME WINDOWS 
  # Numerator for PNC Visit Completion
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
  # Numerator for PNC Visit Completion
  mutate(VC_PNC0_NUM_LATE =ifelse(ANY_TYPE_VISIT_COMPLETE_7 == 1 & PNC0_PASS_LATE == 1, 1, 0),
         VC_PNC1_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_8 == 1 & PNC1_PASS_LATE == 1, 1, 0),
         VC_PNC4_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_9 == 1 & PNC4_PASS_LATE == 1, 1, 0),
         VC_PNC6_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_10 == 1 & PNC6_PASS_LATE == 1, 1, 0),
         VC_PNC26_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_11 == 1 & PNC26_PASS_LATE == 1, 1, 0),
         VC_PNC52_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_12 == 1 & PNC52_PASS_LATE == 1, 1, 0)) %>% 
  ## exclude particpants who do not have DOB -- we are not able to calculate their windows - exclude from num and denom
  mutate(VC_PNC0_NUM_LATE = ifelse(is.na(PNC0_PASS_LATE), NA, VC_PNC0_NUM_LATE),
         VC_PNC1_NUM_LATE = ifelse(is.na(PNC1_PASS_LATE), NA, VC_PNC1_NUM_LATE),
         VC_PNC4_NUM_LATE = ifelse(is.na(PNC4_PASS_LATE), NA, VC_PNC4_NUM_LATE),
         VC_PNC6_NUM_LATE = ifelse(is.na(PNC6_PASS_LATE), NA, VC_PNC6_NUM_LATE),
         VC_PNC26_NUM_LATE = ifelse(is.na(PNC26_PASS_LATE), NA, VC_PNC26_NUM_LATE),
         VC_PNC52_NUM_LATE = ifelse(is.na(PNC52_PASS_LATE), NA, VC_PNC52_NUM_LATE)) 
  

save(Visit_Complete_Pnc, file= paste("Visit_Complete_Pnc","_", UploadDate, ".RData",sep = ""))
#**************************************************************************************
#### PROTOCOL COMPLIANCE #### 
#* Are all expected forms for the visit complete? 
#* Num (hard code in monitoring report rmd): For each form: visit type = i AND visit status = 1 or 2 AND passed window for visit type = i 
#* Denom: Any form with visit type = i AND visit status = 1 or 2 AND passed window for visit type = i 
#**************************************************************************************
## ANC
## CALCULATE DENOMINATORS FOR PROTOCOL COMPLIANCE  
Prot_Compliance_Anc <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), 
         contains("_VISIT_COMPLETE_")) %>% 
    # DENOMINATOR for protocol compliance
    mutate(PC_ENROLL_DENOM =ifelse(ANY_TYPE_VISIT_COMPLETE_1 == 1 & ENROLL_PASS == 1, 1, 0),
           PC_ANC20_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_2 == 1 & ANC20_PASS == 1, 1, 0),
           PC_ANC28_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_3 == 1 & ANC28_PASS == 1, 1, 0),
           PC_ANC32_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_4 == 1 & ANC32_PASS == 1, 1, 0),
           PC_ANC36_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_5 == 1 & ANC36_PASS == 1, 1, 0)) 


save(Prot_Compliance_Anc, file= paste("Prot_Compliance_Anc","_", UploadDate, ".RData",sep = ""))

## PNC 
## CALCULATE DENOMINATORS FOR PROTOCOL COMPLIANCE  
Prot_Compliance_Pnc <- MatData_Pnc_Visits %>% 
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), 
         contains("_VISIT_COMPLETE_"), BIRTH_OUTCOME_YN, M23_CLOSE_DSDECOD) %>% 
  # DENOMINATOR for protocol compliance
  mutate(PC_PNC0_DENOM =ifelse(ANY_TYPE_VISIT_COMPLETE_7 == 1 & PNC0_PASS == 1, 1, 0),
         PC_PNC1_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_8 == 1 & PNC1_PASS == 1, 1, 0),
         PC_PNC4_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_9 == 1 & PNC4_PASS == 1, 1, 0),
         PC_PNC6_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_10 == 1 & PNC6_PASS == 1, 1, 0),
         PC_PNC26_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_11 == 1 & PNC26_PASS == 1, 1, 0),
         PC_PNC52_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_12 == 1 & PNC52_PASS == 1, 1, 0)) 

save(Prot_Compliance_Pnc, file= paste("Prot_Compliance_Pnc","_", UploadDate, ".RData",sep = ""))

#**************************************************************************************
####  FORM COMPLETION #### 
#* Are forms completed regardless of visit status? 
#* Num (hard code in monitoring report rmd): for each form: visit type = i AND have any visit status AND passed window for visit type = i 
#* Denom: Any form with visit type = i AND have any visit status AND passed window for visit type = i
#**************************************************************************************

## ANC
Form_Completion_Anc <- MatData_Anc_Visits %>% 
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), 
         contains("_VISIT_COMPLETE_"),contains("_TYPE_VISIT_"), GA_ENROLL_WKS) %>% 
  # DENOMINATOR for form completion
    # step 1. indicator for any type visit = i with any visit status 
  mutate(TYPE_VISIT_ANY_STATUS_1 = ifelse((M01_TYPE_VISIT_1 == 1 & !is.na(M01_VISIT_COMPLETE_1)) | 
                                          (M03_TYPE_VISIT_1 == 1 & !is.na(M03_VISIT_COMPLETE_1)) |
                                          (M04_TYPE_VISIT_1 == 1 & !is.na(M04_VISIT_COMPLETE_1)) |
                                          (M05_TYPE_VISIT_1 == 1 & !is.na(M05_VISIT_COMPLETE_1)) |
                                          (M06_TYPE_VISIT_1 == 1 & !is.na(M06_VISIT_COMPLETE_1)) |
                                          (M07_TYPE_VISIT_1 == 1 & !is.na(M07_VISIT_COMPLETE_1)) |
                                          (M08_TYPE_VISIT_1 == 1 & !is.na(M08_VISIT_COMPLETE_1)), 1, 0),
         TYPE_VISIT_ANY_STATUS_2 = ifelse((M04_TYPE_VISIT_2 == 2 & !is.na(M04_VISIT_COMPLETE_2) & GA_ENROLL_WKS <= 17) |
                                          (M05_TYPE_VISIT_2 == 2 & !is.na(M05_VISIT_COMPLETE_2) & GA_ENROLL_WKS <= 17) |
                                          (M06_TYPE_VISIT_2 == 2 & !is.na(M06_VISIT_COMPLETE_2) & GA_ENROLL_WKS <= 17) |
                                          (M07_TYPE_VISIT_2 == 2 & !is.na(M07_VISIT_COMPLETE_2) & GA_ENROLL_WKS <= 17) |
                                          (M08_TYPE_VISIT_2 == 2 & !is.na(M08_VISIT_COMPLETE_2) & GA_ENROLL_WKS <= 17) | 
                                          (M25_TYPE_VISIT_2 == 2 & !is.na(M25_VISIT_COMPLETE_2) & GA_ENROLL_WKS <= 17) | 
                                          (M26_TYPE_VISIT_2 == 2 & !is.na(M26_VISIT_COMPLETE_2) & GA_ENROLL_WKS <= 17), 1, 0),
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
    mutate(FC_ENROLL_DENOM =ifelse(TYPE_VISIT_ANY_STATUS_1 == 1 & ENROLL_PASS == 1, 1, 0),
           FC_ANC20_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_2 == 1 & ANC20_PASS == 1, 1, 0),
           FC_ANC28_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_3 == 1 & ANC28_PASS == 1, 1, 0),
           FC_ANC32_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_4 == 1 & ANC32_PASS == 1, 1, 0),
           FC_ANC36_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_5 == 1 & ANC36_PASS == 1, 1, 0)) 

save(Form_Completion_Anc, file= paste("Form_Completion_Anc","_", UploadDate, ".RData",sep = ""))

## PNC 
Form_Completion_Pnc <- MatData_Pnc_Visits %>% 
  select(SITE, MOMID, PREGID, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), 
         contains("_VISIT_COMPLETE_"),contains("_TYPE_VISIT_"), BIRTH_OUTCOME_YN, M23_CLOSE_DSDECOD) %>% 
  # DENOMINATOR for form completion
  # step 1. indicator for any type visit = i with any visit status 
  mutate(TYPE_VISIT_ANY_STATUS_7 = ifelse((M06_TYPE_VISIT_7 == 7 & !is.na(M06_VISIT_COMPLETE_7)) |
                                            (M12_TYPE_VISIT_7 == 7 & !is.na(M08_VISIT_COMPLETE_7)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_8 = ifelse((M06_TYPE_VISIT_8 == 8 & !is.na(M06_VISIT_COMPLETE_8)) |
                                            (M12_TYPE_VISIT_8 == 8 & !is.na(M08_VISIT_COMPLETE_8)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_9 = ifelse((M06_TYPE_VISIT_9 == 9 & !is.na(M06_VISIT_COMPLETE_9)) |
                                            (M12_TYPE_VISIT_9 == 9 & !is.na(M08_VISIT_COMPLETE_9)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_10 = ifelse((M05_TYPE_VISIT_10 == 10 & !is.na(M05_VISIT_COMPLETE_10)) |
                                             (M06_TYPE_VISIT_10 == 10 & !is.na(M06_VISIT_COMPLETE_10)) |
                                             (M07_TYPE_VISIT_10 == 10 & !is.na(M07_VISIT_COMPLETE_10)) |
                                             (M08_TYPE_VISIT_10 == 10 & !is.na(M08_VISIT_COMPLETE_10)) |
                                             (M12_TYPE_VISIT_10 == 10 & !is.na(M12_VISIT_COMPLETE_10)) | 
                                             (M25_TYPE_VISIT_10 == 10 & !is.na(M25_VISIT_COMPLETE_10)) | 
                                             (M26_TYPE_VISIT_10 == 10 & !is.na(M26_VISIT_COMPLETE_10)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_11 = ifelse((M05_TYPE_VISIT_11 == 11 & !is.na(M05_VISIT_COMPLETE_11)) |
                                             (M06_TYPE_VISIT_11 == 11 & !is.na(M06_VISIT_COMPLETE_11)) |
                                             (M07_TYPE_VISIT_11 == 11 & !is.na(M07_VISIT_COMPLETE_11)) |
                                             (M08_TYPE_VISIT_11 == 11 & !is.na(M08_VISIT_COMPLETE_11)) | 
                                             (M12_TYPE_VISIT_11 == 11 & !is.na(M12_VISIT_COMPLETE_11)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_12 = ifelse((M05_TYPE_VISIT_12 == 12 & !is.na(M05_VISIT_COMPLETE_12)) |
                                             (M06_TYPE_VISIT_12 == 12 & !is.na(M06_VISIT_COMPLETE_12)) |
                                             (M12_TYPE_VISIT_12 == 12 & !is.na(M12_VISIT_COMPLETE_12)), 1, 0)) %>% 
  # step 2. generate denominator 
  mutate(FC_PNC0_DENOM =ifelse(TYPE_VISIT_ANY_STATUS_7 == 1 & PNC0_PASS == 1, 1, 0),
         FC_PNC1_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_8 == 1 & PNC1_PASS == 1, 1, 0),
         FC_PNC4_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_9 == 1 & PNC4_PASS == 1, 1, 0),
         FC_PNC6_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_10 == 1 & PNC6_PASS == 1, 1, 0),
         FC_PNC26_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_11 == 1 & PNC26_PASS == 1, 1, 0),
         FC_PNC52_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_12 == 1 & PNC52_PASS == 1, 1, 0)) 

save(Form_Completion_Pnc, file= paste("Form_Completion_Pnc","_", UploadDate, ".RData",sep = ""))

