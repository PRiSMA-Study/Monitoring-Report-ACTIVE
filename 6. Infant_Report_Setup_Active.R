#*****************************************************************************
#### MONITORING REPORT SETUP -- INFANT ####
#* Function: Merge all forms together in wide format to create a dataset with one row for each woman for each visit 
#* Input: .RData files for each form (generated from 1. data import code) and infant outcomes (generated in code linked here: https://github.com/PRiSMA-Study/PRISMA-Public/blob/main/PRISMA-Infant-Constructed-Outcomes/Infant-Constructed-Variables.R)
#* Last updated: 27 June 2025


## load in data 
rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)

UploadDate = "2024-11-15"

load(paste0("~/Monitoring Report/data/cleaned/", UploadDate, "/", "MatData_Wide_", UploadDate, ".RData"))
load(paste0("~/Monitoring Report/data/cleaned/", UploadDate, "/", "InfData_Wide_", UploadDate, ".RData"))

## extract end_preg_ga from MatData_Pnc_Visits (created in Maternal_Report_Setup-Active.R)
load(paste0("D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data", "/", "MatData_Pnc_Visits", ".RData"))

# set path to save 
path_to_save <- "D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/"
setwd(paste0("D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/"))

#*****************************************************************************
#* Extract variables for monitoring report 
#*****************************************************************************
#update/add/delete variable names in the varNames_sheet.xlsx (check if the var is multiple or singe when add)
MatNames_sheet <- read_excel("~/Monitoring Report/code/varNames_sheet.xlsx", sheet = "MaternalVars")
InfNames_sheet <- read_excel("~/Monitoring Report/code/varNames_sheet.xlsx", sheet = "InfantVars")

mat_enroll <- read_csv(paste0("Z:/Outcome Data/", UploadDate, "/MAT_ENROLL.csv")) %>% 
  select(SITE, SCRNID, MOMID, PREGID, ENROLL, EST_CONCEP_DATE,GA_DIFF_DAYS, 
         EDD_BOE,BOE_METHOD, BOE_GA_WKS_ENROLL, BOE_GA_DAYS_ENROLL)

MatData_Pnc_Visits_subset <- MatData_Pnc_Visits %>% select(SITE, MOMID, PREGID, ENDPREG_DAYS)

inf_outcomes <- read_csv(paste0("Z:/Outcome Data/", UploadDate, "/INF_OUTCOMES.csv")) %>% 
  select(SITE, MOMID, PREGID, INFANTID, ADJUD_NEEDED, LIVEBIRTH)

MatData_Report <- MatData_Wide %>% 
  select(matches(MatNames_sheet$varname), 
         US_GA_WKS_ENROLL, US_GA_DAYS_ENROLL,
         M02_SCRN_OBSSTDAT,EDD_US,BOE_GA_DAYS_ENROLL, 
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
         M00_KNOWN_DOBYN_SCORRES, DOB,
         contains("_FETUS_CT_PERES_US"),
         M02_SCRN_RETURN, contains("RBC_G6PD_LBORRES")) 

InfData_Report <- InfData_Wide %>% 
  select(matches(InfNames_sheet$varname),
         M11_VISIT_COMPLETE_6,
         M09_BIRTH_DSTERM,M09_DELIV_DSSTDAT,
         contains("_TYPE_VISIT_"), 
         contains("_VISIT_COMPLETE"),
         contains("_PNC_AT_VISIT_DAYS"),
         contains("_PNC_AT_VISIT_WKS")) %>% 
  # mutate(DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")) %>% 
  filter(PREGID %in% as.vector(mat_enroll$PREGID)) %>% 
  # if duplicates, take the most recent
  group_by(SITE, MOMID, PREGID, INFANTID) %>% 
  arrange(-desc(M09_DELIV_DSSTDAT)) %>%
  slice(1) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  select(-n) %>%
  ungroup() %>%
  ## merge in infant outcomes to filter out adjudication cases and to make sure the monitoring report tables match the infant outcomes table 
  full_join(inf_outcomes, by = c("SITE", "MOMID", "PREGID", "INFANTID"))

#**************************************************************************************
### InfData_Pnc_Visits
# input: InfData_Report
# Includes all babies who are delivered to mothers who are enrolled. Includes constructed variables relevant to PNC 
#**************************************************************************************

## InfData_Pnc_Visits
InfData_Pnc_Visits <- InfData_Report %>% 
  select(SITE, MOMID, PREGID, INFANTID, M09_BIRTH_DSTERM,M09_DELIV_DSSTDAT,LIVEBIRTH,
         ends_with("_7"),  ends_with("_8"),  ends_with("_9"), 
         ends_with("_10"), ends_with("_11"), ends_with("_12"),contains("M24_")) %>% 
  ## filter for livebirths 
  filter(LIVEBIRTH == 1) %>% 
  rename(DOB = M09_DELIV_DSSTDAT) %>% 
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
  mutate(PNC0_OVERDUE = ifelse(UploadDate>PNC0_LATE & is.na(M13_VISIT_COMPLETE_7), 1, 0),
         PNC1_OVERDUE = ifelse(UploadDate>PNC1_LATE & is.na(M13_VISIT_COMPLETE_8), 1, 0),
         PNC4_OVERDUE = ifelse(UploadDate>PNC4_LATE & is.na(M13_VISIT_COMPLETE_9), 1, 0),
         PNC6_OVERDUE = ifelse(UploadDate>PNC6_LATE & is.na(M13_VISIT_COMPLETE_10), 1, 0),
         PNC26_OVERDUE = ifelse(UploadDate>PNC26_LATE & is.na(M13_VISIT_COMPLETE_11), 1, 0),
         PNC52_OVERDUE = ifelse(UploadDate>PNC52_LATE & is.na(M13_VISIT_COMPLETE_12), 1, 0)) %>% 
  ## CALCULATE INDICATOR VARIALBE FOR ANY VISIT TYPE = i
  mutate(ANY_TYPE_VISIT_COMPLETE_7 = ifelse((M13_TYPE_VISIT_7 == 7 & M13_VISIT_COMPLETE_7 == 1) |
                                            (M14_TYPE_VISIT_7 == 7 & M14_VISIT_COMPLETE_7 == 1) | 
                                            (M15_TYPE_VISIT_7 == 7 & M15_VISIT_COMPLETE_7 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_8 = ifelse((M13_TYPE_VISIT_8 == 8 & M13_VISIT_COMPLETE_8 == 1) |
                                              (M14_TYPE_VISIT_8 == 8 & M14_VISIT_COMPLETE_8 == 1) | 
                                              (M15_TYPE_VISIT_8 == 8 & M15_VISIT_COMPLETE_8 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_9 = ifelse((M13_TYPE_VISIT_9 == 9 & M13_VISIT_COMPLETE_9 == 1) |
                                              (M14_TYPE_VISIT_9 == 9 & M14_VISIT_COMPLETE_9 == 1) | 
                                              (M15_TYPE_VISIT_9 == 9 & M15_VISIT_COMPLETE_9 == 1), 1, 0),
         
         ANY_TYPE_VISIT_COMPLETE_10 = ifelse((M13_TYPE_VISIT_10 == 10 & M13_VISIT_COMPLETE_10 == 1) |
                                               (M14_TYPE_VISIT_10 == 10 & M14_VISIT_COMPLETE_10 == 1) | 
                                               (M15_TYPE_VISIT_10 == 10 & M15_VISIT_COMPLETE_10 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_11 = ifelse((M13_TYPE_VISIT_11 == 11 & M13_VISIT_COMPLETE_11 == 1) |
                                               (M14_TYPE_VISIT_11 == 11 & M14_VISIT_COMPLETE_11 == 1) | 
                                               (M15_TYPE_VISIT_11 == 11 & M15_VISIT_COMPLETE_11 == 1), 1, 0),
         ANY_TYPE_VISIT_COMPLETE_12 = ifelse((M13_TYPE_VISIT_12 == 12 & M13_VISIT_COMPLETE_12 == 1) |
                                               (M14_TYPE_VISIT_12 == 12 & M14_VISIT_COMPLETE_12 == 1) | 
                                               (M15_TYPE_VISIT_12 == 12 & M15_VISIT_COMPLETE_12 == 1), 1, 0)) 


## export 
save(InfData_Pnc_Visits, file= paste0(path_to_save, "InfData_Pnc_Visits",".RData",sep = ""))

#**************************************************************************************
#### VISIT COMPLETION #### 
#* Are forms completed with visit status = 1 or 2? 

#* Num: Any form with visit type = i AND visit status = 1 or 2 
#* Denom: Passed window for visit type = i
#**************************************************************************************
# PNC
## i filtered out anyone who has an endpreg and then
# for pnc visits where we do not expect those with miscarriages, i filter 
Inf_Visit_Complete_Pnc <- InfData_Pnc_Visits %>%
  select(SITE, MOMID, PREGID,INFANTID,LIVEBIRTH, contains("ANY_TYPE_VISIT_COMPLETE_"),
         contains("_PASS"), contains("_ONTIME"), contains("_LATE"), M24_CLOSE_DSSTDAT,M24_CLOSE_DSDECOD, DOB) %>%
  ## filter for livebirths 
  filter(LIVEBIRTH == 1) %>% 
  # merge in est conception date 
  left_join(MatData_Pnc_Visits_subset %>% select(SITE, MOMID, PREGID, ENDPREG_DAYS), 
            by = c("SITE", "MOMID", "PREGID")) %>% 
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
  # Numerator for PNC Visit Completion -- NEED TO EXCLUDE ANY PARTICIAPNT WHO HAS CLOSED OUT YET (M24_CLOSE_DSSTDAT).
  mutate(VC_PNC0_NUM_LATE =ifelse(ANY_TYPE_VISIT_COMPLETE_7 == 1 & PNC0_PASS_LATE == 1 &
                                    ((M24_CLOSE_DSSTDAT > PNC0_LATE) | is.na(M24_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC1_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_8 == 1 & PNC1_PASS_LATE == 1 &
                                     ((M24_CLOSE_DSSTDAT > PNC1_LATE) | is.na(M24_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC4_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_9 == 1 & PNC4_PASS_LATE == 1 &
                                     ((M24_CLOSE_DSSTDAT > PNC4_LATE) | is.na(M24_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC6_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_10 == 1 & PNC6_PASS_LATE == 1 &
                                     ((M24_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M24_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC26_NUM_LATE = ifelse(ANY_TYPE_VISIT_COMPLETE_11 == 1 & PNC26_PASS_LATE == 1 &
                                      ((M24_CLOSE_DSSTDAT > PNC26_LATE) | is.na(M24_CLOSE_DSSTDAT)), 1, 0),
         VC_PNC52_NUM_LATE = ifelse((ANY_TYPE_VISIT_COMPLETE_12 == 1 & PNC52_PASS_LATE == 1 & is.na(M24_CLOSE_DSSTDAT)) |
                                    (ANY_TYPE_VISIT_COMPLETE_12 == 1 & M24_CLOSE_DSDECOD ==1), 1, 0)) %>%
  ## exclude particpants who do not have DOB -- we are not able to calculate their windows - exclude from num and denom
  mutate(VC_PNC0_NUM_LATE = ifelse(is.na(PNC0_PASS_LATE), NA, VC_PNC0_NUM_LATE),
         VC_PNC1_NUM_LATE = ifelse(is.na(PNC1_PASS_LATE), NA, VC_PNC1_NUM_LATE),
         VC_PNC4_NUM_LATE = ifelse(is.na(PNC4_PASS_LATE), NA, VC_PNC4_NUM_LATE),
         VC_PNC6_NUM_LATE = ifelse(is.na(PNC6_PASS_LATE), NA, VC_PNC6_NUM_LATE),
         VC_PNC26_NUM_LATE = ifelse(is.na(PNC26_PASS_LATE), NA, VC_PNC26_NUM_LATE),
         VC_PNC52_NUM_LATE = ifelse(is.na(PNC52_PASS_LATE), NA, VC_PNC52_NUM_LATE)) %>%
## generate denominators - late
mutate(VC_PNC0_DENOM_LATE = ifelse(PNC0_PASS_LATE==1 &
                                     ((M24_CLOSE_DSSTDAT > PNC0_LATE) | is.na(M24_CLOSE_DSSTDAT)), 1, 0),
       VC_PNC1_DENOM_LATE = ifelse(PNC1_PASS_LATE==1 &
                                     ((M24_CLOSE_DSSTDAT > PNC1_LATE) | is.na(M24_CLOSE_DSSTDAT)), 1, 0),
       VC_PNC4_DENOM_LATE = ifelse(PNC4_PASS_LATE==1 &
                                     ((M24_CLOSE_DSSTDAT > PNC4_LATE) | is.na(M24_CLOSE_DSSTDAT)), 1, 0),
       VC_PNC6_DENOM_LATE = ifelse(PNC6_PASS_LATE==1 & 
                                     ((M24_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M24_CLOSE_DSSTDAT)), 1, 0),
       
       VC_PNC6_DENOM_LATE_PROT = ifelse(PNC6_PASS_LATE_PROT==1 & 
                                          ((M24_CLOSE_DSSTDAT > PNC6_LATE_PROT) | is.na(M24_CLOSE_DSSTDAT)), 1, 0),
       
       VC_PNC26_DENOM_LATE = ifelse(PNC26_PASS_LATE==1 & 
                                      ((M24_CLOSE_DSSTDAT > PNC26_LATE) | is.na(M24_CLOSE_DSSTDAT)), 1, 0),
       
       VC_PNC52_DENOM_LATE = ifelse((PNC52_PASS_LATE==1 & is.na(M24_CLOSE_DSSTDAT)) | 
                                      M24_CLOSE_DSDECOD==1, 1, 0)
)


## export 
save(Inf_Visit_Complete_Pnc, file= paste0(path_to_save, "Inf_Visit_Complete_Pnc",".RData",sep = ""))


#**************************************************************************************
#### PROTOCOL COMPLIANCE #### 
#* Are all expected forms for the visit complete? 
# Table 6 (ANC) [dataframe: Prot_Compliance_Anc]
# Table 8 (PNC) [dataframe: Prot_Compliance_Pnc]
# ReMAPP Table 1 (MNH23/MNH26) [dataframe: Prot_Compliance_MNH25]

#* Num (hard code in monitoring report rmd): For each form: visit type = i AND visit status = 1 or 2 AND passed window for visit type = i 
#* Denom: Any form with visit type = i AND visit status = 1 or 2 AND passed window for visit type = i 
#**************************************************************************************

## PNC 
## CALCULATE DENOMINATORS FOR PROTOCOL COMPLIANCE  
Inf_Prot_Compliance_Pnc <- InfData_Pnc_Visits %>% 
  select(SITE, MOMID, PREGID, INFANTID,LIVEBIRTH, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), 
         contains("_VISIT_COMPLETE_"), M24_CLOSE_DSDECOD) %>% 
  ## filter for livebirths 
  filter(LIVEBIRTH == 1) %>% 
  # DENOMINATOR for protocol compliance
  mutate(PC_PNC0_DENOM =ifelse(ANY_TYPE_VISIT_COMPLETE_7 == 1 & PNC0_PASS_LATE == 1, 1, 0),
         PC_PNC1_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_8 == 1 & PNC1_PASS_LATE == 1, 1, 0),
         PC_PNC4_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_9 == 1 & PNC4_PASS_LATE == 1, 1, 0),
         PC_PNC6_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_10 == 1 & PNC6_PASS_LATE == 1 , 1, 0),
         PC_PNC26_DENOM = ifelse(ANY_TYPE_VISIT_COMPLETE_11 == 1 & PNC26_PASS_LATE == 1, 1, 0),
         PC_PNC52_DENOM = ifelse((ANY_TYPE_VISIT_COMPLETE_12 == 1 & PNC52_PASS_LATE == 1) | 
                                  (ANY_TYPE_VISIT_COMPLETE_12 == 1 &  M24_CLOSE_DSDECOD== 1), 1, 0)) 
 
## export 
save(Inf_Prot_Compliance_Pnc, file= paste0(path_to_save, "Inf_Prot_Compliance_Pnc",".RData",sep = ""))

#**************************************************************************************
####  FORM COMPLETION #### 
#* Are forms completed regardless of visit status? 
# Table 7 (ANC) [dataframe: Form_Completion_Anc]
# Table 9 (PNC) [dataframe: Form_Completion_Pnc]

#* Num (hard code in monitoring report rmd): for each form: visit type = i AND have any visit status AND passed window for visit type = i 
#* Denom: Any form with visit type = i AND have any visit status AND passed window for visit type = i
#**************************************************************************************
## PNC 
Inf_Form_Completion_Pnc <- InfData_Pnc_Visits %>% 
  select(SITE, MOMID, PREGID, LIVEBIRTH,contains("ANY_TYPE_VISIT_COMPLETE_"), contains("_PASS"), 
         contains("_VISIT_COMPLETE_"),contains("_TYPE_VISIT_"),PNC52_LATE,PNC52_ONTIME,
         M24_CLOSE_DSDECOD) %>%
  ## filter for livebirths 
  filter(LIVEBIRTH == 1) %>% 
  # filter(!is.na(ENDPREG_DAYS)) %>% ## exclude anyone without a birth outcome (missing ENDPREG_DAYS)
  # DENOMINATOR for form completion
  # step 1. indicator for any type visit = i with any visit status 
  mutate(TYPE_VISIT_ANY_STATUS_7 = ifelse((M13_TYPE_VISIT_7 == 7 & !is.na(M13_VISIT_COMPLETE_7)) |
                                            (M14_TYPE_VISIT_7 == 7 & !is.na(M14_VISIT_COMPLETE_7))| 
                                            (M15_TYPE_VISIT_7 == 7 & !is.na(M15_VISIT_COMPLETE_7)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_8 = ifelse((M13_TYPE_VISIT_8 == 8 & !is.na(M13_VISIT_COMPLETE_8)) |
                                            (M14_TYPE_VISIT_8 == 8 & !is.na(M14_VISIT_COMPLETE_8))| 
                                            (M15_TYPE_VISIT_8 == 8 & !is.na(M15_VISIT_COMPLETE_8)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_9 = ifelse((M13_TYPE_VISIT_9 == 9 & !is.na(M13_VISIT_COMPLETE_9)) |
                                            (M14_TYPE_VISIT_9 == 9 & !is.na(M14_VISIT_COMPLETE_9))| 
                                            (M15_TYPE_VISIT_9 == 9 & !is.na(M15_VISIT_COMPLETE_9)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_10 = ifelse((M13_TYPE_VISIT_10 == 10 & !is.na(M13_VISIT_COMPLETE_10)) |
                                             (M14_TYPE_VISIT_10 == 10 & !is.na(M14_VISIT_COMPLETE_10))| 
                                             (M15_TYPE_VISIT_10 == 10 & !is.na(M15_VISIT_COMPLETE_10)), 1, 0),
         ## might need to add some detail here of who we expect after the 42 day fu (only women who have had a live birth)
         TYPE_VISIT_ANY_STATUS_11 = ifelse((M13_TYPE_VISIT_11 == 11 & !is.na(M13_VISIT_COMPLETE_11)) |
                                             (M14_TYPE_VISIT_11 == 11 & !is.na(M14_VISIT_COMPLETE_11))| 
                                             (M15_TYPE_VISIT_11 == 11 & !is.na(M15_VISIT_COMPLETE_11)), 1, 0),
         
         TYPE_VISIT_ANY_STATUS_12 = ifelse((M13_TYPE_VISIT_12 == 12 & !is.na(M13_VISIT_COMPLETE_12)) |
                                             (M14_TYPE_VISIT_12 == 12 & !is.na(M14_VISIT_COMPLETE_12))| 
                                             (M15_TYPE_VISIT_12 == 12 & !is.na(M15_VISIT_COMPLETE_12)), 1, 0)) %>% 
  # step 2. generate denominator 
  mutate(FC_PNC0_DENOM =ifelse(TYPE_VISIT_ANY_STATUS_7 == 1 & PNC0_PASS_LATE == 1, 1, 0),
         FC_PNC1_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_8 == 1 & PNC1_PASS_LATE == 1, 1, 0),
         FC_PNC4_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_9 == 1 & PNC4_PASS_LATE == 1, 1, 0),
         FC_PNC6_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_10 == 1 & PNC6_PASS_LATE == 1, 1, 0),
         FC_PNC26_DENOM = ifelse(TYPE_VISIT_ANY_STATUS_11 == 1 & PNC26_PASS_LATE == 1, 1, 0),
         FC_PNC52_DENOM = ifelse((TYPE_VISIT_ANY_STATUS_12 == 1 & PNC52_PASS_LATE == 1) |
                                 (TYPE_VISIT_ANY_STATUS_12 == 1 & M24_CLOSE_DSDECOD == 1), 1, 0)) 

## export 
save(Inf_Form_Completion_Pnc, file= paste0(path_to_save, "Inf_Form_Completion_Pnc",".RData",sep = ""))

#**************************************************************************************
#### HEAT MAPS #### 
#* Are forms completed with visit status = 1 or 2 for EACH visit? 
#* PRISMA figure 3a/3b

#* Num: By form: visit type = i AND visit status = 1 or 2 AND passed late window 
#* Denom: Passed window for visit type = i AND didn't closeout AND has not yet delivered
#**************************************************************************************

# PNC
## i filtered out anyone who has an endpreg and then
# for pnc visits where we do not expect those with miscarriages, i filter 
Inf_Heat_Maps_Pnc <- InfData_Pnc_Visits %>%
  filter(LIVEBIRTH==1) %>% 
  left_join(MatData_Pnc_Visits_subset, by = c("SITE", "MOMID", "PREGID")) %>% 
  # filter(!is.na(ENDPREG_DAYS)) %>% ## only include participants with a birth outcome
  select(SITE, MOMID, PREGID, INFANTID, contains("ANY_TYPE_VISIT_COMPLETE_"), contains("TYPE_VISIT"),
         contains("VISIT_COMPLETE"),
         contains("_PASS"), contains("_ONTIME"), contains("_LATE"), M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD, DOB, ENDPREG_DAYS) %>%
  ## generate denominators - LATE
  # has the late window passed AND has not delivered AND has not closed out OR has a form in this visit 
  mutate(HM_PNC0_DENOM_LATE = ifelse((PNC0_PASS_LATE==1 &
                                        ((M24_CLOSE_DSSTDAT > PNC0_LATE) | is.na(M24_CLOSE_DSSTDAT))), 1, 0),
         HM_PNC1_DENOM_LATE = ifelse((PNC1_PASS_LATE==1 &
                                        ((M24_CLOSE_DSSTDAT > PNC1_LATE) | is.na(M24_CLOSE_DSSTDAT))), 1, 0),
         HM_PNC4_DENOM_LATE = ifelse((PNC4_PASS_LATE==1 &
                                        ((M24_CLOSE_DSSTDAT > PNC4_LATE) | is.na(M24_CLOSE_DSSTDAT))), 1, 0),
         HM_PNC6_DENOM_LATE = ifelse((PNC6_PASS_LATE==1 & 
                                        ((M24_CLOSE_DSSTDAT > PNC6_LATE) | is.na(M24_CLOSE_DSSTDAT))), 1, 0),
         
         HM_PNC26_DENOM_LATE = ifelse((PNC26_PASS_LATE==1 & ENDPREG_DAYS>139 & 
                                         ((M24_CLOSE_DSSTDAT > PNC26_LATE) | is.na(M24_CLOSE_DSSTDAT))), 1, 0),
         
         HM_PNC52_DENOM_LATE = ifelse((PNC52_PASS_LATE==1 & ENDPREG_DAYS>139 & is.na(M24_CLOSE_DSSTDAT)) | 
                                        M24_CLOSE_DSDECOD==1, 1, 0)
  )

## export 
save(Inf_Heat_Maps_Pnc, file= paste0(path_to_save, "Inf_Heat_Maps_Pnc",".RData",sep = ""))



#**************************************************************************************
#### BILIRULER #### 
#**************************************************************************************

 
###Set up dataset (infants_livebirths_combined)

infants_livebirths <- inf_outcomes_full %>%
  filter(LIVEBIRTH==1) %>%
  #Add MNH11
  left_join(mnh11,  by = c("SITE", "MOMID", "PREGID","INFANTID")) %>%
  left_join(InfData_Report %>%
              select(SITE, MOMID, PREGID, INFANTID, M11_VISIT_COMPLETE_6), 
            by = c("SITE", "MOMID", "PREGID","INFANTID")) %>% 
  #Add postnatal age (from Stacie's code)
  mutate(M11_VISIT_OBSSTTIM = replace(M11_VISIT_OBSSTTIM, M11_VISIT_OBSSTTIM %in% c("77:77", "99:99", "55:55:00"), NA), # replace default value time with NA 
         M11_VISIT_DATETIME = as.POSIXct(paste(M11_VISIT_OBSSTDAT, M11_VISIT_OBSSTTIM), format= "%Y-%m-%d %H:%M")) %>%  # assign time field type 
  # calculate age (hours and days) at MNH11 visit (if no default value visit date, then calculate)
  mutate(DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME,format="%Y-%m-%d %H:%M")) %>%
  mutate(M11_AGE_AT_VISIT_DATETIME = floor(difftime(M11_VISIT_DATETIME,DELIVERY_DATETIME,units = "hours")))

#Add PNC-0 
mnh13_pnc0 <- mnh13 %>% filter(M13_TYPE_VISIT==7)
mnh14_pnc0 <- mnh14 %>% filter(M14_TYPE_VISIT==7)
infants_livebirths_pnc0 <- infants_livebirths %>%
  left_join(mnh13_pnc0,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(mnh14_pnc0,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  #rename 
  rename_with(~paste0(., "_", 7), .cols = c(contains("M13"), contains("M14")))

#Add PNC-1
mnh13_pnc1 <- mnh13 %>% filter(M13_TYPE_VISIT==8)
mnh14_pnc1 <- mnh14 %>% filter(M14_TYPE_VISIT==8)
infants_livebirths_pnc1 <- infants_livebirths %>%
  left_join(mnh13_pnc1,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(mnh14_pnc1,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  rename_with(~paste0(., "_", 8), .cols = c(contains("M13"), contains("M14"))) %>%
  select("SITE","MOMID","PREGID","INFANTID",contains("M13"),contains("M14"))

#Combine 2 pnc visits
infants_livebirths_pnccombined <- infants_livebirths_pnc0 %>%
  left_join(infants_livebirths_pnc1,by=c("SITE","MOMID","PREGID","INFANTID"))

#Add PNC-4?
mnh13_pnc4 <- mnh13 %>% filter(M13_TYPE_VISIT==9)
mnh14_pnc4 <- mnh14 %>% filter(M14_TYPE_VISIT==9)
infants_livebirths_pnc4 <- infants_livebirths %>%
  left_join(mnh13_pnc4,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(mnh14_pnc4,by=c("SITE","MOMID","PREGID","INFANTID")) %>%
  rename_with(~paste0(., "_", 9), .cols = c(contains("M13"), contains("M14"))) %>%
  select("SITE","MOMID","PREGID","INFANTID",contains("M13"),contains("M14"))

#COMBINE ALL PNC VISITS
infants_livebirths_pnccombined <- infants_livebirths_pnccombined %>%
  left_join(infants_livebirths_pnc4,by=c("SITE","MOMID","PREGID","INFANTID"))

#Add MNH20 (Hospitalizations)
infants_livebirths_combined <- infants_livebirths_pnccombined %>%
  left_join(mnh20,by=c("SITE","MOMID","PREGID","INFANTID"))

#Add MNH24 (Infant closeout)
infants_livebirths_combined <- infants_livebirths_combined %>%
  left_join(mnh24,by=c("SITE","MOMID","PREGID","INFANTID"))%>%
  distinct(INFANTID,.keep_all=TRUE)

#More postnatal age calculations
infants_livebirths_combined <- infants_livebirths_combined %>%
  #PNC-0
  mutate(M14_TCB_OBSSTTIM_7 = replace(M14_TCB_OBSSTTIM_7, M14_TCB_OBSSTTIM_7 %in% c("77:77", "99:99", "55:55:00"), NA)) %>% # replace default value time with NA 
  mutate(M14_VISIT_DATETIME_7 = as.POSIXct(paste(M14_VISIT_OBSSTDAT_7, M14_TCB_OBSSTTIM_7), format= "%Y-%m-%d %H:%M")) %>%  # assign time field type 
  # calculate age (hours and days) at PNC-0 visit (if no default value visit date, then calculate)
  mutate(M14_AGE_AT_VISIT_DATETIME_7 = floor(difftime(M14_VISIT_DATETIME_7,DELIVERY_DATETIME,units = "hours"))) %>%
  #PNC-1
  mutate(M14_TCB_OBSSTTIM_8 = replace(M14_TCB_OBSSTTIM_8, M14_TCB_OBSSTTIM_8 %in% c("77:77", "99:99", "55:55:00"), NA)) %>% # replace default value time with NA 
  mutate(M14_VISIT_DATETIME_8 = as.POSIXct(paste(M14_VISIT_OBSSTDAT_8, M14_TCB_OBSSTTIM_8), format= "%Y-%m-%d %H:%M")) %>%  # assign time field type 
  # calculate age (hours and days) at PNC-1 visit (if no default value visit date, then calculate)
  mutate(M14_AGE_AT_VISIT_DATETIME_8 = floor(difftime(M14_VISIT_DATETIME_8,DELIVERY_DATETIME,units = "hours"))) %>% 
  #PNC-4
  mutate(M14_TCB_OBSSTTIM_9 = replace(M14_TCB_OBSSTTIM_9, M14_TCB_OBSSTTIM_9 %in% c("77:77", "99:99", "55:55:00"), NA)) %>% # replace default value time with NA 
  mutate(M14_VISIT_DATETIME_9 = as.POSIXct(paste(M14_VISIT_OBSSTDAT_9, M14_TCB_OBSSTTIM_9), format= "%Y-%m-%d %H:%M")) %>%  # assign time field type 
  # calculate age (hours and days) at PNC-0 visit (if no default value visit date, then calculate)
  mutate(M14_AGE_AT_VISIT_DATETIME_9 = floor(difftime(M14_VISIT_DATETIME_9,DELIVERY_DATETIME,units = "hours"))) %>%
  
  #Replace values with NAs
  mutate(M11_TBILIRUBIN_UMOLL_LBORRES = replace(M11_TBILIRUBIN_UMOLL_LBORRES,M11_TBILIRUBIN_UMOLL_LBORRES=="-7",NA),
         M14_TCB_UMOLL_LBORRES_7 = replace(M14_TCB_UMOLL_LBORRES_7,M14_TCB_UMOLL_LBORRES_7=="-7",NA),
         M14_TCB_UMOLL_LBORRES_8 = replace(M14_TCB_UMOLL_LBORRES_8,M14_TCB_UMOLL_LBORRES_8=="-7",NA),
         M14_TCB_UMOLL_LBORRES_9 = replace(M14_TCB_UMOLL_LBORRES_9,M14_TCB_UMOLL_LBORRES_9=="-7",NA)
  ) %>%
  
  #Convert units from umol/L to mg/dL at 3 sites
  mutate(M11_TBILIRUBIN_UMOLL_LBORRES = 
           case_when(SITE=="Zambia" | 
                       SITE=="Kenya" | 
                       SITE=="India-SAS" 
                     ~ M11_TBILIRUBIN_UMOLL_LBORRES / 17.1,
                     TRUE ~ M11_TBILIRUBIN_UMOLL_LBORRES )) %>%
  mutate(M14_TCB_UMOLL_LBORRES_7 = 
           case_when(SITE=="Zambia" | 
                       SITE=="Kenya" | 
                       SITE=="India-SAS" 
                     ~ M14_TCB_UMOLL_LBORRES_7 / 17.1,
                     TRUE ~ M14_TCB_UMOLL_LBORRES_7)) %>%
  mutate(M14_TCB_UMOLL_LBORRES_8 = 
           case_when(SITE=="Zambia" | 
                       SITE=="Kenya" | 
                       SITE=="India-SAS" 
                     ~ M14_TCB_UMOLL_LBORRES_8 / 17.1,
                     TRUE ~ M14_TCB_UMOLL_LBORRES_8)) %>%
  mutate(M14_TCB_UMOLL_LBORRES_9 = 
           case_when(SITE=="Zambia" | 
                       SITE=="Kenya" | 
                       SITE=="India-SAS" 
                     ~ M14_TCB_UMOLL_LBORRES_9 / 17.1,
                     TRUE ~ M14_TCB_UMOLL_LBORRES_9)) %>%
  
  #change datetime to make comparisons
  mutate(DEATHDATE_MNH24 = as.POSIXct(DEATHDATE_MNH24,format="%Y-%m-%d")) %>%
  mutate(M11_VISIT_OBSSTDAT=as.POSIXct(M11_VISIT_OBSSTDAT,format="%Y-%m-%d")) %>%
  mutate(M13_VISIT_OBSSTDAT_7=as.POSIXct(M13_VISIT_OBSSTDAT_7,format="%Y-%m-%d")) %>%
  mutate(M13_VISIT_OBSSTDAT_8=as.POSIXct(M13_VISIT_OBSSTDAT_8,format="%Y-%m-%d")) %>%
  mutate(M14_VISIT_OBSSTDAT_7=as.POSIXct(M14_VISIT_OBSSTDAT_7,format="%Y-%m-%d")) %>%
  mutate(M14_VISIT_OBSSTDAT_8=as.POSIXct(M14_VISIT_OBSSTDAT_8,format="%Y-%m-%d")) %>%
  mutate(DOB=as.POSIXct(DOB,format="%Y-%m-%d")) %>%
  mutate(M24_CLOSE_DSSTDAT=as.POSIXct(M24_CLOSE_DSSTDAT,format="%Y-%m-%d"))


#MNH36 PROCESSING STARTS HERE
#pull out duplicates of MNH36

mnh36IPC <- mnh36 %>%
  filter(M36_TYPE_VISIT==6) 
IPCduplicates <- mnh36IPC %>%
  filter(duplicated(INFANTID))
mnh36IPC <- mnh36IPC %>%
  filter(!duplicated(INFANTID))

mnh36PNC0 <- mnh36 %>%
  filter(M36_TYPE_VISIT==7) 
PNC0duplicates <- mnh36PNC0 %>%
  filter(duplicated(INFANTID))
mnh36PNC0 <- mnh36PNC0 %>%
  filter(!duplicated(INFANTID))

mnh36PNC1 <- mnh36 %>%
  filter(M36_TYPE_VISIT==8) 
PNC1duplicates <- mnh36PNC1 %>%
  filter(duplicated(INFANTID))
mnh36PNC1 <- mnh36PNC1 %>%
  filter(!duplicated(INFANTID))

mnh36PNC4 <- mnh36 %>%
  filter(M36_TYPE_VISIT==9) 
PNC4duplicates <- mnh36PNC4 %>%
  filter(duplicated(INFANTID))
mnh36PNC4 <- mnh36PNC4 %>%
  filter(!duplicated(INFANTID))

mnh36PNC6 <- mnh36 %>%
  filter(M36_TYPE_VISIT==10) 
PNC6duplicates <- mnh36PNC6 %>%
  filter(duplicated(INFANTID))
mnh36PNC6 <- mnh36PNC6 %>%
  filter(!duplicated(INFANTID))

mnh36_77 <- mnh36 %>%
  filter(M36_TYPE_VISIT==77) 
duplicates77 <- mnh36_77 %>%
  filter(duplicated(INFANTID))
mnh36_77 <- mnh36_77 %>%
  filter(!duplicated(INFANTID))

#Merge them all back together, without duplicates

mnh36 <- bind_rows(mnh36IPC,mnh36PNC0,mnh36PNC1,mnh36PNC4,mnh36PNC6,mnh36_77)


#In the Bili-ruler protocol, ideally 2 people take measurements using both Bili-ruler and MST. If Bili-ruler measurements differ by 2 or more, or MST measurements differ by 3 or more, then the two people are prompted to work together to take a 3rd measurement. 
#Then the outlier is dropped, and the remaining two are averaged. 
#The function below chooses the outlier and averages the remaining two measurements.

joint_function <- function(BILI_1,BILI_2,BILI_JOINT){
  vec <- sort(c(BILI_1,BILI_2,BILI_JOINT))
  if(abs(vec[3]-vec[2]) > abs(vec[2]-vec[1])){
    return((vec[1]+vec[2])/2)
  }  
  else if(abs(vec[3]-vec[2]) < abs(vec[2]-vec[1])){
    return((vec[3]+vec[2])/2)
  }
  else if(abs(vec[3]-vec[2]) == abs(vec[2]-vec[1])){
    return((vec[1]+vec[2]+vec[3])/3)
  }
}

#Calculate the 'final' bili-ruler and MST value. Only 1 Bili-ruler & 1 MST value are ultimately used in analysis: 
#If there is 1 measurement, use the 1
#if there are 2 measurements, average them
#if there are 3 measurements, use joint_function 
#if there are 0 measurements, mark as 'NA'

mnh36 <- mnh36 %>%
  filter(!is.na(M36_MST_1)& !is.na(M36_MST_2) & !is.na(M36_MST_JOINT) & 
           !is.na(M36_BILI_1) & !is.na(M36_BILI_2) & !is.na(M36_BILI_JOINT)) %>%
  rowwise() %>%
  mutate(M36_BILI_FINAL = case_when(
    #2 valid #s, diff of 1 or 0
    (M36_BILI_1 %in% c(1,2,3,4,5,6) & 
       M36_BILI_2 %in% c(1,2,3,4,5,6) &
       abs(M36_BILI_1 - M36_BILI_2) < 2)
    ~ (M36_BILI_1+M36_BILI_2)/2,
    #2 PRISMA staff, 2 valid IDs, diff of 2 or more, 3rd measurement successful
    (M36_BILI_1 %in% c(1,2,3,4,5,6) & 
       M36_BILI_2 %in% c(1,2,3,4,5,6) & 
       abs(M36_BILI_1 - M36_BILI_2) >= 2 & 
       M36_BILI_JOINT %in% c(1,2,3,4,5,6)) 
    ~ joint_function(M36_BILI_1,M36_BILI_2,M36_BILI_JOINT),
    #2 PRISMA staff, 2 valid IDs, diff of 2 or more, 3rd measurement not valid
    (M36_BILI_1 %in% c(1,2,3,4,5,6) & 
       M36_BILI_2 %in% c(1,2,3,4,5,6) & 
       abs(M36_BILI_1 - M36_BILI_2) >= 2 & 
       !(M36_BILI_JOINT %in% c(1,2,3,4,5,6)))
    ~ (M36_BILI_1+M36_BILI_2)/2,
    #1 valid #
    (M36_BILI_1 %in% c(1,2,3,4,5,6) & 
       !(M36_BILI_2 %in% c(1,2,3,4,4,5,6)))
    ~ M36_BILI_1,
    (M36_BILI_2 %in% c(1,2,3,4,5,6) & 
       !(M36_BILI_1 %in% c(1,2,3,4,4,5,6)))
    ~ M36_BILI_2,
    #no valid #
    !(M36_BILI_1 %in% c(1,2,3,4,5,6)) & 
      !(M36_BILI_2 %in% c(1,2,3,4,4,5,6))
    ~ NA,
    TRUE ~ NA),
    #Same thing for MST values:
    M36_MST_FINAL = case_when(
      #2 valid #s, diff of 1 or 0
      (M36_MST_1 %in% c(1,2,3,4,5,6) & 
         M36_MST_2 %in% c(1,2,3,4,5,6) &
         abs(M36_MST_1 - M36_MST_2) < 2)
      ~ (M36_MST_1+M36_MST_2)/2,
      #2 PRISMA staff, 2 valid IDs, diff of 2 or more, 3rd measurement successful
      (M36_MST_1 %in% c(1,2,3,4,5,6) & 
         M36_MST_2 %in% c(1,2,3,4,5,6) & 
         abs(M36_MST_1 - M36_MST_2) >= 2 & 
         M36_MST_JOINT %in% c(1,2,3,4,5,6))
      ~ joint_function(M36_MST_1,M36_MST_2,M36_MST_JOINT),
      #2 PRISMA staff, 2 valid IDs, diff of 2 or more, 3rd measurement not valid
      (M36_MST_1 %in% c(1,2,3,4,5,6) & 
         M36_MST_2 %in% c(1,2,3,4,5,6) & 
         abs(M36_MST_1 - M36_MST_2) >= 2 & 
         !(M36_MST_JOINT %in% c(1,2,3,4,5,6)))
      ~ (M36_MST_1+M36_MST_2)/2,
      #1 valid #
      (M36_MST_1 %in% c(1,2,3,4,5,6) & 
         !(M36_MST_2 %in% c(1,2,3,4,4,5,6)))
      ~ M36_MST_1,
      (M36_MST_2 %in% c(1,2,3,4,5,6) & 
         !(M36_MST_1 %in% c(1,2,3,4,4,5,6)))
      ~ M36_MST_2,
      #no valid #
      (!(M36_MST_1 %in% c(1,2,3,4,5,6)) & 
         !(M36_MST_2 %in% c(1,2,3,4,4,5,6)))
      ~ NA,
      TRUE ~ NA))


#After duplicates are pulled out, rename variables so they are associated the visit (*_7, etc)
#This next code chunk will rearrange mnh36 to be wide rather than long, and then join it with the previous infants_livebirths_combined dataset (which had MNH11, MNH13, MNH14)

mnh36IPC <- mnh36 %>%
  filter(M36_TYPE_VISIT==6) 
names(mnh36IPC) <- paste0(names(mnh36IPC),"_6")
mnh36IPC <- mnh36IPC %>%
  rename("MOMID" = "MOMID_6",
         "PREGID" = "PREGID_6",
         "INFANTID" = "INFANTID_6",
         "SITE" = "SITE_6")

mnh36PNC0 <- mnh36 %>%
  filter(M36_TYPE_VISIT==7) 
names(mnh36PNC0) <- paste0(names(mnh36PNC0),"_7")
mnh36PNC0 <- mnh36PNC0 %>%
  rename("MOMID" = "MOMID_7",
         "PREGID" = "PREGID_7",
         "INFANTID" = "INFANTID_7",
         "SITE" = "SITE_7")

mnh36PNC1 <- mnh36 %>%
  filter(M36_TYPE_VISIT==8) 
names(mnh36PNC1) <- paste0(names(mnh36PNC1),"_8")
mnh36PNC1 <- mnh36PNC1 %>%
  rename("MOMID" = "MOMID_8",
         "PREGID" = "PREGID_8",
         "INFANTID" = "INFANTID_8",
         "SITE" = "SITE_8") 

mnh36PNC4 <- mnh36 %>%
  filter(M36_TYPE_VISIT==9) 
names(mnh36PNC4) <- paste0(names(mnh36PNC4),"_9")
mnh36PNC4 <- mnh36PNC4 %>%
  rename("MOMID" = "MOMID_9",
         "PREGID" = "PREGID_9",
         "INFANTID" = "INFANTID_9",
         "SITE" = "SITE_9")  

mnh36PNC6 <- mnh36 %>%
  filter(M36_TYPE_VISIT==10)
names(mnh36PNC6) <- paste0(names(mnh36PNC6),"_10")
mnh36PNC6 <- mnh36PNC6 %>% 
  rename("MOMID" = "MOMID_10",
         "PREGID" = "PREGID_10",
         "INFANTID" = "INFANTID_10",
         "SITE" = "SITE_10") 

mnh36_77 <- mnh36 %>%
  filter(M36_TYPE_VISIT==77)
names(mnh36_77) <- paste0(names(mnh36_77),"_77")
mnh36_77 <- mnh36_77 %>% 
  rename("MOMID" = "MOMID_77",
         "PREGID" = "PREGID_77",
         "INFANTID" = "INFANTID_77",
         "SITE" = "SITE_77") 

#Making the wide version of mnh36
mnh36merged <- mnh36IPC %>%
  full_join(mnh36PNC0,by=c("MOMID","PREGID","INFANTID","SITE")) %>%
  full_join(mnh36PNC1,by=c("MOMID","PREGID","INFANTID","SITE")) %>%
  full_join(mnh36PNC4,by=c("MOMID","PREGID","INFANTID","SITE")) %>%
  full_join(mnh36PNC6,by=c("MOMID","PREGID","INFANTID","SITE")) %>%
  full_join(mnh36_77,by=c("MOMID","PREGID","INFANTID","SITE")) 

#Add mnh36 data to infants_livebirths_combined (all still wide version)

InfData_BiliRuler <- infants_livebirths_combined %>%
  full_join(mnh36merged,by=c("MOMID","PREGID","INFANTID","SITE")) %>%
  mutate(DOB = as.Date(DOB)) %>%
  mutate(M13_VISIT_OBSSTDAT_9 = as.Date(M13_VISIT_OBSSTDAT_9),
         M14_VISIT_OBSSTDAT_9 = as.Date(M14_VISIT_OBSSTDAT_9)) %>%
  #HERE WE ADD STACIE'S DATASETS FROM THE MONITORING REPORT; these must be present for the code to work
  left_join(Inf_Visit_Complete_Pnc %>% select(-DOB), by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(Inf_Form_Completion_Pnc, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  left_join(Inf_Prot_Compliance_Pnc, by = c("SITE","MOMID","PREGID","INFANTID")) %>%
  mutate(
    #Add the Bili-ruler study start date, which helps us determine which infants we expect to be enrolled in the Bili-ruler substudy
    STUDYSTARTDATE = case_when(
      SITE=="Ghana" ~ NA,
      SITE=="India-CMC" ~ as.Date(strptime("2024-11-13",format="%Y-%m-%d")),
      SITE=="India-SAS" ~ as.Date(strptime("2024-12-03",format="%Y-%m-%d")),
      SITE=="Kenya" ~ as.Date(strptime("2025-01-13",format="%Y-%m-%d")),
      SITE=="Pakistan" ~ as.Date(strptime("2024-10-24",format="%Y-%m-%d")),
      SITE=="Zambia" ~ as.Date(strptime("2024-11-04",format="%Y-%m-%d")), 
      TRUE ~ NA)
  ) %>% 
  select(SITE, DOB, M11_VISIT_COMPLETE_6, M11_VISIT_OBSSTDAT, STUDYSTARTDATE,
         M36_VISIT_OBSSTDAT_6, contains("M36_TYPE_VISIT"),contains("M36_VISIT_OBSSTDAT"),
         contains("M36_INF_VISIT_MNH36"), VC_PNC0_NUM_LATE, VC_PNC1_NUM_LATE, VC_PNC4_NUM_LATE,
         VC_PNC6_NUM_LATE)

## export 
save(InfData_BiliRuler, file= paste0(path_to_save, "InfData_BiliRuler",".RData",sep = ""))
