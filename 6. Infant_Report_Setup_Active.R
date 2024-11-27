#*****************************************************************************
#### MONITORING REPORT SETUP -- INFANT ####
#* Function: Merge all forms together in wide format to create a dataset with one row for each woman for each visit 
#* Input: .RData files for each form (generated from 1. data import code) and infant outcomes (generated in code linked here: https://github.com/PRiSMA-Study/PRISMA-Public/blob/main/PRISMA-Infant-Constructed-Outcomes/Infant-Constructed-Variables.R)
#* Last updated: 25 October 2024


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
