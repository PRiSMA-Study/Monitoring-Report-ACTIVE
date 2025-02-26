#*****************************************************************************
#* MATERNAL WIDE DATASET BY VISIT 
#*Function: Merge all forms together in wide format to create a dataset with one row for each woman for each visit 
#*Input: .RData files for each form (generated from 1. data import code)
#* Last updated: 25 February 2025

#*Output:   
#* 1. MatData_Wide.RData wide dataset by MOMID and visit type (one row for each woman at each visit)
#* 2. MatData_Wide_Visit.RData: wide dataset by MOMID (one row for each woman)


#* STEPS: 
#* 1. Import merged data 
#* 2. Add date variable with consistent naming across forms 
#* 3. Update mnh12 because the visit type variable naming is different
#* 4. Hard code visit type for M00, M03, M16, M09, M10, M17 (These forms are only filled out at one time point and don't have a visit type variable)
#* 5. Merge MNH00-02 to get MOMID into screening and ultrasound forms 
#* 6. Merge all ANC forms 
#* 7. Add prefix for all ANC visit to each variable 
#* 8. Merge all IPC forms 
#* 9. Merge all PNC forms 
#* 10. Add suffix for all PNC visits to each variable
#* 11. Remove all duplicates in the dataset - these are remove here but will be flagged in queries
#* 12. Merge ANC/IPC/PNC by SITE, SCRNID, MOMID, PREGID, VISIT TYPE - MatData_Wide_Visit.RData
#* 13. Merge ANC/IPC/PNC by SITE, SCRNID, MOMID, PREGID - MatData_Wide.RData

## LINES TO UPDATE EACH RUN: 
## LINE 35 - set upload date
#*****************************************************************************

library(lubridate)
library(readxl)
library(tidyverse)

UploadDate = "2025-02-21"
#*****************************************************************************
#* Import merged data 
#*****************************************************************************
setwd(paste0("~/Monitoring Report/data/merged/", UploadDate, sep = ""))

## import all rda files 
rdata_files = list.files(pattern="*.RData")
walk(rdata_files, ~ suppressWarnings(load(.x, .GlobalEnv)))

## only pull the variables we need for monitoring report 
MatNames_sheet <- read_excel("~/Monitoring Report/code/varNames_sheet.xlsx", sheet = "MaternalVars")

#*****************************************************************************
#* Assign expected visit type and rename "date" varnames 
#*****************************************************************************
m26_merged <- m26_merged %>% 
  mutate(M26_FTGE_OBSTDAT = parse_date_time(M26_FTGE_OBSTDAT, order = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))

## Add date variable with consistent naming across forms 
if (exists("m01_merged") == TRUE){ m01_merged <- m01_merged %>% mutate(VISIT_DATE = M01_US_OHOSTDAT) }
if (exists("m03_merged") == TRUE){ m03_merged <- m03_merged %>% mutate(VISIT_DATE = M03_SD_OBSSTDAT) }
if (exists("m04_merged") == TRUE){ m04_merged <- m04_merged %>% mutate(VISIT_DATE = M04_ANC_OBSSTDAT) }
if (exists("m05_merged") == TRUE){ m05_merged <- m05_merged %>% mutate(VISIT_DATE = M05_ANT_PEDAT) }
if (exists("m06_merged") == TRUE){ m06_merged <- m06_merged %>% mutate(VISIT_DATE = M06_DIAG_VSDAT) }
if (exists("m07_merged") == TRUE){ m07_merged <- m07_merged %>% mutate(VISIT_DATE = M07_MAT_SPEC_COLLECT_DAT) }
if (exists("m08_merged") == TRUE){ m08_merged <- m08_merged %>% mutate(VISIT_DATE = M08_LBSTDAT) }
#if (exists("m09_merged") == TRUE){ m09_merged <- m09_merged %>% mutate(VISIT_DATE = M09_MAT_LD_OHOSTDAT) }
if (exists("m10_merged") == TRUE){ m10_merged <- m10_merged %>% mutate(VISIT_DATE = M10_FORMCOMPLDAT_MNH10) }
if (exists("m11_merged") == TRUE){ m11_merged <- m11_merged %>% mutate(VISIT_DATE = M11_VISIT_OBSSTDAT) }
if (exists("m12_merged") == TRUE){ m12_merged <- m12_merged %>% mutate(VISIT_DATE = M12_VISIT_OBSSTDAT) }
if (exists("m13_infantmerged") == TRUE){ m13_infantmerged <- m13_infantmerged %>% mutate(VISIT_DATE = M13_VISIT_OBSSTDAT) }
if (exists("m14_infantmerged") == TRUE){ m14_infantmerged <- m14_infantmerged %>% mutate(VISIT_DATE = M14_VISIT_OBSSTDAT) }
if (exists("m15_infantmerged") == TRUE){ m15_infantmerged <- m15_infantmerged %>% mutate(VISIT_DATE = M15_OBSSTDAT) }
if (exists("m16_merged") == TRUE){ m16_merged <- m16_merged %>% mutate(VISIT_DATE = M16_VISDAT) }
if (exists("m17_merged") == TRUE){ m17_merged <- m17_merged %>% mutate(VISIT_DATE = M17_VISDAT) }
if (exists("m18_merged") == TRUE){ m18_merged <- m18_merged %>% mutate(VISIT_DATE = M18_VISDAT) }
if (exists("m19_merged") == TRUE){ m19_merged <- m19_merged %>% mutate(VISIT_DATE = M19_OBSSTDAT) }
if (exists("m20_merged") == TRUE){ m20_merged <- m20_merged %>% mutate(VISIT_DATE = M20_OBSSTDAT) }
if (exists("m21_merged") == TRUE){ m21_merged <- m21_merged %>% mutate(VISIT_DATE = M21_AESTDAT) }
if (exists("m22_merged") == TRUE){ m22_merged <- m22_merged %>% mutate(VISIT_DATE = M22_DVSTDAT) }
#if (exists("m23_merged") == TRUE){ m23_merged <- m23_merged %>% mutate(VISIT_DATE = M23_CLOSE_DSSTDAT) }
if (exists("m24_merged") == TRUE){ m24_merged <- m24_merged %>% mutate(VISIT_DATE = M24_CLOSE_DSSTDAT) }
if (exists("m25_merged") == TRUE){ m25_merged <- m25_merged %>% mutate(VISIT_DATE = M25_OBSSTDAT) }
if (exists("m25_merged") == TRUE){ m25_merged <- m25_merged %>% mutate(VISIT_DATE = ymd(VISIT_DATE)) }
if (exists("m26_merged") == TRUE){ m26_merged <- m26_merged %>% mutate(VISIT_DATE = M26_FTGE_OBSTDAT) }
if (exists("m26_merged") == TRUE){ m26_merged <- m26_merged %>% mutate(VISIT_DATE = ymd(VISIT_DATE)) }

## Hard code visit type for M00, M03, M16, M09, M10, M11, M17
if (exists("m00_merged") == TRUE){ m00_merged$TYPE_VISIT = 1 }
if (exists("m02_merged") == TRUE){ m02_merged$TYPE_VISIT = 1 }
if (exists("m03_merged") == TRUE){ m03_merged$M03_TYPE_VISIT = 1 }
if (exists("m09_merged") == TRUE){ m09_merged$M09_TYPE_VISIT = 6}
if (exists("m10_merged") == TRUE){ m10_merged$M10_TYPE_VISIT = 6}
if (exists("m11_merged") == TRUE){ m11_merged$M11_TYPE_VISIT = 6}
if (exists("m16_merged") == TRUE){ m16_merged$M16_TYPE_VISIT = 5}
if (exists("m17_merged") == TRUE){ m17_merged$M17_TYPE_VISIT = 6}
if (exists("m18_merged") == TRUE){ m18_merged$M18_TYPE_VISIT = 12 }

## only want to look at maternal adverse events 
if (exists("m21_merged") == TRUE){ m21_merged <- m21_merged %>% filter(M21_AETERM %in% c(1,4,88)) }

## Remove momid and pregid from MNH00 to merge -- we are going to only have the momid and pregid as defined in m02
m01_enroll <- m01_merged %>% filter(M01_TYPE_VISIT == 1) %>% ## only want enrollment visit 
  select(-MOMID, -PREGID) %>%  # merge in MOMID and PREGID from mnh02 later 
  rename("TYPE_VISIT" = M01_TYPE_VISIT) %>% 
  # filter out any ultrasound visit dates that are 07-07-1907
  filter(M01_US_OHOSTDAT != ymd("1907-07-07")) %>%   ## FOR KENYA DATA, THIS WILL BE 2007-07-07
  # calculate us ga in days with reported ga in wks + days. if ga is -7 or -5, replace with NA
  ## combine ga weeks and days variables to get a single gestational age variable
  mutate(GA_US_DAYS_FTS1 =  ifelse(!SITE %in% c("India-CMC", "India-SAS") & M01_US_GA_WKS_AGE_FTS1!= -7 & M01_US_GA_DAYS_AGE_FTS1 != -7,  (M01_US_GA_WKS_AGE_FTS1 * 7 + M01_US_GA_DAYS_AGE_FTS1), NA), 
         GA_US_DAYS_FTS2 =  ifelse(!SITE %in% c("India-CMC", "India-SAS") & M01_US_GA_WKS_AGE_FTS2!= -7 & M01_US_GA_DAYS_AGE_FTS2 != -7,  (M01_US_GA_WKS_AGE_FTS2 * 7 + M01_US_GA_DAYS_AGE_FTS2), NA),
         GA_US_DAYS_FTS3 =  ifelse(!SITE %in% c("India-CMC", "India-SAS") & M01_US_GA_WKS_AGE_FTS3!= -7 & M01_US_GA_DAYS_AGE_FTS3 != -7,  (M01_US_GA_WKS_AGE_FTS3 * 7 + M01_US_GA_DAYS_AGE_FTS3), NA),
         GA_US_DAYS_FTS4 =  ifelse(!SITE %in% c("India-CMC", "India-SAS") & M01_US_GA_WKS_AGE_FTS4!= -7 & M01_US_GA_DAYS_AGE_FTS4 != -7,  (M01_US_GA_WKS_AGE_FTS4 * 7 + M01_US_GA_DAYS_AGE_FTS4), NA)) %>% 
  ## combine ga weeks and days variables to get a single gestational age variable - CMC is using acog - use this here 
  mutate(GA_US_DAYS_FTS1 =  ifelse(SITE %in% c("India-CMC", "India-SAS") & M01_CAL_GA_WKS_AGE_FTS1!= -7 & M01_CAL_GA_DAYS_AGE_FTS1 != -7,  (M01_CAL_GA_WKS_AGE_FTS1 * 7 + M01_CAL_GA_DAYS_AGE_FTS1), GA_US_DAYS_FTS1), 
         GA_US_DAYS_FTS2 =  ifelse(SITE %in% c("India-CMC", "India-SAS") & M01_CAL_GA_WKS_AGE_FTS2!= -7 & M01_CAL_GA_DAYS_AGE_FTS2 != -7,  (M01_CAL_GA_WKS_AGE_FTS2 * 7 + M01_CAL_GA_DAYS_AGE_FTS2), GA_US_DAYS_FTS2),
         GA_US_DAYS_FTS3 =  ifelse(SITE %in% c("India-CMC", "India-SAS") & M01_CAL_GA_WKS_AGE_FTS3!= -7 & M01_CAL_GA_DAYS_AGE_FTS3 != -7,  (M01_CAL_GA_WKS_AGE_FTS3 * 7 + M01_CAL_GA_DAYS_AGE_FTS3), GA_US_DAYS_FTS3),
         GA_US_DAYS_FTS4 =  ifelse(SITE %in% c("India-CMC", "India-SAS") & M01_CAL_GA_WKS_AGE_FTS4!= -7 & M01_CAL_GA_DAYS_AGE_FTS4 != -7,  (M01_CAL_GA_WKS_AGE_FTS4 * 7 + M01_CAL_GA_DAYS_AGE_FTS4), GA_US_DAYS_FTS4)) %>% 
  #  pull the largest GA for multiple fetuses + convert to weeks
  mutate(US_GA_DAYS_ENROLL = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% ## where GA_US_DAYS_FTSx is the reported GA by ultrasound (added together M01_US_GA_WKS_AGE_FTSx and M01_US_GA_DAYS_AGE_FTSx to get a single estimate in days)
  mutate(US_GA_WKS_ENROLL = US_GA_DAYS_ENROLL %/% 7) %>% 
  #  convert ga by LMP to days and wks
  mutate(LMP_GA_DAYS_ENROLL =  ifelse(M01_GA_LMP_WEEKS_SCORRES != -7 & M01_GA_LMP_DAYS_SCORRES != -7,  (M01_GA_LMP_WEEKS_SCORRES * 7 + M01_GA_LMP_DAYS_SCORRES), NA)) %>% 
  mutate(LMP_GA_WKS_ENROLL = LMP_GA_DAYS_ENROLL %/% 7) %>%
  ## generate indicator variable for missing US 
  mutate(MISSING_BOTH_US_LMP = ifelse((US_GA_WKS_ENROLL < 0 & LMP_GA_WKS_ENROLL < 0) | 
                                        (is.na(US_GA_WKS_ENROLL) & is.na(LMP_GA_WKS_ENROLL)), 1, 0)) %>% 
  #  calculate the difference in days between reported LMP and reported US
  mutate(GA_DIFF_DAYS = LMP_GA_DAYS_ENROLL-US_GA_DAYS_ENROLL) %>%
  #  obtain best obstetric estimate in weeks
  mutate(BOE_GA_DAYS_ENROLL = case_when(LMP_GA_DAYS_ENROLL %/% 7 < 9 ~
                                          if_else(abs(GA_DIFF_DAYS) <= 5,
                                                  LMP_GA_DAYS_ENROLL,
                                                  US_GA_DAYS_ENROLL),
                                        LMP_GA_DAYS_ENROLL %/% 7 < 16 ~
                                          if_else(abs(GA_DIFF_DAYS) <=7,
                                                  LMP_GA_DAYS_ENROLL, US_GA_DAYS_ENROLL),
                                        LMP_GA_DAYS_ENROLL %/% 7 >= 16 ~
                                          if_else(abs(GA_DIFF_DAYS) <=10,
                                                  LMP_GA_DAYS_ENROLL, US_GA_DAYS_ENROLL),
                                        TRUE ~ US_GA_DAYS_ENROLL)) %>%
  mutate(BOE_GA_WKS_ENROLL = BOE_GA_DAYS_ENROLL %/% 7) %>% 
  # generate EDD based on BOE 
  # "zero out" GA and obtain the estimated "date of conception" 
  mutate(PREG_START_DATE = M01_US_OHOSTDAT - US_GA_DAYS_ENROLL) %>% 
  # add 280 days to PREG_START_DATE to generate EDD based on BOE 
  mutate(EDD_BOE = PREG_START_DATE + 280) %>% 
  ## EDD based on ultrasound 
  mutate(EDD_US =  PREG_START_DATE + 280) %>% 
  group_by(SCRNID) %>%
  arrange(-desc(M01_US_OHOSTDAT)) %>%
  slice(1) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  select(-n)


## Only select ID variables from MNH02 to merge into MNH01 
m02_wide <- m02_merged %>% select(SITE, SCRNID, MOMID, PREGID, M02_SCRN_OBSSTDAT)  %>% 
  mutate(MOMID = ifelse(str_detect(MOMID, "n/a"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "n/a"), NA, PREGID)) %>% 
  mutate(MOMID = ifelse(str_detect(MOMID, "N/A"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "N/A"), NA, PREGID)) %>% 
  mutate(MOMID = case_when(MOMID == "" ~ NA, TRUE ~ MOMID),
         PREGID = case_when(PREGID == "" ~ NA, TRUE ~ PREGID),
  ) %>% 
  group_by(SCRNID) %>%
  arrange(-desc(M02_SCRN_OBSSTDAT)) %>%
  slice(1) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  select(-n) 


## Merge enrollment form with US form to get GA at enrollment -- SUBSET OF DATA 
enroll_bind <- full_join(m02_wide, m01_enroll, by = c("SITE", "SCRNID")) %>% distinct()
enroll_bind <- enroll_bind %>% relocate(c(MOMID,PREGID), .after = SCRNID) %>% 
  select(SITE,SCRNID, MOMID, PREGID, PREG_START_DATE, US_GA_DAYS_ENROLL, 
         US_GA_WKS_ENROLL, EDD_US,EDD_BOE, BOE_GA_DAYS_ENROLL, M02_SCRN_OBSSTDAT) %>% distinct() 


## merge momid and pregid into mnh01 to merge in later 
m02_ids <- m02_merged %>% 
  group_by(SCRNID) %>%
  arrange(-desc(M02_SCRN_OBSSTDAT)) %>%
  slice(1) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  select(-n) %>% select(SITE, SCRNID, MOMID, PREGID) %>% ## export mnh02 ids
  mutate(MOMID = ifelse(str_detect(MOMID, "n/a"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "n/a"), NA, PREGID)) %>% 
  mutate(MOMID = ifelse(str_detect(MOMID, "N/A"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "N/A"), NA, PREGID)) %>% 
  mutate(MOMID = case_when(MOMID == "" ~ NA, TRUE ~ MOMID),
         PREGID = case_when(PREGID == "" ~ NA, TRUE ~ PREGID),
  ) 


m01_merged_enroll <- m01_merged %>% filter(M01_TYPE_VISIT == 1)  %>% 
  select(-MOMID, -PREGID) %>%
  left_join(m02_ids, by = c("SITE", "SCRNID"))

# mnh01_all_visits <- m01_merged %>% filter(M01_TYPE_VISIT != 1)
mnh01_all_visits_missing_id <- m01_merged %>% 
  filter(M01_TYPE_VISIT != 1) %>% 
  filter((is.na(MOMID) | is.na(PREGID)) & !is.na(SCRNID)) %>% 
  select(-MOMID, -PREGID) %>% 
  left_join(m02_ids, by = c("SITE", "SCRNID")) 

mnh01_all_visits_nomissing_id <- m01_merged %>% 
  filter(M01_TYPE_VISIT != 1) %>% 
  filter(!is.na(MOMID) & !is.na(PREGID)) 

m01_merged <- bind_rows(m01_merged_enroll, mnh01_all_visits_missing_id, mnh01_all_visits_nomissing_id) # rebind data

## generate sub datasets for MNH00, MNH01, MNH02, and MNH03 to bind in later 
# (since these are filled out at only one visit, we want to merge them in separately )
m01_to_bind <- m01_merged %>% 
  # remove unscheduled visits 
  filter(M01_TYPE_VISIT!=13, M01_TYPE_VISIT!=14) %>% 
  # merge in enrollment data including BOE and PREG_START_DATE
  left_join(enroll_bind[c("SITE", "SCRNID", "MOMID", "PREGID", "PREG_START_DATE",
                          "BOE_GA_DAYS_ENROLL", "EDD_BOE")], by = c("SITE", "SCRNID", "MOMID", "PREGID")) %>% 
  # generate gestational age at each visit 
  mutate(M01_GESTAGE_AT_VISIT_DAYS = as.numeric(VISIT_DATE - PREG_START_DATE, na.rm= TRUE)) %>% 
  # add in variable for GA at visit in WEEKS 
  mutate(M01_GESTAGE_AT_VISIT_WKS = as.numeric(VISIT_DATE - PREG_START_DATE, na.rm= TRUE) %/% 7) %>% 
  # generate visit complete variable 
  mutate(M01_VISIT_COMPLETE = ifelse(M01_MAT_VISIT_MNH01 == 1 | M01_MAT_VISIT_MNH01 == 2, 1, 0)) %>%
  # rename visit date 
  rename("M01_VISIT_DATE" = VISIT_DATE,
         "TYPE_VISIT" = M01_TYPE_VISIT) %>% 
  select(-PREG_START_DATE, -BOE_GA_DAYS_ENROLL, -EDD_BOE) %>% 
  group_by(SITE, SCRNID, TYPE_VISIT) %>%
  arrange(-desc(M01_VISIT_DATE)) %>%
  slice(1) %>%
  mutate(n=n()) %>%
  ungroup() 

m00_to_bind <- m00_merged %>% 
  mutate(TYPE_VISIT = 1)

m02_to_bind <- m02_merged %>% 
  mutate(TYPE_VISIT = 1) %>% 
  # remove visit date as we will have already included it in the enrollment data subset above
  # leaving it in here will cause duplicates
  select(-M02_SCRN_OBSSTDAT) %>% 
  mutate(MOMID = ifelse(str_detect(MOMID, "n/a"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "n/a"), NA, PREGID)) %>% 
  mutate(MOMID = ifelse(str_detect(MOMID, "N/A"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "N/A"), NA, PREGID)) %>% 
  mutate(MOMID = case_when(MOMID == "" ~ NA, TRUE ~ MOMID),
         PREGID = case_when(PREGID == "" ~ NA, TRUE ~ PREGID),
  )


m03_to_bind <- m03_merged %>% 
  # merge in enrollment data including BOE and PREG_START_DATE
  left_join(enroll_bind[c("SITE", "MOMID", "PREGID", "PREG_START_DATE")], by = c("SITE", "MOMID", "PREGID")) %>% 
  # generate gestational age at each visit 
  mutate(M03_GESTAGE_AT_VISIT_DAYS = as.numeric(VISIT_DATE - PREG_START_DATE, na.rm= TRUE)) %>% 
  # add in variable for GA at visit in WEEKS 
  mutate(M03_GESTAGE_AT_VISIT_WKS = as.numeric(VISIT_DATE - PREG_START_DATE, na.rm= TRUE) %/% 7) %>% 
  # generate visit complete variable 
  mutate(M03_VISIT_COMPLETE = ifelse(M03_MAT_VISIT_MNH03 == 1 | M03_MAT_VISIT_MNH03 == 2, 1, 0)) %>%
  # rename visit date 
  rename("M03_VISIT_DATE" = VISIT_DATE) %>% 
  # add visit type 
  mutate(TYPE_VISIT = 1) %>% 
  select(-PREG_START_DATE) 

## test for duplicates:
test <- enroll_bind %>% group_by(SITE,SCRNID, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1 ) 
dim(test)
test <- m01_to_bind %>% group_by(SITE, SCRNID, MOMID, PREGID, TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
dim(test)
test <- m02_to_bind %>%  group_by(SITE,SCRNID, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1 )
dim(test)
test <- m03_to_bind %>% group_by(SITE,MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1) 
dim(test)

test <- m00_to_bind %>% group_by(SITE,SCRNID) %>% mutate(n=n()) %>% filter(n>1) 
dim(test)

#*****************************************************************************
#* ANC FORMS 
#*****************************************************************************
## Compile all ANC merged forms into list
anc_out <- list(m04_merged, m05_merged, m06_merged, m07_merged, m08_merged, m25_merged, m26_merged)
names(anc_out) <- c("m04", "m05", "m06", "m07", "m08", "m25", "m26")

## Merge enrollment forms with each of the other forms 
anc_data <- list()
for (i in names(anc_out)) {
  anc_data[[i]] <- full_join(anc_out[[i]], enroll_bind, by = c("SITE", "MOMID", "PREGID"), multiple = "all") %>% distinct()
}

# Extract ANC visits in data 
anc_visits <- c(1,2,3,4,5)
for (i in names(anc_out)) {
  anc_data[[i]] <- anc_data[[i]] %>% mutate(TYPE_VISIT = if_all(matches("(.+)_TYPE_VISIT"))) %>% 
    filter(TYPE_VISIT %in% anc_visits)
}

for (i in anc_visits) {
  print(i)
}

## extract anc variables 
for (i in names(anc_out)) {
  # List of target variables
  target_vars <- c(
    as.character(MatNames_sheet$varname),
    "US_GA_WKS_ENROLL", "US_GA_DAYS_ENROLL", "M02_SCRN_OBSSTDAT", "EDD_US", "EDD_BOE",
    "BOE_GA_DAYS_ENROLL", "PREG_START_DATE", "VISIT_DATE", "M00_KNOWN_DOBYN_SCORRES",
    "M02_SCRN_RETURN"
  )
  
  # List of target patterns
  target_patterns <- c(
    "TYPE_VISIT", "_VISIT_COMPLETE", "M04_ANC_OBSSTDAT_", "M12_VISIT_OBSSTDAT_",
    "GESTAGE_AT_VISIT_DAYS", "GESTAGE_AT_VISIT_WKS", "GESTAGE_AT_BIRTH_",
    "_PNC_AT_VISIT_", "_GA_LMP_DAYS_SCORRES_", "M04_FETAL_LOSS_DSSTDAT",
    "M09_DELIV_DSSTDAT_INF", "RBC_G6PD_LBORRES", "RBC_THALA_LBORRES"
  )
  
  # Selecting columns that exist in the current data frame
  anc_data[[i]] <- anc_data[[i]] %>%
    select(intersect(target_vars, names(.)),
           intersect(grep(paste(target_patterns, collapse = "|"), names(.), value = TRUE), names(.)))
}

## Organize Data: 
## Remove M00-M03 (these are only filled out at screening/enrollment and do not require the same updates)
## Add binary ANC Yes/No column (1 = Yes, 0 = No)
## Calculate gestational age at visit for each form
## Generate variable for visit complete 
# If the visit status variable (MAT_VISIT_MNH##) reports options 1 (conducted in person) or 2 (conducted by phone), then the visit is considered complete 
## Extract the maximum GA at screening US for each woman 

anc_visit_out <- list()
for(i in names(anc_data)){
  
  anc_visit_out[[i]] <- anc_data[[i]] %>% 
    rowwise() %>%
    group_by(SCRNID, MOMID, PREGID, SITE) %>% 
    ungroup() %>% 
    # add in variable for GA at visit in DAYS 
    mutate(!!paste0(toupper(i),quo_name("_GESTAGE_AT_VISIT_DAYS")) := as.numeric(VISIT_DATE - PREG_START_DATE, na.rm= TRUE)) %>% 
    # add in variable for GA at visit in WEEKS 
    mutate(!!paste0(toupper(i),quo_name("_GESTAGE_AT_VISIT_WKS")) := as.numeric(VISIT_DATE - PREG_START_DATE, na.rm= TRUE) %/% 7) %>% 
    # add in binary variable for ANC (Yes == 1, No == 0) 
    mutate(!!paste0(toupper(i),quo_name("_ANC_YN")) := 1) %>% 
    # add in binary variable if visit is complete 
    mutate(!!paste0(toupper(i),quo_name("_VISIT_COMPLETE")) := ifelse(if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 1 | if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 2, 1, 0)) %>% 
    ## add prefix to new variables 
    rename(!!paste0(toupper(i),quo_name("_VISIT_DATE")) := "VISIT_DATE")  %>%
    ## move type_visit to the front of each dataset
    relocate(c("TYPE_VISIT"), .after = PREGID) %>% 
    filter(TYPE_VISIT != 13, TYPE_VISIT != 14) 
}

# Merge all ANC (minus MNH00-03) forms together 
anc_data_wide <- anc_visit_out %>% reduce(full_join, by =  c("SITE","SCRNID", "MOMID", "PREGID", "TYPE_VISIT",
                                                             "US_GA_DAYS_ENROLL", "US_GA_WKS_ENROLL", "PREG_START_DATE",
                                                             "M02_SCRN_OBSSTDAT","BOE_GA_DAYS_ENROLL","EDD_BOE", "EDD_US")) %>% distinct()


# Merge MNH00,MNH01, MNH02, and MNH03 back into the data using the full merged dataset created above (dataframe name = enroll_bind_all)
anc_data_wide <- anc_data_wide %>% 
  full_join(m01_to_bind, by = c("SITE", "SCRNID", "MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(m00_to_bind, by = c("SITE","SCRNID", "TYPE_VISIT")) %>%
  full_join(m02_to_bind, by = c("SITE","SCRNID","MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(m03_to_bind, by = c("SITE","MOMID", "PREGID", "TYPE_VISIT")) %>%
  distinct() %>% 
  group_by(SCRNID) %>%
  fill(M00_SCRN_OBSSTDAT, .direction = "downup") %>%
  ungroup() 


## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
anc_data_wide_visit = anc_data_wide

## Move MNH00 data to the front the dataframe 
m00_to_move <- grep("M00_", names(anc_data_wide_visit))
anc_data_wide_visit<- anc_data_wide_visit %>% 
  relocate(TYPE_VISIT, .after = PREGID) %>% 
  relocate(any_of(m00_to_move), .after = TYPE_VISIT)

## Final maternal wide dataset by visit type == anc_data_wide_visit 

## IN ORDER TO MAKE THE DATA WIDE WITH ONE ROW FOR EACH WOMAN, WE NEED TO ADD A PREFIX TO ALL FORMS THAT ARE FILLED OUT AT MULTIPLE VISITS 
## The following code will do the following
# 1. Extract the all visit types to their own dataset 
# 2. Add a suffix to the end of each variable to represent the visit type 
## Example: data from visit type = 1 would have variables named "HEIGHT_PERES_1" 
## Extract Visit 1 (ANC < 20)
## extract screening ids with missign screening dates 

scrnid_to_remove <- anc_data_wide %>% filter(is.na(M00_SCRN_OBSSTDAT)) %>%
  select(SITE, MOMID, PREGID, SCRNID, M00_SCRN_OBSSTDAT, M01_US_OHOSTDAT, TYPE_VISIT) %>% 
  filter(!is.na(SCRNID) & SCRNID != "") %>% 
  distinct(SCRNID, .keep_all = TRUE)

visit_anc_1 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 1, !is.na(SCRNID)) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 1,
         M02_VISIT_COMPLETE = case_when(
           !is.na(M02_SCRN_OBSSTDAT) ~ 1,
           is.na(M02_SCRN_OBSSTDAT) ~ 0)) %>% 
  rename_with(~paste0(., "_", 1), 
              .cols = -c("SITE", "SCRNID", "MOMID", "PREGID", contains("M02"), contains("M00"), 
                         "PREG_START_DATE", "US_GA_DAYS_ENROLL", 
                         "US_GA_WKS_ENROLL", "EDD_US", "EDD_BOE", "BOE_GA_DAYS_ENROLL")) %>% 
  filter(!SCRNID %in% as.vector(scrnid_to_remove$SCRNID)) 

## NOTE: Forms that only get filled out at enrollment (visit 1) (MNH00, MNH02, MNH03) will need to be removed from the other visit data as to not have duplicates
m00_to_remove <- grep("M00_", names(anc_data_wide_visit))
m02_to_remove <- grep("M02_", names(anc_data_wide_visit))
m03_to_remove <- grep("M03_", names(anc_data_wide_visit))
baseline_to_remove <- c("PREG_START_DATE", "US_GA_DAYS_ENROLL", "US_GA_WKS_ENROLL",
                        "EDD_US", "EDD_BOE", "BOE_GA_DAYS_ENROLL")

## Extract Visit 2 (ANC 20)
visit_anc_2 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 2) %>% 
  select(-any_of(m00_to_remove),
         -any_of(m02_to_remove),
         -any_of(m03_to_remove), 
         -any_of(baseline_to_remove), 
         -TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 2) %>% # US variable "TYPE_VISIT" exists, but does not have the "M01_" prefix -- add here 
  rename_with(~paste0(., "_", 2), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) %>% 
  filter(!SCRNID %in% as.vector(scrnid_to_remove$SCRNID)) 

## Extract Visit 3 (ANC 28)
visit_anc_3 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 3) %>% 
  select(-any_of(m00_to_remove),
         -any_of(m02_to_remove),
         -any_of(m03_to_remove), 
         -any_of(baseline_to_remove), 
         -TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 3) %>% # US variable "TYPE_VISIT" exists, but does not have the "M01_" prefix -- add here 
  rename_with(~paste0(., "_", 3), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) %>% 
  filter(!SCRNID %in% as.vector(scrnid_to_remove$SCRNID))

## Extract Visit 4 (ANC 32)
visit_anc_4 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 4) %>% 
  select(-any_of(m00_to_remove),
         -any_of(m02_to_remove),
         -any_of(m03_to_remove), 
         -any_of(baseline_to_remove),
         -TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 4) %>% # US variable "TYPE_VISIT" exists, but does not have the "M01_" prefix -- add here 
  rename_with(~paste0(., "_", 4), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) %>% 
  filter(!SCRNID %in% as.vector(scrnid_to_remove$SCRNID)) %>% 
  filter(!is.na(SCRNID))


## Extract Visit 5 (ANC 36)
visit_anc_5 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 5) %>% 
  select(-any_of(m00_to_remove),
         -any_of(m02_to_remove),
         -any_of(m03_to_remove),
         -any_of(baseline_to_remove), 
         -TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 5) %>% # US variable "TYPE_VISIT" exists, but does not have the "M01_" prefix -- add here 
  rename_with(~paste0(., "_", 5), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) %>% 
  filter(!SCRNID %in% as.vector(scrnid_to_remove$SCRNID))

# check or duplicates
test1 <- visit_anc_1 %>% group_by(SITE,SCRNID, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
test2 <- visit_anc_2 %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
test3 <- visit_anc_3 %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
test4 <- visit_anc_4 %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
test5 <- visit_anc_5 %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)

## Compile all visit type datasets into a list 
anc_visit_out <- mget(ls(pattern = "visit_anc_"))

gc()


# Merge all forms together 
anc_data_wide <- anc_visit_out %>%
  reduce(full_join, by =  c("SITE","SCRNID", "MOMID", "PREGID")) %>% distinct()

gc()

## remove weird data points 
anc_data_wide <- anc_data_wide %>% group_by(SITE, SCRNID) %>% mutate(n=n()) %>% filter(n==1) 

# table(anc_data_wide$M01_VISIT_COMPLETE_5, anc_data_wide$SITE)
#*****************************************************************************
#* IPC 
#*****************************************************************************
## Compile all IPC merged forms into list
ipc_out <- list(m05_merged, m06_merged, m07_merged, m08_merged, m09_merged, m10_merged, m17_merged)
names(ipc_out) <- c("m05", "m06", "m07","m08", "m09", "m10", "m17")

# Extract IPC visits in data 
ipc_data <- list()
for (i in names(ipc_out)) {
  ipc_data[[i]] <- ipc_out[[i]] %>% mutate(TYPE_VISIT = if_all(matches("(.+)_TYPE_VISIT"))) %>% 
    filter(TYPE_VISIT == 6)
}

## extract ipc variables 
for (i in names(ipc_out)) {
  # List of target variables
  target_vars <- c(
    as.character(MatNames_sheet$varname,"VISIT_DATE")
  )
  
  # List of target patterns
  target_patterns <- c(
    "TYPE_VISIT", "_VISIT_COMPLETE", "M04_ANC_OBSSTDAT_", "M12_VISIT_OBSSTDAT_",
    "GESTAGE_AT_VISIT_DAYS", "GESTAGE_AT_VISIT_WKS", "GESTAGE_AT_BIRTH_",
    "_PNC_AT_VISIT_", "_GA_LMP_DAYS_SCORRES_", "M04_FETAL_LOSS_DSSTDAT",
    "M09_DELIV_DSSTDAT_INF", "INFANTS_FAORRES_", "BIRTH_DSTERM"
  )
  
  # Selecting columns that exist in the current data frame
  ipc_data[[i]] <- ipc_data[[i]] %>% 
    select(intersect(target_vars, names(.)), 
           intersect(grep(paste(target_patterns, collapse = "|"), names(.), value = TRUE), names(.)))
  
}

## Organize Data: 
## Add binary IPC Yes/No column (1 = Yes, 0 = No)
## Generate variable for visit complete 
# If the visit status variable (MAT_VISIT_MNH##) reports options 1 (conducted in person) or 2 (conducted by phone), then the visit is considered complete 
## calculate days ipc 

ipc_data_out <- list()
for(i in names(ipc_data)){
  
  ipc_data_out[[i]] <- ipc_data[[i]] %>% 
    rowwise() %>%
    group_by(SITE, MOMID, PREGID) %>% 
    # add in binary variable for IPC (Yes == 1, No == 0) 
    mutate(!!paste0(toupper(i),quo_name("_IPC_YN")) := 1) %>% 
    # add in binary variable if visit is complete 
    mutate(!!paste0(toupper(i),quo_name("_VISIT_COMPLETE")) := ifelse(if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 1 | if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 2, 1, 0)) %>% 
    ## move type_visit to the front of each data set
    relocate(TYPE_VISIT, .after = PREGID)  
  
}


## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
# Merge all forms together 
ipc_data_wide_visit <- ipc_data_out %>% reduce(full_join, by =  c("SITE","MOMID", "PREGID", "TYPE_VISIT")) %>% distinct()

## For each participant, extract the minimum delivery time and minimum delivery date 
m09 <- m09_merged %>% select(SITE, MOMID, PREGID, M09_INFANTS_FAORRES, 
                             M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF2, 
                             M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF4,
                             M09_DELIV_DSSTTIM_INF1, M09_DELIV_DSSTTIM_INF2, 
                             M09_DELIV_DSSTTIM_INF3, M09_DELIV_DSSTTIM_INF4)  %>% 
  group_by(SITE, MOMID, PREGID) %>% 
  # remove duplicates 
  mutate(NON_DUPLICATE = n()) %>% 
  filter(NON_DUPLICATE == 1) %>% 
  # replace default value date with NA 
  mutate(M09_DELIV_DSSTDAT_INF1 = replace(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF1==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF2 = replace(M09_DELIV_DSSTDAT_INF2, M09_DELIV_DSSTDAT_INF2==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF3 = replace(M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF3==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF4 = replace(M09_DELIV_DSSTDAT_INF4, M09_DELIV_DSSTDAT_INF4==ymd("1907-07-07"), NA)) %>% 
  # replace default value time with NA 
  mutate(M09_DELIV_DSSTTIM_INF1 = replace(M09_DELIV_DSSTTIM_INF1, M09_DELIV_DSSTTIM_INF1=="77:77", NA),
         M09_DELIV_DSSTTIM_INF2 = replace(M09_DELIV_DSSTTIM_INF2, M09_DELIV_DSSTTIM_INF2=="77:77", NA),
         M09_DELIV_DSSTTIM_INF3 = replace(M09_DELIV_DSSTTIM_INF3, M09_DELIV_DSSTTIM_INF3=="77:77", NA),
         M09_DELIV_DSSTTIM_INF4 = replace(M09_DELIV_DSSTTIM_INF4, M09_DELIV_DSSTTIM_INF4=="77:77", NA)) %>% 
  # concatenate date and time of birth 
  mutate(DELIVERY_DATETIME_INF1 = paste(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTTIM_INF1),
         DELIVERY_DATETIME_INF2 = paste(M09_DELIV_DSSTDAT_INF2, M09_DELIV_DSSTTIM_INF2),
         DELIVERY_DATETIME_INF3 = paste(M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTTIM_INF3),
         DELIVERY_DATETIME_INF4 = paste(M09_DELIV_DSSTDAT_INF4, M09_DELIV_DSSTTIM_INF4)) %>% 
  # assign time field type for time of birth
  mutate(DELIVERY_DATETIME_INF1 = as.POSIXct(DELIVERY_DATETIME_INF1, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF2 = as.POSIXct(DELIVERY_DATETIME_INF2, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF3 = as.POSIXct(DELIVERY_DATETIME_INF3, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF4 = as.POSIXct(DELIVERY_DATETIME_INF4, format= "%Y-%m-%d %H:%M")) %>%
  # assign minimum dob and time of birth
  mutate(DELIVERY_DATETIME = 
           pmin(DELIVERY_DATETIME_INF1, DELIVERY_DATETIME_INF2, 
                DELIVERY_DATETIME_INF3, DELIVERY_DATETIME_INF4, na.rm = TRUE)) %>% 
  mutate(DOB = 
           pmin(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF2, 
                M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF4, na.rm = TRUE)) %>% 
  # merge in mnh01 estimated date of conception variable to calculate age at birth
  left_join(anc_data_wide[c("SITE", "MOMID", "PREGID", "PREG_START_DATE")], by = c("SITE", "MOMID", "PREGID")) %>%
  # calculate gestational age at birth
  mutate(GESTAGE_AT_BIRTH_DAYS = as.numeric(DOB-PREG_START_DATE),
         GESTAGE_AT_BIRTH_WKS = as.numeric(DOB-PREG_START_DATE) %/% 7) %>% 
  select(SITE, MOMID, PREGID, DOB, GESTAGE_AT_BIRTH_DAYS, GESTAGE_AT_BIRTH_WKS)

#####
ipc_data_wide_visit <- ipc_data_wide_visit %>% left_join(m09, by = c("SITE", "MOMID", "PREGID"))

## Final maternal wide dataset by visit type == ipc_data_wide_visit 

## IF WE WANT TO HAVE A WIDE DATASET WITH 1 ROW FOR EACH WOMAN, THEN WE NEED TO ADD A SUFFIX TO EACH OF THE VARIABLE NAMES 
## The code does the following:   
# 1. remove visit type variable (a form-specific visit type variable remains, ex. M09_TYPE_VISIT = 6)
# 2. Add suffix to the end of the varnames. Since IPC only happens at one time point, all suffixes are "6"
# ipc_data_out_wide <- list()
# for(i in names(ipc_data)){
#   ipc_data_out_wide[[i]] <- ipc_data_out[[i]] %>% 
#     select(-TYPE_VISIT) %>% 
#     rename_with(~paste0(., "_", 6), .cols = -c("SITE", "MOMID", "PREGID","DOB", "GESTAGE_AT_BIRTH_DAYS", "GESTAGE_AT_BIRTH_WKS")) 
# }

gc()
ipc_data_wide <- ipc_data_wide_visit %>% select(-TYPE_VISIT) %>% 
  rename_with(~paste0(., "_", 6), .cols = -c("SITE", "MOMID", "PREGID","DOB", "GESTAGE_AT_BIRTH_DAYS", "GESTAGE_AT_BIRTH_WKS")) 

## Final maternal wide dataset == anc_data_wide 
# ipc_data_wide <- ipc_data_out_wide %>% reduce(full_join, by =  c("SITE","MOMID", "PREGID"))

gc()
#*****************************************************************************

#*****************************************************************************
mat_enroll <- read_xlsx(paste0("Z:/Outcome Data/",UploadDate, "/MAT_ENROLL.xlsx")) %>% select(SITE,SCRNID, MOMID, PREGID, ENROLL)

## Compile all PNC merged forms into list
pnc_out <- list(m05_merged, m06_merged, m07_merged, m08_merged, m12_merged, m25_merged, m26_merged)
names(pnc_out) <- c("m05", "m06", "m07", "m08", "m12", "m25", "m26")

rm(list=ls()[! ls() %in% c("mat_enroll", "MatNames_sheet", "anc_data_wide","ipc_data_wide", "m23_merged", "UploadDate", "pnc_out", "m09")])

gc()

## Merge m09 (subset of delivery form with gestage_at_birth variables) forms with each of the other forms 
pnc_data <- list()
for (i in names(pnc_out)) { 
  pnc_data[[i]] <-left_join(pnc_out[[i]], m09, by = c("SITE", "MOMID", "PREGID")) %>% distinct()
}

# Extract PNC visits in data 
pnc_visits <- c(7,8,9,10,11,12)
for (i in names(pnc_out)) {
  pnc_data[[i]] <- pnc_data[[i]] %>% mutate(TYPE_VISIT = if_all(matches("(.+)_TYPE_VISIT"))) %>% 
    filter(TYPE_VISIT %in% pnc_visits) 
}


## extract pnc variables 
for (i in names(pnc_out)) {
  # List of target variables
  target_vars <- c(
    as.character(MatNames_sheet$varname),"VISIT_DATE"
  )
  
  # List of target patterns
  target_patterns <- c(
    "TYPE_VISIT", "_VISIT_COMPLETE", "M04_ANC_OBSSTDAT_", "M12_VISIT_OBSSTDAT_",
    "GESTAGE_AT_VISIT_DAYS", "GESTAGE_AT_VISIT_WKS", "GESTAGE_AT_BIRTH_",
    "_PNC_AT_VISIT_", "_GA_LMP_DAYS_SCORRES_", "M04_FETAL_LOSS_DSSTDAT", "RBC_G6PD_LBORRES",
    "RBC_THALA_LBORRES", "M12_MAT_VISIT_MNH12"
  )
  
  # Selecting columns that exist in the current data frame
  pnc_data[[i]] <- pnc_data[[i]] %>% 
    select(intersect(target_vars, names(.)), 
           intersect(grep(paste(target_patterns, collapse = "|"), names(.), value = TRUE), names(.)))
  
}


## Organize Data: 
## Remove M09 (M09 data included to calculate days PNC, but will be removed as to not be duplicated from IPC data)
## Add binary PNC Yes/No column (1 = Yes, 0 = No)
## Calculate days PNC at visit for each form
## Generate variable for visit complete 
# If the visit status variable (MAT_VISIT_MNH##) reports options 1 (conducted in person) or 2 (conducted by phone), then the visit is considered complete 

pnc_data_out <- list()
for(i in names(pnc_data)){
  ## remove m01 variables now that we have GA at ultrasound 
  m09_to_remove <- grep("M09_", names(pnc_data[[i]]))
  
  pnc_data_out[[i]] <- pnc_data[[i]] %>% 
    rowwise() %>%
    # calculate DAYS since delivery (MIGHT NEED TO CHANGE TO HOURS)
    mutate(!!paste0(toupper(i),quo_name("_PNC_AT_VISIT_DAYS")) := as.numeric(VISIT_DATE - DOB)) %>% 
    # calculate WEEKS since delivery 
    mutate(!!paste0(toupper(i),quo_name("_PNC_AT_VISIT_WKS")) := as.numeric(VISIT_DATE - DOB) %/% 7) %>% 
    # add in binary variable for pnc (Yes == 1, No == 0) 
    mutate(!!paste0(toupper(i),quo_name("_PNC_YN")) := 1) %>% 
    # add in binary variable if visit is complete 
    mutate(!!paste0(toupper(i),quo_name("_VISIT_COMPLETE")) := ifelse(if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 1 | if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 2, 1, 0)) %>% 
    ## add prefix in to all new variables 
    rename(!!paste0(toupper(i),quo_name("_VISIT_DATE")) := "VISIT_DATE")  %>% 
    ## remove m09 variables
    select(-any_of(m09_to_remove), -GESTAGE_AT_BIRTH_DAYS, -GESTAGE_AT_BIRTH_WKS, -DOB) %>% 
    ## move type_visit to the front of each dataset
    relocate(TYPE_VISIT, .after = PREGID) %>% 
    filter(!TYPE_VISIT %in% c(13,14))
}


## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
pnc_data_wide_visit <- pnc_data_out %>% reduce(full_join,
                                               by =  c("SITE","MOMID", "PREGID", "TYPE_VISIT"))

## Final maternal wide dataset by visit type == pnc_data_wide_visit 

## IN ORDER TO MAKE THE DATA WIDE WITH ONE ROW FOR EACH WOMAN, WE NEED TO ADD A PREFIX TO ALL FORMS THAT ARE FILLED OUT AT MULTIPLE VISITS 
## The following code will do the following
# 1. Extract the all visit types to their own dataset 
# 2. Add a suffix to the end of each variable to represent the visit type 
## Example: data from visit type = 1 would have variables named "HEIGHT_PERES_1" 

## Extract Visit 7 (PNC 0)
visit_pnc_7 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 7) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M07_TYPE_VISIT = 7) %>% 
  rename_with(~paste0(., "_", 7), .cols = -c("SITE", "MOMID", "PREGID"))  %>%
  group_by(SITE, MOMID, PREGID) %>% 
  slice(1)


## Extract Visit 8 (PNC 1)
visit_pnc_8 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 8) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 8) %>% 
  rename_with(~paste0(., "_", 8), .cols = -c("SITE", "MOMID", "PREGID"))  %>%
  group_by(SITE, MOMID, PREGID) %>% 
  slice(1)


## Extract Visit 9 (PNC 4)
visit_pnc_9 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 9) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 9) %>% 
  rename_with(~paste0(., "_", 9), .cols = -c("SITE", "MOMID", "PREGID"))  %>%
  group_by(SITE, MOMID, PREGID) %>% 
  slice(1)


## Extract Visit 10 (PNC 6)
visit_pnc_10 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 10) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 10) %>% 
  rename_with(~paste0(., "_", 10), .cols = -c("SITE", "MOMID", "PREGID")) %>%
  group_by(SITE, MOMID, PREGID) %>% 
  slice(1)


## Extract Visit 11 (PNC 26)
visit_pnc_11 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 11) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 11) %>% 
  rename_with(~paste0(., "_", 11), .cols = -c("SITE", "MOMID", "PREGID")) 

## Extract Visit 12 (PNC 52)
visit_pnc_12 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 12) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 12) %>% 
  rename_with(~paste0(., "_", 12), .cols = -c("SITE", "MOMID", "PREGID")) %>% 
  group_by(SITE, MOMID, PREGID) %>% 
  slice(1)

# check for duplicates:
test7 <- visit_pnc_7 %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
test8 <- visit_pnc_8 %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
test9 <- visit_pnc_9 %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
test10 <- visit_pnc_10 %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
test11 <- visit_pnc_11 %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
test12 <- visit_pnc_12 %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)


## Compile all visit type datasets into a list 
pnc_visit_out <- mget(ls(pattern = "visit_pnc_"))

## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
# Merge all forms together 
pnc_data_wide <- pnc_visit_out %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID")) %>% distinct()
## Final maternal wide dataset == pnc_data_wide 


gc()

#*****************************************************************************
#* Merge all ANC, IPC, PNC data to get wide dataset 
#* #* One row for each mom 
#*****************************************************************************
rm(list=ls()[! ls() %in% c("anc_data_wide","ipc_data_wide", "pnc_data_wide", "m23_merged", "UploadDate")])

# ### MERGE ALL TOGETHER - BY MOMID, PREGID, SCRNID  
# 
gc()

## only pull the variables we need for monitoring report 
MatNames_sheet <- read_excel("~/Monitoring Report/code/varNames_sheet.xlsx", sheet = "MaternalVars")

anc_data_wide <- anc_data_wide %>% 
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
         M00_KNOWN_DOBYN_SCORRES, 
         contains("M01_FETUS_CT_PERES_US"),
         M02_SCRN_RETURN, contains("RBC_G6PD_LBORRES"), contains("RBC_THALA_LBORRES"))

ipc_data_wide <- ipc_data_wide %>% 
  select(matches(MatNames_sheet$varname), 
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
         contains("VISIT_DATE")) 

pnc_data_wide <- pnc_data_wide %>% 
  select(matches(MatNames_sheet$varname), 
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
         contains("VISIT_DATE"), contains("RBC_G6PD_LBORRES"), contains("RBC_THALA_LBORRES")) 

gc()

## test for duplicates 
test_anc <- anc_data_wide %>% group_by(SITE,SCRNID, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
test_ipc <- ipc_data_wide %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)
test_pnc <- pnc_data_wide %>% group_by(SITE, MOMID, PREGID) %>% mutate(n=n()) %>% filter(n>1)

rm(test_anc)
rm(test_ipc)
rm(test_pnc)

## merge data together
out <- list(anc_data_wide, ipc_data_wide, pnc_data_wide)
MatData_Wide <- out %>% reduce(full_join, by = c("SITE", "MOMID", "PREGID"))  %>%
  relocate(DOB, .after = PREGID) %>% distinct()

gc()


## Merge in maternal closeout form (MNH23) -- only filled out once which is why we merge it at the end  
MatData_Wide <- full_join(MatData_Wide, m23_merged, by =c("SITE", "MOMID", "PREGID")) %>% 
  relocate(any_of(c("SCRNID", "MOMID", "PREGID")), .after = SITE) %>% 
  distinct()

gc()

MatData_Wide <- MatData_Wide %>% 
  # select(SITE, MOMID, PREGID, 
  #        SCRNID,M00_SCRN_OBSSTDAT,M01_VISIT_COMPLETE_4,  M01_VISIT_COMPLETE_5) %>% 
  mutate(MOMID = ifelse(str_detect(MOMID, "n/a"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "n/a"), NA, PREGID)) %>% 
  # mutate(MOMID = ifelse(str_detect(MOMID, "N/A"), NA, MOMID),
  #        PREGID = ifelse(str_detect(PREGID, "N/A"), NA, PREGID)) %>% 
  group_by(SCRNID) %>% 
  mutate(n=n()) %>% 
  mutate(KEEP = case_when((!is.na(MOMID) & !is.na(PREGID)) | n==1 ~ 1, TRUE ~0)) %>% 
  filter(KEEP ==1) %>% 
  ungroup() 


gc()

MatData_Wide <- MatData_Wide %>% 
  ## Only keep instances where screening ID is present (remove NA screening IDs) 
  filter(!is.na(SCRNID))

#*****************************************************************************
#* Export wide dataset 
#*****************************************************************************
gc()
# export to personal 
setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
save(MatData_Wide, file= paste("MatData_Wide","_", UploadDate,".RData",sep = ""))

# export to shared  
# first need to make subfolder with upload date
maindir <- paste0("Z:/Processed Data", sep = "")
subdir = UploadDate
dir.create(file.path(maindir, subdir), showWarnings = FALSE)

setwd(paste("Z:/Processed Data/",UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
save(MatData_Wide, file= paste("MatData_Wide","_", UploadDate,".RData",sep = ""))
