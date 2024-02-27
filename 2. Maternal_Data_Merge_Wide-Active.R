#*****************************************************************************
#* MATERNAL WIDE DATASET BY VISIT 
#*Function: Merge all forms together in wide format to create a dataset with one row for each woman for each visit 
#*Input: .RData files for each form (generated from 1. data import code)
#* Last updated: 24 Feb 2024

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

UploadDate = "2024-02-23"

#*****************************************************************************
#* Import merged data 
#*****************************************************************************
setwd(paste0("~/Monitoring Report/data/merged/", UploadDate, sep = ""))

## import all rda files 
rdata_files = list.files(pattern="*.RData")
walk(rdata_files, ~ load(.x, .GlobalEnv))

#*****************************************************************************
#* Assign expected visit type and rename "date" varnames 
#*****************************************************************************
#* Remove MNH08 variables from MNH07 (Zambia has additional variables)
m07_merged <- m07_merged %>% select(-contains("MNH08"))

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
if (exists("m25_merged") == TRUE){ m25_merged <- m25_merged %>% mutate(VISIT_DATE = dmy(VISIT_DATE)) }
if (exists("m26_merged") == TRUE){ m26_merged <- m26_merged %>% mutate(VISIT_DATE = M26_FTGE_OBSTDAT) }
if (exists("m26_merged") == TRUE){ m26_merged <- m26_merged %>% mutate(VISIT_DATE = dmy(VISIT_DATE)) }

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

## Remove scrnid from MNH26 -- causes issues in merge 
# if (exists("m26_merged") == TRUE){ m26_merged <- m26_merged %>% select(-M26_SCRNID) }


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
  mutate(GA_US_DAYS_FTS1 =  ifelse(SITE!="India-CMC" & M01_US_GA_WKS_AGE_FTS1!= -7 & M01_US_GA_DAYS_AGE_FTS1 != -7,  (M01_US_GA_WKS_AGE_FTS1 * 7 + M01_US_GA_DAYS_AGE_FTS1), NA), 
         GA_US_DAYS_FTS2 =  ifelse(SITE!="India-CMC" & M01_US_GA_WKS_AGE_FTS2!= -7 & M01_US_GA_DAYS_AGE_FTS2 != -7,  (M01_US_GA_WKS_AGE_FTS2 * 7 + M01_US_GA_DAYS_AGE_FTS2), NA),
         GA_US_DAYS_FTS3 =  ifelse(SITE!="India-CMC" & M01_US_GA_WKS_AGE_FTS3!= -7 & M01_US_GA_DAYS_AGE_FTS3 != -7,  (M01_US_GA_WKS_AGE_FTS3 * 7 + M01_US_GA_DAYS_AGE_FTS3), NA),
         GA_US_DAYS_FTS4 =  ifelse(SITE!="India-CMC" & M01_US_GA_WKS_AGE_FTS4!= -7 & M01_US_GA_DAYS_AGE_FTS4 != -7,  (M01_US_GA_WKS_AGE_FTS4 * 7 + M01_US_GA_DAYS_AGE_FTS4), NA)) %>% 
  ## combine ga weeks and days variables to get a single gestational age variable - CMC is using acog - use this here 
  mutate(GA_US_DAYS_FTS1 =  ifelse(SITE=="India-CMC" & M01_CAL_GA_WKS_AGE_FTS1!= -7 & M01_CAL_GA_DAYS_AGE_FTS1 != -7,  (M01_CAL_GA_WKS_AGE_FTS1 * 7 + M01_CAL_GA_DAYS_AGE_FTS1), GA_US_DAYS_FTS1), 
         GA_US_DAYS_FTS2 =  ifelse(SITE=="India-CMC" & M01_CAL_GA_WKS_AGE_FTS2!= -7 & M01_CAL_GA_DAYS_AGE_FTS2 != -7,  (M01_CAL_GA_WKS_AGE_FTS2 * 7 + M01_CAL_GA_DAYS_AGE_FTS2), GA_US_DAYS_FTS2),
         GA_US_DAYS_FTS3 =  ifelse(SITE=="India-CMC" & M01_CAL_GA_WKS_AGE_FTS3!= -7 & M01_CAL_GA_DAYS_AGE_FTS3 != -7,  (M01_CAL_GA_WKS_AGE_FTS3 * 7 + M01_CAL_GA_DAYS_AGE_FTS3), GA_US_DAYS_FTS3),
         GA_US_DAYS_FTS4 =  ifelse(SITE=="India-CMC" & M01_CAL_GA_WKS_AGE_FTS4!= -7 & M01_CAL_GA_DAYS_AGE_FTS4 != -7,  (M01_CAL_GA_WKS_AGE_FTS4 * 7 + M01_CAL_GA_DAYS_AGE_FTS4), GA_US_DAYS_FTS4)) %>% 
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
  mutate(EST_CONCEP_DATE = M01_US_OHOSTDAT - US_GA_DAYS_ENROLL) %>% 
  # add 280 days to EST_CONCEP_DATE to generate EDD based on BOE 
  mutate(EDD_BOE = EST_CONCEP_DATE + 280) %>% 
  ## EDD based on ultrasound 
  mutate(EDD_US =  EST_CONCEP_DATE + 280)

## Only select ID variables from MNH02 to merge into MNH01 
m02_wide <- m02_merged %>% select(SITE, SCRNID, MOMID, PREGID, M02_SCRN_OBSSTDAT) 

## Merge enrollment form with US form to get GA at enrollment -- SUBSET OF DATA 
enroll_bind <- full_join(m02_wide, m01_enroll, by = c("SITE", "SCRNID")) %>% distinct()
enroll_bind <- enroll_bind %>% relocate(c(MOMID,PREGID), .after = SCRNID) %>% 
  select(SITE,SCRNID, MOMID, PREGID, EST_CONCEP_DATE, US_GA_DAYS_ENROLL, 
         US_GA_WKS_ENROLL, EDD_US,EDD_BOE, BOE_GA_DAYS_ENROLL, M02_SCRN_OBSSTDAT) %>% distinct()

## merge momid and pregid into mnh01 to merge in later 
m02_ids <- m02_merged %>% select(SITE, SCRNID, MOMID, PREGID) ## export mnh02 ids
m01_merged_enroll <- m01_merged %>% filter(M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>%
  left_join(m02_ids, by = c("SITE", "SCRNID"))
mnh01_all_visits <- m01_merged %>% filter(M01_TYPE_VISIT != 1)
m01_merged <- bind_rows(m01_merged_enroll, mnh01_all_visits) # rebind data

## generate sub datasets for MNH00, MNH01, MNH02, and MNH03 to bind in later 
# (since these are filled out at only one visit, we want to merge them in separately )
m01_to_bind <- m01_merged %>% 
  # remove unscheduled visits 
  filter(M01_TYPE_VISIT!=13, M01_TYPE_VISIT!=14) %>% 
  # merge in enrollment data including BOE and EST_CONCEP_DATE
  left_join(enroll_bind[c("SITE", "SCRNID", "MOMID", "PREGID", "EST_CONCEP_DATE",
                          "BOE_GA_DAYS_ENROLL", "EDD_BOE")], by = c("SITE", "SCRNID", "MOMID", "PREGID")) %>% 
  # generate gestational age at each visit 
  mutate(M01_GESTAGE_AT_VISIT_DAYS = as.numeric(VISIT_DATE - EST_CONCEP_DATE, na.rm= TRUE)) %>% 
  # add in variable for GA at visit in WEEKS 
  mutate(M01_GESTAGE_AT_VISIT_WKS = as.numeric(VISIT_DATE - EST_CONCEP_DATE, na.rm= TRUE) %/% 7) %>% 
  # generate visit complete variable 
  mutate(M01_VISIT_COMPLETE = ifelse(M01_MAT_VISIT_MNH01 == 1 | M01_MAT_VISIT_MNH01 == 2, 1, 0)) %>%
  # rename visit date 
  rename("M01_VISIT_DATE" = VISIT_DATE,
         "TYPE_VISIT" = M01_TYPE_VISIT) %>% 
  select(-EST_CONCEP_DATE, -BOE_GA_DAYS_ENROLL, -EDD_BOE)

m00_to_bind <- m00_merged %>% 
  mutate(TYPE_VISIT = 1)

m02_to_bind <- m02_merged %>% 
  mutate(TYPE_VISIT = 1) %>% 
  # remove visit date as we will have already included it in the enrollment data subset above
  # leaving it in here will cause duplicates
  select(-M02_SCRN_OBSSTDAT)


m03_to_bind <- m03_merged %>% 
  # merge in enrollment data including BOE and EST_CONCEP_DATE
  left_join(enroll_bind[c("SITE", "MOMID", "PREGID", "EST_CONCEP_DATE")], by = c("SITE", "MOMID", "PREGID")) %>% 
  # generate gestational age at each visit 
  mutate(M03_GESTAGE_AT_VISIT_DAYS = as.numeric(VISIT_DATE - EST_CONCEP_DATE, na.rm= TRUE)) %>% 
  # add in variable for GA at visit in WEEKS 
  mutate(M03_GESTAGE_AT_VISIT_WKS = as.numeric(VISIT_DATE - EST_CONCEP_DATE, na.rm= TRUE) %/% 7) %>% 
  # generate visit complete variable 
  mutate(M03_VISIT_COMPLETE = ifelse(M03_MAT_VISIT_MNH03 == 1 | M03_MAT_VISIT_MNH03 == 2, 1, 0)) %>%
  # rename visit date 
  rename("M03_VISIT_DATE" = VISIT_DATE) %>% 
  # add visit type 
  mutate(TYPE_VISIT = 1) %>% 
  select(-EST_CONCEP_DATE)


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
    mutate(!!paste0(toupper(i),quo_name("_GESTAGE_AT_VISIT_DAYS")) := as.numeric(VISIT_DATE - EST_CONCEP_DATE, na.rm= TRUE)) %>% 
    # add in variable for GA at visit in WEEKS 
    mutate(!!paste0(toupper(i),quo_name("_GESTAGE_AT_VISIT_WKS")) := as.numeric(VISIT_DATE - EST_CONCEP_DATE, na.rm= TRUE) %/% 7) %>% 
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
                                                             "US_GA_DAYS_ENROLL", "US_GA_WKS_ENROLL", "EST_CONCEP_DATE",
                                                             "M02_SCRN_OBSSTDAT","BOE_GA_DAYS_ENROLL","EDD_BOE", "EDD_US")) %>% distinct()

# Merge MNH00,MNH01, MNH02, and MNH03 back into the data using the full merged dataset created above (dataframe name = enroll_bind_all)
anc_data_wide <- anc_data_wide %>% 
  full_join(m01_to_bind, by = c("SITE", "SCRNID", "MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(m00_to_bind, by = c("SITE","SCRNID", "TYPE_VISIT")) %>%
  full_join(m02_to_bind, by = c("SITE","SCRNID","MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(m03_to_bind, by = c("SITE","MOMID", "PREGID", "TYPE_VISIT")) %>%
  distinct()

## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
anc_data_wide_visit = anc_data_wide

## Move MNH00 data to the front the dataframe 
m00_to_move <- grep("M00_", names(anc_data_wide_visit))
anc_data_wide_visit<- anc_data_wide_visit %>% 
  #filter(SCRNID != "n/a") %>% 
  relocate(TYPE_VISIT, .after = PREGID) %>% 
  relocate(any_of(m00_to_move), .after = TYPE_VISIT)

## Final maternal wide dataset by visit type == anc_data_wide_visit 


## IN ORDER TO MAKE THE DATA WIDE WITH ONE ROW FOR EACH WOMAN, WE NEED TO ADD A PREFIX TO ALL FORMS THAT ARE FILLED OUT AT MULTIPLE VISITS 
## The following code will do the following
# 1. Extract the all visit types to their own dataset 
# 2. Add a suffix to the end of each variable to represent the visit type 
## Example: data from visit type = 1 would have variables named "HEIGHT_PERES_1" 


## Extract Visit 1 (ANC < 20)
visit_anc_1 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 1) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 1,
         M02_VISIT_COMPLETE = case_when(
           !is.na(M02_SCRN_OBSSTDAT) ~ 1,
           is.na(M02_SCRN_OBSSTDAT) ~ 0)) %>% 
  rename_with(~paste0(., "_", 1), 
              .cols = -c("SITE", "SCRNID", "MOMID", "PREGID", contains("M02"), contains("M00"), 
                         "EST_CONCEP_DATE", "US_GA_DAYS_ENROLL", 
                         "US_GA_WKS_ENROLL", "EDD_US", "EDD_BOE", "BOE_GA_DAYS_ENROLL")) 

## NOTE: Forms that only get filled out at enrollment (visit 1) (MNH00, MNH02, MNH03) will need to be removed from the other visit data as to not have duplicates
m00_to_remove <- grep("M00_", names(anc_data_wide_visit))
m02_to_remove <- grep("M02_", names(anc_data_wide_visit))
m03_to_remove <- grep("M03_", names(anc_data_wide_visit))
baseline_to_remove <- c("EST_CONCEP_DATE", "US_GA_DAYS_ENROLL", "US_GA_WKS_ENROLL",
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
  rename_with(~paste0(., "_", 2), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) 

## Extract Visit 3 (ANC 28)
visit_anc_3 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 3) %>% 
  select(-any_of(m00_to_remove),
         -any_of(m02_to_remove),
         -any_of(m03_to_remove), 
         -any_of(baseline_to_remove), 
         -TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 3) %>% # US variable "TYPE_VISIT" exists, but does not have the "M01_" prefix -- add here 
  rename_with(~paste0(., "_", 3), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) 

## Extract Visit 4 (ANC 32)
visit_anc_4 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 4) %>% 
  select(-any_of(m00_to_remove),
         -any_of(m02_to_remove),
         -any_of(m03_to_remove), 
         -any_of(baseline_to_remove),
         -TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 4) %>% # US variable "TYPE_VISIT" exists, but does not have the "M01_" prefix -- add here 
  rename_with(~paste0(., "_", 4), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) 

## Extract Visit 4 (ANC 36)
visit_anc_5 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 5) %>% 
  select(-any_of(m00_to_remove),
         -any_of(m02_to_remove),
         -any_of(m03_to_remove),
         -any_of(baseline_to_remove), 
         -TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 5) %>% # US variable "TYPE_VISIT" exists, but does not have the "M01_" prefix -- add here 
  rename_with(~paste0(., "_", 5), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) 

## Compile all visit type datasets into a list 
anc_visit_out <- mget(ls(pattern = "visit_anc_"))

# Merge all forms together 
anc_data_wide <- anc_visit_out %>% reduce(full_join, by =  c("SITE","SCRNID", "MOMID", "PREGID")) %>% distinct()

gc()
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
  left_join(anc_data_wide[c("SITE", "MOMID", "PREGID", "EST_CONCEP_DATE")], by = c("SITE", "MOMID", "PREGID")) %>%
  # calculate gestational age at birth
  mutate(GESTAGE_AT_BIRTH_DAYS = as.numeric(DOB-EST_CONCEP_DATE),
         GESTAGE_AT_BIRTH_WKS = as.numeric(DOB-EST_CONCEP_DATE) %/% 7) %>% 
  select(SITE, MOMID, PREGID, DOB, GESTAGE_AT_BIRTH_DAYS, GESTAGE_AT_BIRTH_WKS)

ipc_data_wide_visit <- ipc_data_wide_visit %>% left_join(m09, by = c("SITE", "MOMID", "PREGID"))

# pre long m09 -- 1059
# make long m09 -- 1059
# join with ipc data wide -- 1097
# merge with all -- 1059
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

ipc_data_wide <- ipc_data_wide_visit %>% select(-TYPE_VISIT) %>% 
  rename_with(~paste0(., "_", 6), .cols = -c("SITE", "MOMID", "PREGID","DOB", "GESTAGE_AT_BIRTH_DAYS", "GESTAGE_AT_BIRTH_WKS")) 

## Final maternal wide dataset == anc_data_wide 
# ipc_data_wide <- ipc_data_out_wide %>% reduce(full_join, by =  c("SITE","MOMID", "PREGID"))

# test <- ipc_data_wide %>%
#   relocate(any_of(c("MOMID", "PREGID")), .after = SITE)
# out_IDS <- test[duplicated(test[,1:3]),]
# dim(out_IDS)

gc()
#*****************************************************************************
#* PNC 
#*****************************************************************************
## Compile all PNC merged forms into list
pnc_out <- list(m05_merged, m06_merged, m07_merged, m08_merged, m12_merged, m25_merged, m26_merged)
names(pnc_out) <- c("m05", "m06", "m07", "m08", "m12", "m25", "m26")

## Merge m09 (subset of delivery form with gestage_at_birth variables) forms with each of the other forms 
pnc_data <- list()
for (i in names(pnc_out)) { 
  pnc_data[[i]] <- left_join(pnc_out[[i]], m09, by = c("SITE", "MOMID", "PREGID")) %>% distinct()
}

# Extract PNC visits in data 
pnc_visits <- c(7,8,9,10,11,12)
for (i in names(pnc_out)) {
  pnc_data[[i]] <- pnc_data[[i]] %>% mutate(TYPE_VISIT = if_all(matches("(.+)_TYPE_VISIT"))) %>% 
    filter(TYPE_VISIT %in% pnc_visits)
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
    filter(TYPE_VISIT != 13, TYPE_VISIT!=14)
}


## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
pnc_data_wide_visit <- pnc_data_out %>% reduce(full_join, by =  c("SITE","MOMID", "PREGID", "TYPE_VISIT"))

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
  rename_with(~paste0(., "_", 7), .cols = -c("SITE", "MOMID", "PREGID")) 

## Extract Visit 8 (PNC 1)
visit_pnc_8 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 8) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 8) %>% 
  rename_with(~paste0(., "_", 8), .cols = -c("SITE", "MOMID", "PREGID")) 

## Extract Visit 9 (PNC 4)
visit_pnc_9 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 9) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 9) %>% 
  rename_with(~paste0(., "_", 9), .cols = -c("SITE", "MOMID", "PREGID")) 

## Extract Visit 10 (PNC 6)
visit_pnc_10 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 10) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 10) %>% 
  rename_with(~paste0(., "_", 10), .cols = -c("SITE", "MOMID", "PREGID")) 

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
  rename_with(~paste0(., "_", 12), .cols = -c("SITE", "MOMID", "PREGID")) 

## Compile all visit type datasets into a list 
pnc_visit_out <- mget(ls(pattern = "visit_pnc_"))

## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
# Merge all forms together 
pnc_data_wide <- pnc_visit_out %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID")) %>% distinct()
## Final maternal wide dataset == pnc_data_wide 

# check for duplicates:
# test <- pnc_data_wide %>%
#   relocate(any_of(c("MOMID", "PREGID")), .after = SITE)
# out_IDS <- test[duplicated(test[,1:3]),]
# dim(out_IDS)

gc()
#*****************************************************************************
#* Merge all ANC, IPC, PNC data to get wide dataset BY VISIT 
#* One row for each mom at each visit 
#*****************************************************************************

# ### MERGE ALL TOGETHER - BY VISIT 
# out <- list(anc_data_wide_visit, ipc_data_wide_visit, pnc_data_wide_visit)
# MatData_Wide_Visit <- out %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID", "TYPE_VISIT"))  
# 
# ## Merge in maternal closeout form (MNH23) -- only filled out once which is why we merge it at the end  
# MatData_Wide_Visit <- left_join(MatData_Wide_Visit, m23_merged, by =c("SITE", "MOMID", "PREGID"))
# 
# # export
# setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
# #dt=format(Sys.time(), "%Y-%m-%d")
# save(MatData_Wide_Visit, file= paste("MatData_Wide_Visit","_", UploadDate,".RData",sep = ""))
#*****************************************************************************
#* Merge all ANC, IPC, PNC data to get wide dataset 
#* #* One row for each mom 
#*****************************************************************************
rm(list=ls()[! ls() %in% c("anc_data_wide","ipc_data_wide", "pnc_data_wide", "m23_merged", "UploadDate")])

# ### MERGE ALL TOGETHER - BY MOMID, PREGID, SCRNID  
# anc_data_wide <- anc_data_wide %>% filter(SITE != "India-SAS")
# ipc_data_wide <- ipc_data_wide %>% filter(SITE != "India-SAS")
# pnc_data_wide <- pnc_data_wide %>% filter(SITE != "India-SAS")
# 
gc()

## only pull the variables we need for monitoring report 
MatNames_sheet <- read_excel("~/Monitoring Report/code/varNames_sheet.xlsx", sheet = "MaternalVars")

anc_data_wide <- anc_data_wide %>% 
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
         M00_KNOWN_DOBYN_SCORRES) 

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
         contains("VISIT_DATE")) 

gc()

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

# check for duplicates:
# test <- MatData_Wide %>%
#   relocate(any_of(c("SCRNID", "MOMID", "PREGID")), .after = SITE)
# out_IDS <- test[duplicated(test[,1:4]),]
# dim(out_IDS)

## remove duplicates
out_IDS <- MatData_Wide[duplicated(MatData_Wide[,1:4]),]
out_duplicated_IDS_SCRNID <- out_IDS %>% distinct(SITE, SCRNID)
MatData_Wide <- MatData_Wide %>% filter(!(SCRNID %in% out_duplicated_IDS_SCRNID))
#*****************************************************************************
#* Merge all forms that do not have visit type 
#* Steps: 
# 1. Generate indicator variable assigning a number for each instance a form is filled out for participant 
# 2. Generate variable denoting the total number of visits for each form for each woman 
# 4. Add suffix to each variable indicating what visit number it is (based on frequency and not visit type)
# 5. Merge all non-visit type forms together 
# 6. Merge into wide dataset 
#*****************************************************************************
if (exists("m19_merged") == TRUE){ 
  non_sched_m19 <- m19_merged %>% 
    mutate(VISIT_1 = 1) %>% 
    group_by(SITE, MOMID, PREGID) %>%
    arrange(-desc(VISIT_DATE)) %>% 
    mutate(VISIT_N = cumsum(VISIT_1)) %>% 
    ## add prefix in to all new variables 
    rename(!!paste0("M19",quo_name("_VISIT_DATE")) := "VISIT_DATE")  #%>% 
  ## remove m09 variables
  #select(VISIT_1) 
  
  # print list of visit frequencies to see the max number of visits an individual has
  print(table(non_sched_m19$VISIT_N))
  
  # add suffix to each variable name by visit# 
  non_sched_m19_visit1 = non_sched_m19 %>% 
    filter(VISIT_N == 1) %>% 
    rename_with(~paste0(., "_VISIT1"), .cols = -c("SITE", "MOMID", "PREGID"))
  
  non_sched_m19_visit2 = non_sched_m19 %>% 
    filter(VISIT_N == 2) %>% 
    rename_with(~paste0(., "_VISIT2"), .cols = -c("SITE", "MOMID", "PREGID"))
  
  non_sched_m19_visit3 = non_sched_m19 %>% 
    filter(VISIT_N == 3) %>% 
    rename_with(~paste0(., "_VISIT3"), .cols = -c("SITE", "MOMID", "PREGID"))
  
  ## Compile all visit type datasets into a list 
  non_sched_form_out_m19 <- mget(ls(pattern = "non_sched_m19_"))
  
  # Merge all forms together -- this should have the same number of rows as the number visit =1 observations
  non_sched_wide_m19 <- non_sched_form_out_m19 %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID")) %>% distinct()
  
  # add variable indicating the total number of hospitalizations an individual has
  non_sched_wide_m19 <- non_sched_wide_m19 %>% 
    mutate(M19_VISITS_TOT = sum(across(matches("VISIT_1")), na.rm = TRUE)) %>% 
    select(-contains("VISIT_1_"), -contains("VISIT_N_"))
  
}

if (exists("m21_merged") == TRUE){ 
  non_sched_m21 <- m21_merged %>% 
    ## since this form can be filled out for maternal or infant adverse events --  filter for maternal events 
    filter(M21_AETERM %in% c(1, 4, 88)) %>% 
    select(-INFANTID) %>% 
    mutate(VISIT_1 = 1) %>% 
    group_by(SITE, MOMID, PREGID) %>%
    arrange(-desc(VISIT_DATE)) %>% 
    mutate(VISIT_N = cumsum(VISIT_1)) %>% 
    ## add prefix in to all new variables 
    rename(!!paste0("M21",quo_name("_VISIT_DATE")) := "VISIT_DATE")  
  
  # print list of visit frequencies to see the max number of visits an individual has
  print(table(non_sched_m21$VISIT_N))
  
  # add suffix to each variable name by visit# 
  non_sched_m21_visit1 = non_sched_m21 %>% 
    filter(VISIT_N == 1) %>% 
    rename_with(~paste0(., "_VISIT1"), .cols = -c("SITE", "MOMID", "PREGID"))
  
  non_sched_m21_visit2 = non_sched_m21 %>% 
    filter(VISIT_N == 2) %>% 
    rename_with(~paste0(., "_VISIT2"), .cols = -c("SITE", "MOMID", "PREGID"))
  
  # non_sched_m21_visit3 = non_sched_m21 %>% 
  #   filter(VISIT_N == 3) %>% 
  #   rename_with(~paste0(., "_VISIT3"), .cols = -c("SITE", "MOMID", "PREGID"))
  
  ## Compile all visit type datasets into a list 
  non_sched_form_out_m21 <- mget(ls(pattern = "non_sched_m21_"))
  
  # Merge all forms together -- this should have the same number of rows as the number visit =1 observations
  non_sched_wide_m21 <- non_sched_form_out_m21 %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID")) %>% distinct()
  
  # add variable indicating the total number of hospitalizations an individual has
  non_sched_wide_m21 <- non_sched_wide_m21 %>% 
    mutate(M21_VISITS_TOT = sum(across(matches("VISIT_1")), na.rm = TRUE)) %>% 
    select(-contains("VISIT_1_"), -contains("VISIT_N_"))
}

if (exists("m22_merged") == TRUE){ 
  ## since mnh22 also can be for infants -- we include infantid here but still merge on momid/pregid
  non_sched_m22 <- m22_merged %>% 
    ## since this form can be filled out for maternal or infant adverse events -- only filter for maternal events 
    mutate(VISIT_1 = 1) %>% 
    group_by(SITE, MOMID, PREGID, INFANTID) %>%
    arrange(-desc(VISIT_DATE)) %>% 
    mutate(VISIT_N = cumsum(VISIT_1)) %>% 
    ## add prefix in to all new variables 
    rename(!!paste0("M22",quo_name("_VISIT_DATE")) := "VISIT_DATE")  
  
  # print list of visit frequencies to see the max number of visits an individual has
  print(table(non_sched_m22$VISIT_N))
  
  # add suffix to each variable name by visit# 
  non_sched_m22_visit1 = non_sched_m22 %>% 
    filter(VISIT_N == 1) %>% 
    rename_with(~paste0(., "_VISIT1"), .cols = -c("SITE", "MOMID", "PREGID", "INFANTID"))
  
  non_sched_m22_visit2 = non_sched_m22 %>% 
    filter(VISIT_N == 2) %>% 
    rename_with(~paste0(., "_VISIT2"), .cols = -c("SITE", "MOMID", "PREGID","INFANTID"))
  
  non_sched_m22_visit3 = non_sched_m22 %>%
    filter(VISIT_N == 3) %>%
    rename_with(~paste0(., "_VISIT3"), .cols = -c("SITE", "MOMID", "PREGID","INFANTID"))
  
  ## Compile all visit type datasets into a list 
  non_sched_form_out_m22 <- mget(ls(pattern = "non_sched_m22_"))
  
  # Merge all forms together -- this should have the same number of rows as the number visit =1 observations
  non_sched_wide_m22 <- non_sched_form_out_m22 %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID","INFANTID")) %>% distinct()
  
  # add variable indicating the total number of hospitalizations an individual has
  non_sched_wide_m22 <- non_sched_wide_m22 %>% 
    mutate(M22_VISITS_TOT = sum(across(matches("VISIT_1")), na.rm = TRUE)) %>% 
    select(-contains("VISIT_1_"), -contains("VISIT_N_"))
}

## Compile all visit type datasets into a list 
non_sched_form_out_all <- mget(ls(pattern = "non_sched_wide_m"))

# Merge all forms together 
non_sched_form_wide_all <- non_sched_form_out_all %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID")) %>% distinct()

## Merge in maternal closeout form (MNH23) -- only filled out once which is why we merge it at the end  
MatData_Wide <- left_join(MatData_Wide, non_sched_form_wide_all, by =c("SITE", "MOMID", "PREGID")) %>% 
  relocate(any_of(c("SCRNID", "MOMID", "PREGID", "INFANTID")), .after = SITE) %>% 
  distinct()


# check for duplicates:
# test <- MatData_Wide %>%
#   relocate(any_of(c("SCRNID", "MOMID", "PREGID")), .after = SITE)
# out_IDS <- test[duplicated(test[,1:4]),]
# dim(out_IDS)

## remove any gesational ages >=20wks at enrollment ultrasound -- this should be flagged in queries 
## MatData_Wide <- MatData_Wide %>% filter(GA_US_DAYS_1 < 140 | is.na(GA_US_DAYS_1))
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

# export as csv 
write.csv(MatData_Wide,  file= paste("MatData_Wide","_", UploadDate,".csv",sep = ""))


