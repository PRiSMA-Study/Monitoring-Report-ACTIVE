#*****************************************************************************
#* MATERNAL WIDE DATASET BY VISIT 
#*Function: Merge all forms together in wide format to create a dataset with one row for each woman for each visit 
#*Input: .RData files for each form (generated from 1. data import code)
#* Last updated: 17 April 2023

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
# LINE 38 - set upload date

#*****************************************************************************
rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)

UploadDate = "2023-04-21"

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
## Add date variable with consistent naming across forms 
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
if (exists("m13_merged") == TRUE){ m13_merged <- m13_merged %>% mutate(VISIT_DATE = M13_VISIT_OBSSTDAT) }
if (exists("m14_merged") == TRUE){ m14_merged <- m14_merged %>% mutate(VISIT_DATE = M14_VISIT_OBSSTDAT) }
if (exists("m15_merged") == TRUE){ m15_merged <- m15_merged %>% mutate(VISIT_DATE = M15_OBSSTDAT) }
if (exists("m16_merged") == TRUE){ m16_merged <- m16_merged %>% mutate(VISIT_DATE = M16_VISDAT) }
if (exists("m17_merged") == TRUE){ m17_merged <- m17_merged %>% mutate(VISIT_DATE = M17_VISDAT) }
if (exists("m18_merged") == TRUE){ m18_merged <- m18_merged %>% mutate(VISIT_DATE = M18_VISDAT) }
if (exists("m19_merged") == TRUE){ m19_merged <- m19_merged %>% mutate(VISIT_DATE = M19_OBSSTDAT) }
if (exists("m20_merged") == TRUE){ m20_merged <- m20_merged %>% mutate(VISIT_DATE = M20_OBSSTDAT) }
if (exists("m21_merged") == TRUE){ m21_merged <- m21_merged %>% mutate(VISIT_DATE = M21_AESTDAT) }
#if (exists("m23_merged") == TRUE){ m23_merged <- m23_merged %>% mutate(VISIT_DATE = M23_CLOSE_DSSTDAT) }
if (exists("m24_merged") == TRUE){ m24_merged <- m24_merged %>% mutate(VISIT_DATE = M24_CLOSE_DSSTDAT) }
if (exists("m25_merged") == TRUE){ m25_merged <- m25_merged %>% mutate(VISIT_DATE = M25_OBSSTDAT) }
if (exists("m25_merged") == TRUE){ m25_merged <- m25_merged %>% mutate(VISIT_DATE = dmy(VISIT_DATE)) }
if (exists("m26_merged") == TRUE){ m26_merged <- m26_merged %>% mutate(VISIT_DATE = M26_FTGE_OBSTDAT) }
if (exists("m26_merged") == TRUE){ m26_merged <- m26_merged %>% mutate(VISIT_DATE = dmy(VISIT_DATE)) }

## Update mnh12 because the visit type variable naming is different 
if (exists("m12_merged") == TRUE){ m12_merged <- m12_merged %>% rename("M12_TYPE_VISIT" = "M12_PNC_N_VISIT") }

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

## Rename type_visit variables in lab and depression forms to make consistent 
if (exists("m07_merged") == TRUE){ m07_merged <- m07_merged %>% rename("M07_TYPE_VISIT" = "M07_MAT_SPEC_COLLECT_VISIT") }
if (exists("m08_merged") == TRUE){ m08_merged <- m08_merged %>% rename("M08_TYPE_VISIT" = "M08_VISIT_LBSTDAT") }
#if (exists("m25_merged") == TRUE){ m25_merged <- m25_merged %>% rename("M25_TYPE_VISIT" = "M25_ANC_VISIT_N") }

## Remove momid and pregid from MNH00 to merge -- we are going to only have the momid and pregid as defined in m02
m01_wide <- m01_merged %>% filter(M01_US_VISIT == 1) %>% ## only want enrollment visit 
  select(SITE, SCRNID,
         M01_US_GA_WKS_AGE_FTS1,M01_US_GA_DAYS_AGE_FTS1,
         M01_US_GA_WKS_AGE_FTS2,M01_US_GA_DAYS_AGE_FTS2,
         M01_US_GA_WKS_AGE_FTS3,M01_US_GA_DAYS_AGE_FTS3,
         M01_US_GA_WKS_AGE_FTS4,M01_US_GA_DAYS_AGE_FTS4,
         M01_GA_LMP_WEEKS_SCORRES, M01_GA_LMP_DAYS_SCORRES,
         M01_US_OHOSTDAT, M01_US_VISIT) %>% 
        filter(SCRNID != "n/a") ## remove scrnid == n/a for pakistan 

## Only select ID variables from MNH02 to merge into MNH01 
m02_wide <- m02_merged %>% select(SITE, SCRNID, MOMID, PREGID, M02_SCRN_OBSSTDAT) %>%  filter(SCRNID != "n/a") ## remove scrnid == n/a for pakistan 

## Merge enrollment form with US form to get GA at enrollment -- SUBSET OF DATA 
enroll_bind <- left_join(m01_wide, m02_wide, by = c("SITE", "SCRNID")) %>% distinct()
enroll_bind <- enroll_bind %>% relocate(c(MOMID,PREGID), .after = SCRNID) 


## Merge enrollment for with US form -- FULL DATA (will bind this into ANC data later)
m00_wide <- m00_merged %>% select(-c(MOMID, PREGID)) %>% filter(SCRNID != "n/a" | is.na(SCRNID)) ## remove momid and screening id 
m01_wide <- m01_merged %>% select(-c(MOMID, PREGID)) %>% filter(SCRNID != "n/a") # can't do m01 because the visit type is off 

enroll_bind_all <- left_join(m00_wide, m02_merged, by = c("SITE", "SCRNID", "TYPE_VISIT")) %>% distinct()
enroll_bind_all <- enroll_bind_all %>% relocate(c(MOMID,PREGID), .after = SCRNID) 

## calculate visit type for M26 
m01_visit1 = m01_merged %>% filter(M01_US_VISIT == 1) %>% select(-c("MOMID", "PREGID")) ## only want the first US visit (screening)

# merge M26 and M01 together 
#m26_out_tomerge_nomom_preg <- m26_merged %>% select(-c("MOMID", "PREGID"))
m26_out_tomerge <- m26_merged
m26_out <- left_join(m26_out_tomerge, m01_visit1, by = c("SITE", "SCRNID"))

# calculate GA at visit 
m26_merged <- m26_out %>% 
  mutate(GA_US_DAYS_FTS1 =  ifelse(M01_US_GA_WKS_AGE_FTS1!= -7 & M01_US_GA_DAYS_AGE_FTS1 != -7,  (M01_US_GA_WKS_AGE_FTS1 * 7 + M01_US_GA_DAYS_AGE_FTS1), NA), 
         GA_US_DAYS_FTS2 =  ifelse(M01_US_GA_WKS_AGE_FTS2!= -7 & M01_US_GA_DAYS_AGE_FTS2 != -7,  (M01_US_GA_WKS_AGE_FTS2 * 7 + M01_US_GA_DAYS_AGE_FTS2), NA),
         GA_US_DAYS_FTS3 =  ifelse(M01_US_GA_WKS_AGE_FTS3!= -7 & M01_US_GA_DAYS_AGE_FTS3 != -7,  (M01_US_GA_WKS_AGE_FTS3 * 7 + M01_US_GA_DAYS_AGE_FTS3), NA),
         GA_US_DAYS_FTS4 =  ifelse(M01_US_GA_WKS_AGE_FTS4!= -7 & M01_US_GA_DAYS_AGE_FTS4 != -7,  (M01_US_GA_WKS_AGE_FTS4 * 7 + M01_US_GA_DAYS_AGE_FTS4), NA)) %>% 
  mutate(GA_US_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% 
  mutate(M26_GA_AT_VISIT_DAYS = floor(as.numeric(difftime(VISIT_DATE,M01_US_OHOSTDAT, units = "days")) + GA_US_DAYS)) %>% 
  mutate(M26_GA_AT_VISIT_WKS = floor(as.numeric((difftime(VISIT_DATE,M01_US_OHOSTDAT, units = "days")) + GA_US_DAYS)/7)) %>% 
  mutate(M26_TYPE_VISIT = case_when(
    M26_GA_AT_VISIT_WKS >= 18 & M26_GA_AT_VISIT_WKS < 23 ~ 2, 
    M26_GA_AT_VISIT_WKS >= 26 & M26_GA_AT_VISIT_WKS < 31 ~ 3, 
    M26_GA_AT_VISIT_WKS >= 31 & M26_GA_AT_VISIT_WKS < 34 ~ 4, 
    M26_GA_AT_VISIT_WKS >= 34 & M26_GA_AT_VISIT_WKS < 39 ~ 5, 
    TRUE ~ 13
  ))  %>% 
 # mutate(M26_VISIT_COMPLETE = ifelse(M26_MAT_VITAL_MNH26 == 1 | M26_MAT_VITAL_MNH26 == 2, 1, 0)) %>% 
  select(-contains("M01_"), 
         -contains("GA_US_"), 
         -contains("M26_GA_AT_VISIT_WKS"),
         -contains("SCRNID"))
         

## Compile all merged forms into list  
all_out <- mget(ls(pattern = "_merged*"))
form_vec <- as.vector(paste((gsub("_merged","", names(all_out)))))
names(all_out) <- form_vec


#*****************************************************************************
#* ANC FORMS 
#*****************************************************************************
## Make Vector of ANC forms  
anc_vec <- c("m00", "m01", "m02", "m03", "m04", "m05", "m06", "m07", "m08", "m16", "m25", "m26")

# Make a vector of the forms that are included in the data uploaded 
anc_vec_data <- anc_vec[anc_vec %in% form_vec]

## Merge enrollment forms with each of the other forms 
anc_data <- list()
for (i in anc_vec_data[-c(1:3)]) {
  anc_data[[i]] <- full_join(all_out[[i]], enroll_bind, by = c("SITE", "MOMID", "PREGID")) %>% distinct()
}

# Extract ANC visits in data 
anc_visits <- c(1,2,3,4,5)
for (i in anc_vec_data[-c(1:3)]) {
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
  ## remove m01 variables now that we have GA at ultrasound 
  m01_to_remove <- grep("M01_", names(anc_data[[i]]))
  
  anc_visit_out[[i]] <- anc_data[[i]] %>% 
    rowwise() %>%
    ## extract the maximum gestational age for each woman 
    mutate(GA_US_DAYS_FTS1 =  ifelse(M01_US_GA_WKS_AGE_FTS1!= -7 & M01_US_GA_DAYS_AGE_FTS1 != -7,  (M01_US_GA_WKS_AGE_FTS1 * 7 + M01_US_GA_DAYS_AGE_FTS1), NA), 
           GA_US_DAYS_FTS2 =  ifelse(M01_US_GA_WKS_AGE_FTS2!= -7 & M01_US_GA_DAYS_AGE_FTS2 != -7,  (M01_US_GA_WKS_AGE_FTS2 * 7 + M01_US_GA_DAYS_AGE_FTS2), NA),
           GA_US_DAYS_FTS3 =  ifelse(M01_US_GA_WKS_AGE_FTS3!= -7 & M01_US_GA_DAYS_AGE_FTS3 != -7,  (M01_US_GA_WKS_AGE_FTS3 * 7 + M01_US_GA_DAYS_AGE_FTS3), NA),
           GA_US_DAYS_FTS4 =  ifelse(M01_US_GA_WKS_AGE_FTS4!= -7 & M01_US_GA_DAYS_AGE_FTS4 != -7,  (M01_US_GA_WKS_AGE_FTS4 * 7 + M01_US_GA_DAYS_AGE_FTS4), NA)) %>% 
    mutate(GA_US_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% 
    # extract date of enrollment US visit  
    mutate(BASELINEDATE = ymd(M02_SCRN_OBSSTDAT)) %>%
    group_by(SCRNID, MOMID, PREGID, SITE) %>% 
    ungroup() %>% 
    # add in variable for GA at visit in DAYS 
    mutate(!!paste0(toupper(i),quo_name("_GA_AT_VISIT_DAYS")) := floor(as.numeric(difftime(VISIT_DATE,M01_US_OHOSTDAT, units = "days")) + GA_US_DAYS)) %>% 
    # add in variable for GA at visit in WEEKS 
    mutate(!!paste0(toupper(i),quo_name("_GA_AT_VISIT_WKS")) := floor(as.numeric((difftime(VISIT_DATE,M01_US_OHOSTDAT, units = "days")) + GA_US_DAYS)/7)) %>% 
    # add in binary variable for ANC (Yes == 1, No == 0) 
    mutate(!!paste0(toupper(i),quo_name("_ANC_YN")) := 1) %>% 
    # add in binary variable if visit is complete 
    mutate(!!paste0(toupper(i),quo_name("_VISIT_COMPLETE")) := ifelse(if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 1 | if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 2, 1, 0)) %>% 
    ## add prefix to new variables 
    rename(!!paste0(toupper(i),quo_name("_VISIT_DATE")) := "VISIT_DATE")  %>%
    ## remove m01 variables now that we have GA at ultrasound 
    select(-any_of(m01_to_remove)) %>%
    select(-c("GA_US_DAYS_FTS1", "GA_US_DAYS_FTS2", "GA_US_DAYS_FTS3", "GA_US_DAYS_FTS4", "M02_SCRN_OBSSTDAT")) %>% 
    ## move type_visit to the front of each dataset
    relocate(c("TYPE_VISIT", "GA_US_DAYS", "BASELINEDATE"), .after = PREGID) 
}

# Merge all ANC (minus MNH00-03) forms together 
anc_data_wide <- anc_visit_out %>% reduce(full_join, by =  c("SITE","SCRNID", "MOMID", "PREGID", "TYPE_VISIT", "GA_US_DAYS", "BASELINEDATE")) %>% distinct()

# Merge MNH00 and MNH02 back into the data using the full merged dataset created above (dataframe name = enroll_bind_all)
anc_data_wide<- full_join(anc_data_wide, enroll_bind_all, by =  c("SITE","MOMID","SCRNID", "PREGID", "TYPE_VISIT")) %>% distinct()

## Merge MNH01 back into data 
m01 <- m01_merged %>% select(-c(MOMID, PREGID)) %>% filter(SCRNID != "n/a") # Remove momid and pregid (will use these IDs from MNH02 instead)
m02_mom_preg <- m02_merged %>% select(SITE, SCRNID, MOMID, PREGID) %>% filter(SCRNID != "n/a")
m01 <- full_join(m01, m02_mom_preg, by = c("SITE", "SCRNID")) %>% distinct() ## Join MNH01 and MNH02 together 
m01 <- m01 %>% relocate(MOMID, PREGID, .after = SCRNID) 


# Merge in US data for those with US at enrollment (M01_US_VISIT=1)
m01_1 <- m01 %>%
  filter(M01_US_VISIT==1) %>% 
  mutate(TYPE_VISIT = 1) %>% 
  mutate(M01_VISIT_COMPLETE = case_when(
    !is.na(M01_US_OHOSTDAT) ~ 1,
    is.na(M01_US_OHOSTDAT) ~ 0
  )) 

# Merge in US data for those with a non-enrollment US  (M01_US_VISIT=2)
m01_2 <- m01 %>%
  filter(M01_US_VISIT==2) %>% 
  mutate(M01_VISIT_COMPLETE = case_when(
    !is.na(M01_US_OHOSTDAT) ~ 1,
    is.na(M01_US_OHOSTDAT) ~ 0
  ))

# In order to calculate the GA at any non-enrollment US visit, we need to merge in the GA information from all other visits 
# In the next version of CRFs/Data Dictionary to be sent to sites, there will be an option to select the visit type at the time of US so we will not have to manually calcuate 
anc_data_wide_visit <- anc_data_wide %>%
                                  ungroup() %>% 
                                  group_by(SITE, SCRNID, MOMID, PREGID) %>%
                                  fill(BASELINEDATE, .direction = "downup")

anc_data_wide_visit_tomerge <- anc_data_wide_visit %>% select(SITE, SCRNID, MOMID, PREGID,GA_US_DAYS, BASELINEDATE) %>% filter(!is.na(BASELINEDATE)) %>% distinct() 
out <- left_join(m01_2, anc_data_wide_visit_tomerge, by = c("SITE", "SCRNID", "MOMID", "PREGID")) %>% distinct() 

## Calculate GA at US visit for ANC visit type and create visit type 
out <- out %>% 
  mutate(BASELINEDATE = replace(BASELINEDATE, BASELINEDATE== ymd("1907-07-07"), NA)) %>% 
  # add in variable for GA at visit in DAYS  
  mutate(M01_GA_AT_VISIT_DAYS = floor(as.numeric((difftime(M01_US_OHOSTDAT,BASELINEDATE, units = "days")) + GA_US_DAYS))) %>% 
  # add in variable for GA at visit in WEEKS 
  mutate(M01_GA_AT_VISIT_WKS = floor(as.numeric(((difftime(M01_US_OHOSTDAT,BASELINEDATE, units = "days")) + GA_US_DAYS)/7))) %>% 
  # calculate and assign type visit based on GA at visit 
  mutate(TYPE_VISIT = case_when(
    M01_GA_AT_VISIT_WKS >= 18 & M01_GA_AT_VISIT_WKS < 23 ~ 2, 
    M01_GA_AT_VISIT_WKS >= 26 & M01_GA_AT_VISIT_WKS < 31 ~ 3, 
    M01_GA_AT_VISIT_WKS >= 31 & M01_GA_AT_VISIT_WKS < 34 ~ 4, 
    M01_GA_AT_VISIT_WKS >= 34 & M01_GA_AT_VISIT_WKS < 39 ~ 5, 
    TRUE ~ 13
  ))  %>% 
  select(-c(BASELINEDATE, GA_US_DAYS, M01_GA_AT_VISIT_DAYS, M01_GA_AT_VISIT_DAYS, M01_GA_AT_VISIT_WKS))

## Rbind the ultrasound forms (US at screening (m01_1) and US at any other visit (out) to merge back into full dataset 
m01_data_to_merge <- rbind(m01_1, out)

## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
## Merge all ultrasound data back into full dataset
anc_data_wide_visit <- left_join(anc_data_wide_visit, m01_data_to_merge, by = c("SITE", "SCRNID", "MOMID", "PREGID", "TYPE_VISIT")) %>% distinct()

## Move MNH00 data to the front the dataframe 
m00_to_move <- grep("M00_", names(anc_data_wide_visit))
anc_data_wide_visit<- anc_data_wide_visit %>% 
  filter(SCRNID != "n/a") %>% 
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
  rename_with(~paste0(., "_", 1), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) 

## NOTE: Forms that only get filled out at enrollment (visit 1) (MNH00, MNH02, MNH03) will need to be removed from the other visit data as to not have duplicates
m00_to_remove <- grep("M00_", names(anc_data_wide_visit))
m02_to_remove <- grep("M02_", names(anc_data_wide_visit))
m03_to_remove <- grep("M03_", names(anc_data_wide_visit))

## Extract Visit 2 (ANC 20)
visit_anc_2 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 2) %>% 
  select(-any_of(m00_to_remove),
         -any_of(m02_to_remove),
         -any_of(m03_to_remove), 
         -TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 2) %>% # US variable "TYPE_VISIT" exists, but does not have the "M01_" prefix -- add here 
  rename_with(~paste0(., "_", 2), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) 
  
## Extract Visit 3 (ANC 28)
visit_anc_3 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 3) %>% 
  select(-any_of(m00_to_remove),
         -any_of(m02_to_remove),
         -any_of(m03_to_remove), 
         -TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 3) %>% # US variable "TYPE_VISIT" exists, but does not have the "M01_" prefix -- add here 
  rename_with(~paste0(., "_", 3), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) 

## Extract Visit 4 (ANC 32)
visit_anc_4 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 4) %>% 
  select(-any_of(m00_to_remove),
         -any_of(m02_to_remove),
         -any_of(m03_to_remove), 
         -TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 4) %>% # US variable "TYPE_VISIT" exists, but does not have the "M01_" prefix -- add here 
  rename_with(~paste0(., "_", 4), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) 

## Extract Visit 4 (ANC 36)
visit_anc_5 <- anc_data_wide_visit %>% 
  filter(TYPE_VISIT == 5) %>% 
  select(-any_of(m00_to_remove),
         -any_of(m02_to_remove),
         -any_of(m03_to_remove), 
         -TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 5) %>% # US variable "TYPE_VISIT" exists, but does not have the "M01_" prefix -- add here 
  rename_with(~paste0(., "_", 5), .cols = -c("SITE", "SCRNID", "MOMID", "PREGID")) 

## Compile all visit type datasets into a list 
anc_visit_out <- mget(ls(pattern = "visit_anc_"))

# Merge all forms together 
anc_data_wide <- anc_visit_out %>% reduce(full_join, by =  c("SITE","SCRNID", "MOMID", "PREGID")) %>% distinct()

#*****************************************************************************
#* IPC 
#*****************************************************************************

## Make Vector of IPC forms  
ipc_vec <- c("m05", "m06", "m07","m09", "m10", "m17")

# Make a vector of the forms that are included in the data uploaded 
ipc_vec_data <- ipc_vec[ipc_vec %in% form_vec]


# Extract IPC visits in data 
ipc_data <- list()
for (i in ipc_vec_data) {
  ipc_data[[i]] <- all_out[[i]] %>% mutate(TYPE_VISIT = if_all(matches("(.+)_TYPE_VISIT"))) %>% 
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
    ## move type_visit to the front of each dataset
    relocate(TYPE_VISIT, .after = PREGID)  
}

## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
# Merge all forms together 
ipc_data_wide_visit <- ipc_data_out %>% reduce(full_join, by =  c("SITE","MOMID", "PREGID", "TYPE_VISIT")) %>% distinct()

## Final maternal wide dataset by visit type == ipc_data_wide_visit 

## IF WE WANT TO HAVE A WIDE DATASET WITH 1 ROW FOR EACH WOMAN, THEN WE NEED TO ADD A SUFFIX TO EACH OF THE VARIABLE NAMES 
## The code does the following:   
  # 1. remove visit type variable (a form-specific visit type variable remains, ex. M09_TYPE_VISIT = 6)
  # 2. Add suffix to the end of the varnames. Since IPC only happens at one time point, all suffixes are "6"
ipc_data_out_wide <- list()
for(i in names(ipc_data)){
  ipc_data_out_wide[[i]] <- ipc_data_out[[i]] %>% 
    select(-TYPE_VISIT) %>% 
    rename_with(~paste0(., "_", 6), .cols = -c("SITE", "MOMID", "PREGID")) 
}

## Final maternal wide dataset == anc_data_wide 
ipc_data_wide <- ipc_data_out_wide %>% reduce(full_join, by =  c("SITE","MOMID", "PREGID"))

#*****************************************************************************
#* PNC 
#*****************************************************************************
## Make Vector of PNC forms  
pnc_vec <- c("m05", "m06", "m07", "m08", "m12", "m18", "m25", "m26")

# Make a vector of the forms that are included in the data uploaded 
pnc_vec_data <- pnc_vec[pnc_vec %in% form_vec]

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
  mutate(DOB = 
           pmin(DELIVERY_DATETIME_INF1, DELIVERY_DATETIME_INF2, 
                DELIVERY_DATETIME_INF3, DELIVERY_DATETIME_INF4, na.rm = TRUE)) %>% 
  select(SITE, MOMID, PREGID, DOB, M09_INFANTS_FAORRES)

## Merge enrollment forms with each of the other forms 
pnc_data <- list()
for (i in pnc_vec_data) { 
  pnc_data[[i]] <- left_join( all_out[[i]], m09, by = c("SITE", "MOMID", "PREGID")) %>% distinct()
}

# Extract PNC visits in data 
pnc_visits <- c(7,8,9,10,11,12)
for (i in pnc_vec_data) {
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
    mutate(!!paste0(toupper(i),quo_name("_PNC_AT_VISIT_DAYS")) := floor(as.numeric(difftime(VISIT_DATE,DOB), units = "days"))) %>% 
    # calculate WEEKS since delivery 
    mutate(!!paste0(toupper(i),quo_name("_PNC_AT_VISIT_WKS")) := floor(as.numeric(difftime(VISIT_DATE,DOB), units = "days")/7)) %>% 
    # add in binary variable for pnc (Yes == 1, No == 0) 
    mutate(!!paste0(toupper(i),quo_name("_PNC_YN")) := 1) %>% 
    # add in binary variable if visit is complete 
    mutate(!!paste0(toupper(i),quo_name("_VISIT_COMPLETE")) := ifelse(if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 1 | if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 2, 1, 0)) %>% 
    ## add prefix in to all new variables 
    rename(!!paste0(toupper(i),quo_name("_VISIT_DATE")) := "VISIT_DATE")  %>% 
    ## remove m09 variables
    select(-any_of(m09_to_remove)) %>% 
    ## move type_visit to the front of each dataset
    relocate(TYPE_VISIT, .after = PREGID) 
}


## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
pnc_data_wide_visit <- pnc_data_out %>% reduce(full_join, by =  c("SITE","MOMID", "PREGID", "TYPE_VISIT", "DOB"))

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
  rename_with(~paste0(., "_", 7), .cols = -c("SITE", "MOMID", "PREGID", "DOB")) 

## Extract Visit 8 (PNC 1)
visit_pnc_8 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 8) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 8) %>% 
  rename_with(~paste0(., "_", 8), .cols = -c("SITE", "MOMID", "PREGID", "DOB")) 

## Extract Visit 9 (PNC 4)
visit_pnc_9 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 9) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 9) %>% 
  rename_with(~paste0(., "_", 9), .cols = -c("SITE", "MOMID", "PREGID", "DOB")) 

## Extract Visit 10 (PNC 6)
visit_pnc_10 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 10) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 10) %>% 
  rename_with(~paste0(., "_", 10), .cols = -c("SITE", "MOMID", "PREGID", "DOB")) 

## Extract Visit 11 (PNC 26)
visit_pnc_11 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 11) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 11) %>% 
  rename_with(~paste0(., "_", 11), .cols = -c("SITE", "MOMID", "PREGID", "DOB")) 

## Extract Visit 12 (PNC 52)
visit_pnc_12 <- pnc_data_wide_visit %>% 
  filter(TYPE_VISIT == 12) %>% 
  select(-TYPE_VISIT) %>% 
  mutate(M01_TYPE_VISIT = 12) %>% 
  rename_with(~paste0(., "_", 12), .cols = -c("SITE", "MOMID", "PREGID", "DOB")) 

## Compile all visit type datasets into a list 
pnc_visit_out <- mget(ls(pattern = "visit_pnc_"))

## THE FOLLOWING CODE WILL GENERATE A WIDE DATASET WITH ONE ROW FOR EACH MOM FOR EACH VISIT 
# Merge all forms together 
pnc_data_wide <- pnc_visit_out %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID", "DOB")) %>% distinct()
## Final maternal wide dataset == pnc_data_wide 

#*****************************************************************************
#* Merge all ANC, IPC, PNC data to get wide dataset BY VISIT 
#* One row for each mom at each visit 
#*****************************************************************************

### MERGE ALL TOGETHER - BY VISIT 
out <- list(anc_data_wide_visit, ipc_data_wide_visit, pnc_data_wide_visit)
MatData_Wide_Visit <- out %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID", "TYPE_VISIT"))  

## Merge in maternal closeout form (MNH23) -- only filled out once which is why we merge it at the end  
MatData_Wide_Visit <- left_join(MatData_Wide_Visit, m23_merged, by =c("SITE", "MOMID", "PREGID"))

# export
setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
save(MatData_Wide_Visit, file= paste("MatData_Wide_Visit","_", UploadDate,".RData",sep = ""))

#*****************************************************************************
#* Merge all ANC, IPC, PNC data to get wide dataset 
#* #* One row for each mom 
#*****************************************************************************

### MERGE ALL TOGETHER - BY MOMID, PREGID, SCRNID  
out <- list(anc_data_wide, ipc_data_wide, pnc_data_wide)
MatData_Wide <- out %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID"))  %>% relocate(DOB, .after = PREGID)%>% distinct()

## Merge in maternal closeout form (MNH23) -- only filled out once which is why we merge it at the end  
MatData_Wide <- full_join(MatData_Wide, m23_merged, by =c("SITE", "MOMID", "PREGID")) %>% 
  relocate(any_of(c("SCRNID", "MOMID", "PREGID")), .after = SITE) %>% 
  distinct()

## remove duplicates 8566 (n = 23 (15 unique IDs))
out_IDS <- MatData_Wide[duplicated(MatData_Wide[,1:4]),]
out_duplicated_IDS <- out_IDS %>% pull(MOMID)
MatData_Wide <- MatData_Wide %>% filter(!(MOMID %in% out_duplicated_IDS))

# export
setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
save(MatData_Wide, file= paste("MatData_Wide","_", UploadDate,".RData",sep = ""))

