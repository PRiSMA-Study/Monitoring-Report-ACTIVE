#*****************************************************************************
#* INFANT WIDE DATASET by VISIT 
#*Function: Merge all forms together in wide format to create a dataset with one row for each woman for each visit 
#*Input: .RData files for each form (generated from 1. data import code)
#* Last updated: 17 April 2023

#*Output:   
#* 1. InfData_Wide.RData wide dataset by INFANTID and visit type (one row for each infant at each visit)
#* 2. InfData_Wide_Visit.RData wide dataset by INFANTID (one row for each infant)


#* STEPS: 
#* 1. Import merged data 
#* 2. Add date variable with consistent naming across forms 
#* 3. Update mnh13 because the visit type variable naming is different
#* 4. Hard code visit type for M11 (These forms are only filled out at one time point and don't have a visit type variable)
#* 5. Reorganize MNH09 to long format where there is only one column for INFANTID (instead of INFANTID_INF1, INFANTID_INF2, etc.)
#* 6. Merge all infant forms 
#* 7. Assign suffix to the end of each form for each pnc visit 
#* 8. Make data wide using SITE, INFANTID, VISIT TYPE - InfData_Wide_Visit.RData
#* 9. Make data wide using SITE, INFANTID - InfData_Wide.RData
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
## rename visit date
if (exists("m09_merged") == TRUE){ m09_merged <- m09_merged %>% mutate(VISIT_DATE = M09_MAT_LD_OHOSTDAT) }
#if (exists("m11_merged") == TRUE){ m11_merged <- m11_merged %>% mutate(VISIT_DATE = M11_VISIT_OBSSTDAT) }
if (exists("m13_infantmerged") == TRUE){ m13_infantmerged <- m13_infantmerged %>% mutate(VISIT_DATE = M13_VISIT_OBSSTDAT) }
if (exists("m14_infantmerged") == TRUE){ m14_infantmerged <- m14_infantmerged %>% mutate(VISIT_DATE = M14_VISIT_OBSSTDAT) }
if (exists("m15_infantmerged") == TRUE){ m15_infantmerged <- m15_infantmerged %>% mutate(VISIT_DATE = M15_OBSSTDAT) }
if (exists("m20_infantmerged") == TRUE){ m20_infantmerged <- m20_infantmerged %>% mutate(VISIT_DATE = M20_OBSSTDAT) }
if (exists("m24_infantmerged") == TRUE){ m24_infantmerged <- m24_infantmerged %>% mutate(VISIT_DATE = M24_CLOSE_DSSTDAT) }

## add visit type for M111, M13, M14, M15 -- to update once data dictionary updates go out 
if (exists("m11_merged") == TRUE){ m11_merged$M11_TYPE_VISIT = 6}
if (exists("m13_infantmerged") == TRUE){ m13_infantmerged <- m13_infantmerged %>% rename(M13_TYPE_VISIT = M13_PNC_N_VISIT)}
if (exists("m14_infantmerged") == TRUE){ m14_infantmerged <- m14_infantmerged %>% rename(M14_TYPE_VISIT = M14_POC_VISIT)}
if (exists("m15_infantmerged") == TRUE){ m15_infantmerged <- m15_infantmerged %>% rename(M15_TYPE_VISIT = M15_OBSTERM)}

## update visit type assignments for pak data 
m13_infantmerged <- m13_infantmerged %>% 
  mutate(M13_TYPE_VISIT = ifelse(M13_TYPE_VISIT == 1, 7, 
                                 ifelse(M13_TYPE_VISIT == 2, 8, 
                                        ifelse(M13_TYPE_VISIT == 3, 9, 
                                               ifelse(M13_TYPE_VISIT == 4, 10, 
                                                      ifelse(M13_TYPE_VISIT == 5, 11, 
                                                             ifelse(M13_TYPE_VISIT == 6, 12, M13_TYPE_VISIT)))))))
## extract date of birth for each infant 
# replace default value date with NA 
m09_wide <- m09_merged %>% 
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
  mutate(M09_DELIVERY_DATETIME_INF1 = as.POSIXct(DELIVERY_DATETIME_INF1, format= "%Y-%m-%d %H:%M"),
         M09_DELIVERY_DATETIME_INF2 = as.POSIXct(DELIVERY_DATETIME_INF2, format= "%Y-%m-%d %H:%M"),
         M09_DELIVERY_DATETIME_INF3 = as.POSIXct(DELIVERY_DATETIME_INF3, format= "%Y-%m-%d %H:%M"),
         M09_DELIVERY_DATETIME_INF4 = as.POSIXct(DELIVERY_DATETIME_INF4, format= "%Y-%m-%d %H:%M"))


## first need to make m09 long format for each infant 
m09_INF1 <- m09_wide %>% 
  rename("INFANTID" = "M09_INFANTID_INF1",
         "M09_CES_FAORRES_INF1" = "M09_CES_PROCCUR_INF1") %>% 
  filter(INFANTID != "n/a") %>% 
  select(SITE, MOMID, PREGID,INFANTID, contains("_INF1")) %>% 
  rename_with(~str_remove(., '_INF1')) 

m09_INF2 <- m09_wide %>% rename("INFANTID" = "M09_INFANTID_INF2") %>% 
  filter(INFANTID != "n/a") %>% 
  select(SITE, MOMID, PREGID,INFANTID, contains("_INF2")) %>% 
  rename_with(~str_remove(., '_INF2')) 

m09_INF3 <- m09_wide %>% rename("INFANTID" = "M09_INFANTID_INF3") %>% 
  filter(INFANTID != "n/a") %>% 
  select(SITE, MOMID, PREGID,INFANTID, contains("_INF3")) %>% 
  rename_with(~str_remove(., '_INF3')) 

m09_INF4 <- m09_wide %>% rename("INFANTID" = "M09_INFANTID_INF4") %>%
  filter(INFANTID != "n/a") %>% 
  select(SITE, MOMID, PREGID,INFANTID, contains("_INF4")) %>% 
  rename_with(~str_remove(., '_INF4'))

## bind all infants together 
m09_inf <- bind_rows(m09_INF1, m09_INF2, m09_INF3, m09_INF4) 
## put all forms into a list 
all_out <- mget(ls(pattern = "_infantmerged*"))
form_vec <- as.vector(paste((gsub("_infantmerged","", names(all_out)))))
names(all_out) <- form_vec

## get PNC forms vector 
inf_vec <- c("m13", "m14", "m15", "m20")

# get a vector of the forms that are in the data pulled 
inf_vec_data <- inf_vec[inf_vec %in% form_vec]

## merge birth data with each of the other forms 
inf_data <- list()
for (i in inf_vec_data) {
  inf_data[[i]] <- left_join(all_out[[i]], m09_inf, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% distinct()
}

# get pnc visit types allowed 
inf_visits <- c(7,8,9,10,11,12)
for (i in inf_vec_data) {
 # inf_data[[i]] <- inf_data[[i]] %>% rename("TYPE_VISIT" = paste(toupper(i),"_","TYPE_VISIT", sep = "")) %>% 
  inf_data[[i]] <- inf_data[[i]] %>% mutate(TYPE_VISIT = if_all(matches("(.+)_TYPE_VISIT"))) %>% 
  filter(TYPE_VISIT %in% inf_visits)
}

## Add binary PNC Yes/No column (1 = Yes, 0 = No)
## calculate days PNC 
inf_data_out <- list()
for(i in names(inf_data)){
  ## remove m09 variables now that we have age at visit  
  m09_to_remove <- grep("M09_", names(inf_data[[i]]))

  inf_data_out[[i]] <- inf_data[[i]] %>% 
    rowwise() %>%
    group_by(SITE, MOMID, PREGID, INFANTID) %>% 
    ungroup() %>% 
    mutate(!!paste0(toupper(i),quo_name("_PNC_AT_VISIT_DAYS")) := floor(as.numeric(difftime(VISIT_DATE,DELIVERY_DATETIME, units = "days"))), 0) %>% 
    # add in variable for GA at visit in WEEKS 
    mutate(!!paste0(toupper(i),quo_name("_PNC_AT_VISIT_WKS")) := floor(as.numeric((difftime(VISIT_DATE,DELIVERY_DATETIME, units = "days")))/7)) %>% 
    # add in binary variable for PNC (Yes == 1, No == 0) 
    mutate(!!paste0(toupper(i),quo_name("_PNC_YN")) := 1) %>% 
    # add in binary variable if visit is complete 
    mutate(!!paste0(toupper(i),quo_name("_VISIT_COMPLETE")) := ifelse(if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 1 | if_all(matches("(.+)_MAT_VISIT_MNH(.+)")) == 2, 1, 0)) %>% 
    ## add prefix in to all new variables 
    rename(!!paste0(toupper(i),quo_name("_VISIT_DATE")) := "VISIT_DATE") %>% 
    #relocate(TYPE_VISIT, .after = PREGID) %>%  ## move type_visit to the front of each dataset
    #rename(!!paste0(toupper(i),quo_name("_TYPE_VISIT")) := "TYPE_VISIT") %>% 
    select(-any_of(m09_to_remove))    ## remove m09 variables 
}

## get vector of all the visits in the data 
out <- list()
for(i in names(inf_data_out)){
  out[[i]] <- inf_data_out[[i]] %>% 
    select((matches("_TYPE_VISIT"))) %>% distinct() %>% pull()
}

vec_visit <-  unique(unlist(out, use.names = FALSE))

# merge all forms together 
inf_data_out_wide <- inf_data_out %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID", "INFANTID", "DELIVERY_DATETIME", "TYPE_VISIT")) %>% distinct() 
inf_data_out_wide <- inf_data_out_wide %>% relocate(TYPE_VISIT, .after= INFANTID)

## BY VISIT TYPE 
## Merge in MNH11 post-delivery outcomes 
m11_merged_out <- m11_merged %>% select(-c("MOMID", "PREGID"))
InfData_Wide_Visit <- full_join(inf_data_out_wide, m11_merged_out, by = c("SITE", "INFANTID")) 

## export data 
setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
save(InfData_Wide_Visit, file= paste("InfData_Wide_Visit","_", UploadDate,".RData",sep = ""))

# ## BY MOMID, INFANTID
# visit_pnc_6 <- inf_data_out_wide %>% 
#   filter(TYPE_VISIT == 6) %>% 
#   select(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, DELIVERY_DATETIME, contains("M11_")) %>% 
#   select(-TYPE_VISIT) %>% 
#   rename_with(~paste0(., "_", 6), .cols = -c("SITE", "MOMID", "PREGID", "INFANTID", "DELIVERY_DATETIME")) 
# 
# ## NOTE: Forms that only get filled out at delivery (MNH11) will need to be removed from the other visit data as to not have duplicates
# m11_to_remove <- grep("M11_", names(InfData_Wide_Visit))
# 
visit_pnc_7 <- inf_data_out_wide %>% 
  filter(TYPE_VISIT == 7) %>% 
  select(-TYPE_VISIT) %>% 
  rename_with(~paste0(., "_", 7), .cols = -c("SITE", "MOMID", "PREGID", "INFANTID", "DELIVERY_DATETIME")) 

visit_pnc_8 <- inf_data_out_wide %>% 
  filter(TYPE_VISIT == 8) %>% 
  select(-TYPE_VISIT) %>% 
  rename_with(~paste0(., "_", 8), .cols = -c("SITE", "MOMID", "PREGID", "INFANTID", "DELIVERY_DATETIME")) 


visit_pnc_9 <- inf_data_out_wide %>% 
  filter(TYPE_VISIT == 9) %>% 
  select(-TYPE_VISIT) %>% 
  rename_with(~paste0(., "_", 9), .cols = -c("SITE", "MOMID", "PREGID", "INFANTID", "DELIVERY_DATETIME")) 

visit_pnc_10 <- inf_data_out_wide %>% 
  filter(TYPE_VISIT == 10) %>% 
  select(-TYPE_VISIT) %>% 
  rename_with(~paste0(., "_", 10), .cols = -c("SITE", "MOMID", "PREGID", "INFANTID", "DELIVERY_DATETIME")) 

visit_pnc_11 <- inf_data_out_wide %>% 
  filter(TYPE_VISIT == 11) %>% 
  select(-TYPE_VISIT) %>% 
  rename_with(~paste0(., "_", 11), .cols = -c("SITE", "MOMID", "PREGID", "INFANTID", "DELIVERY_DATETIME")) 

visit_pnc_12 <- inf_data_out_wide %>% 
  filter(TYPE_VISIT == 12) %>% 
  select(-TYPE_VISIT) %>% 
  rename_with(~paste0(., "_", 12), .cols = -c("SITE", "MOMID", "PREGID", "INFANTID", "DELIVERY_DATETIME")) 

infant_pnc_visit_out <- mget(ls(pattern = "visit_pnc_"))
# merge all forms together 
InfData_Wide <- infant_pnc_visit_out %>% reduce(full_join, by =  c("SITE", "MOMID", "PREGID", "INFANTID", "DELIVERY_DATETIME")) %>% distinct()

## Merge in MNH11 post-delivery outcomes - delete this because we added in above 
#InfData_Wide <- full_join(InfData_Wide, m11_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))

# merge m24 infant closeout into dataset
m24_infantmerged <- m24_infantmerged %>% 
  mutate(M24_VISIT_COMPLETE = ifelse(!is.na(M24_CLOSE_DSDECOD), 1, 0))


InfData_Wide <- full_join(InfData_Wide, m24_infantmerged, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% distinct()

## Merge in MNH11 post-delivery outcomes 
m11_merged_out <- m11_merged %>% select(-c("MOMID", "PREGID"))
InfData_Wide <- full_join(InfData_Wide, m11_merged_out, by = c("SITE","INFANTID")) %>% distinct()

## export data 
setwd(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, sep = ""))
#dt=format(Sys.time(), "%Y-%m-%d")
save(InfData_Wide, file= paste("InfData_Wide","_", UploadDate,".RData",sep = ""))
