#*****************************************************************************
#*Function: Read in raw data and merged all sites data by form 
#*Input: Raw .csvs for each site 
#*Output: .RData file for each form that is merged for all sites 
#* Last updated: 09 April 2024 (new date formatting)


## STEPS ## 
#*1. Import data from each site 
#*2  Import data dictionary        ## this will be used to inform field types 
#*3. Merge all site data together by each form 
## a. For each form, compile all site data into list 
## b. Assign field types (Numeric, Date)
## c. Remove duplicate IDs 
## d. Add form number prefix to each variable name (ex. "MNH00_")
## e. Add "SITE" variable to the beginning of data
## f. Merge all site forms together 
## g. Export data in .RData format

#* 4. Extract missing varnames from site data (by form)

## LINES TO UPDATE EACH RUN: 
# LINE 44 - set upload date
# LINE 48 - set vector of all sites with data in that upload 
#*****************************************************************************
## UPDATE ALL DATE CODES
# rm(list = ls())
library(tidyverse)
library(readr)
library(dplyr)
library(data.table)
library(stringr)
library(lubridate)
library(naniar)
library(readxl)

# UPDATE EACH RUN # 
# set upload date 
UploadDate = "2024-04-05"

# UPDATE EACH RUN # 
# create vector of all sites with data in the upload 
site_vec <- c("Pakistan", "Kenya", "Ghana", "Zambia", "India-CMC", "India-SAS")

# set path to save 
path_to_save = paste0("~/Monitoring Report/data/merged/" ,UploadDate, "/")

## import data dictionary -- this will be used to pull field types for each variable  
data_dict <-read_excel("~/PRiSMAv2Data/PRISMA-Data-Queries-GW/R/PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx")
data_dict <- data_dict %>% 
  select(Form,`Variable Name`, `Field Type (Date, Time, Number, Text)`) %>% 
  rename("FieldType" = `Field Type (Date, Time, Number, Text)`)
data_dict$`Variable Name` <- toupper(data_dict$`Variable Name`)

## Change working directory to a site-specific folder OFF network drive -- DATA folder 
# first need to make subfolder with upload date
merged_dir <- paste0("~/Monitoring Report/data/merged", sep = "")
cleaned_dir <- paste0("~/Monitoring Report/data/cleaned", sep = "")
date_dir <- UploadDate
dir.create(file.path(merged_dir, date_dir), showWarnings = FALSE)
dir.create(file.path(cleaned_dir, date_dir), showWarnings = FALSE)
#*****************************************************************************
# #*Will need to set directory and read data for each country 
#*****************************************************************************
# set working directory to network drive
site = "Pakistan"
# setwd(paste("Z:/SynapseCSVs/",site,"/",UploadDate, sep = ""))
setwd(paste("~/","import","/",UploadDate,"_pak", sep = ""))

## import raw .CSVs in wide format
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

#  ## make sure all column names are uppercase
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

## convert to individual dataframes
names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)
list2env(myfiles, globalenv())

# replace empty momid and pregid with NA
mnh02_Pakistan <- mnh02_Pakistan %>%
  mutate(MOMID = ifelse(str_detect(MOMID, "n/a"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "n/a"), NA, PREGID))

# rename MOMID variable in mnh28
mnh28_Pakistan <- mnh28_Pakistan %>% rename("MOMID" = "VR.ID")

#************************Kenya************************

site = "Kenya"
# setwd(paste("Z:/SynapseCSVs/",site,"/",UploadDate, sep = ""))
setwd(paste("~/","import","/",UploadDate,"_ke", sep = ""))

## import raw .CSVs in wide format
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

#  ## make sure all column names are uppercase
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

## convert to individual dataframes
names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)
list2env(myfiles, globalenv())

## pull mnh02 MOMID and PREGID and merge into MNH01 -- only SCRNID is reported for enrollment visits in mnh01
mnh02_ids <- mnh02_Kenya %>% select(MOMID,PREGID, SCRNID) %>%
  mutate(MOMID = ifelse(MOMID == "N/A",NA, MOMID),
         PREGID = ifelse(is.na(MOMID), NA, PREGID))

mnh01_Kenya <- mnh01_Kenya %>%
  select(-MOMID, -PREGID) %>%
  left_join(mnh02_ids, by = c("SCRNID"))

rm(mnh02_ids)

# replace empty momid and pregid with NA
mnh01_Kenya <- mnh01_Kenya %>%
  mutate(MOMID = ifelse(str_detect(MOMID, "K"), MOMID, NA),
         PREGID = ifelse(str_detect(PREGID, "K"), PREGID, NA))

mnh02_Kenya <- mnh02_Kenya %>%
  mutate(MOMID = ifelse(str_detect(MOMID, "K"), MOMID, NA),
         PREGID = ifelse(str_detect(PREGID, "K"), PREGID, NA))

# rename infantid variable in mnh28
mnh28_Kenya <- mnh28_Kenya %>% rename("INFANTID" = "INFANTID_INF")
#************************Zambia************************
site = "Zambia"
# setwd(paste("Z:/SynapseCSVs/",site,"/",UploadDate, sep = ""))
setwd(paste("~/","import","/","2024-03-08","_zam", sep = ""))

## import raw .CSVs in wide format
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

#  ## make sure all column names are uppercase
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

## convert to individual dataframes
names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)
list2env(myfiles, globalenv())

# ## 12/-8 update for zambia - they had all ZAPPS variables
variable_names <- read_excel("~/PRiSMAv2Data/PRISMA-Data-Queries-GW/R/PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx")
variable_names <- variable_names %>% filter(Form == "MNH01") %>% pull(`Variable Name`)

mnh01_Zambia <- mnh01_Zambia %>%  select(all_of(variable_names))


# replace empty momid and pregid with NA
mnh01_Zambia <- mnh01_Zambia %>%
  mutate(MOMID = ifelse(str_detect(MOMID, "Z"), MOMID, NA),
         PREGID = ifelse(str_detect(PREGID, "Z"), PREGID, NA))

mnh02_Zambia <- mnh02_Zambia %>%
  mutate(MOMID = ifelse(str_detect(MOMID, "Z"), MOMID, NA),
         PREGID = ifelse(str_detect(PREGID, "Z"), PREGID, NA))

# rename MOMID variable in mnh28
mnh28_Zambia <- mnh28_Zambia %>% rename("MOMID" = "PTID")

#************************Ghana************************
site = "Ghana"
# setwd(paste("Z:/SynapseCSVs/",site,"/",UploadDate, sep = ""))
setwd(paste("~/","import","/",UploadDate,"_gha", sep = ""))

## import raw .CSVs in wide format
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

#  ## make sure all column names are uppercase
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

## convert to individual dataframes
names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)
list2env(myfiles, globalenv())

## pull mnh02 MOMID and PREGID and merge into MNH01 -- only SCRNID is reported for enrollment visits in mnh01
mnh02_ids <- mnh02_Ghana %>% select(MOMID,PREGID, SCRNID)

mnh01_Ghana <- mnh01_Ghana %>%
  select(-SCRNID) %>%
  left_join(mnh02_ids, by = c("MOMID", "PREGID"))

rm(mnh02_ids)
#************************India-CMC************************
site = "India_CMC"

# setwd(paste("Z:/SynapseCSVs/",site,"/",UploadDate, sep = ""))
setwd(paste("~/","import","/",UploadDate,"_cmc", sep = ""))

## import raw .CSVs in wide format
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

#  ## make sure all column names are uppercase
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

site = "India-CMC"

## convert to individual dataframes
names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)
list2env(myfiles, globalenv())

## pull mnh02 MOMID and PREGID and merge into MNH01 -- only SCRNID is reported for enrollment visits in mnh01
mnh02_ids <- `mnh02_India-CMC` %>% select(MOMID,PREGID, SCRNID)
`mnh01_India-CMC` <- `mnh01_India-CMC` %>% select(-MOMID, -PREGID) %>%
  left_join(mnh02_ids, by = c("SCRNID"))

rm(mnh02_ids)

# replace empty momid and pregid with NA
`mnh01_India-CMC` <- `mnh01_India-CMC` %>%
  mutate(MOMID = ifelse(str_detect(MOMID, "n/a"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "n/a"), NA, PREGID))

`mnh02_India-CMC` <- `mnh02_India-CMC` %>%
  mutate(MOMID = ifelse(str_detect(MOMID, "n/a"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "n/a"), NA, PREGID))


#************************India-SAS************************
site = "India_SAS"

# setwd(paste("Z:/SynapseCSVs/",site,"/",UploadDate, sep = ""))
setwd(paste("~/","import","/",UploadDate,"_sas", sep = ""))

## import raw .CSVs in wide format
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

#  ## make sure all column names are uppercase
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

site = "India-SAS"

## convert to individual dataframes
names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)
list2env(myfiles, globalenv())

## pull mnh02 MOMID and PREGID and merge into MNH01 -- only SCRNID is reported for enrollment visits in mnh01
mnh02_ids <- `mnh02_India-SAS` %>% select(MOMID,PREGID, SCRNID)
`mnh01_India-SAS` <- `mnh01_India-SAS` %>% select(-MOMID, -PREGID) %>%
  left_join(mnh02_ids, by = c("SCRNID"))

rm(mnh02_ids)

# replace empty momid and pregid with NA
`mnh01_India-SAS` <- `mnh01_India-SAS` %>%
  mutate(MOMID = ifelse(str_detect(MOMID, "n/a"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "n/a"), NA, PREGID))


`mnh02_India-SAS` <- `mnh02_India-SAS` %>%
  mutate(MOMID = ifelse(str_detect(MOMID, "n/a"), NA, MOMID),
         PREGID = ifelse(str_detect(PREGID, "n/a"), NA, PREGID))

#*************************************************
#* Merge all data by form and site 
#*************************************************
# Remove lists made in the stacked data
# List all objects in the global environment
objects <- ls()
# 
# # Identify data frames that match the pattern
data_frames_to_delete_1 <- objects[grep("m0", objects)]
data_frames_to_delete_2 <- objects[grep("m2", objects)]

# 
# # Remove identified data frames
rm(list = data_frames_to_delete_1)
rm(list = data_frames_to_delete_2)


#******M00

# Compile all site MNH00 data into list  
for (x in site_vec) {
  if (exists(paste("mnh00_", x, sep = ""))==TRUE){
    
    m00 <- mget(ls(pattern = "mnh00_.*"))
    
  }
}

if (exists("m00") == TRUE) {
  
  # Assign field types (Numeric, Date)
  # From data dictionary, extract all MNH00 variables for both Numeric and Date field types 
  data_dict_m00 <- data_dict %>% filter(Form == "MNH00") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m00, function(df) {
    m00_dd_numeric <- data_dict_m00 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m00_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  })
  
  date <- lapply(numeric, function(df) {
    m00_dd_date <- data_dict_m00 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m00_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m00 = date
  
  # Remove duplicate IDs 
  # Add form number prefix to each variable (ex. "MNH00_")
  m00_rbind <- lapply(m00, function(x) x %>% 
                        select(SCRNID, MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M00_", .), -c(1:3)) %>% 
                        replace_with_na(replace = list(MOMID = c("n/a"),
                                                       PREGID = c("n/a"))) %>% 
                        group_by(SCRNID) %>%  
                        arrange(desc(M00_SCRN_OBSSTDAT)) %>% 
                        slice(1) %>% 
                        ungroup() %>% 
                        ## remove momid and pregid - only want scrnid in prescreening form
                        select(-MOMID, -PREGID) %>% 
                        distinct() ## don't pull any duplicate rows -- will need to check to make sure this isn't pulling any duplicate IDs 
                      
  )
  
  # Add "SITE" variable to the beginning of data
  for(i in names(m00_rbind)){
    m00_rbind[[i]]$SITE <- paste(gsub("mnh00_","",i))
  }
  
  # Extract list of all variable names (will use this to match the order of varnames in each dataset)
  allNms <- unique(unlist(lapply(m00_rbind, names)))
  
  # Merge all MNH00 forms together 
  m00_merged <- do.call(rbind,c(lapply(m00_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),     ## this function will make the order of varnames consistent between dataframes
                           function(y) NA)))), make.row.names=FALSE))
  
  # Make SITE the first variable 
  m00_merged <- m00_merged %>% relocate(SITE)
  
  # Extract missing varnames from site data
  m00_missing <- lapply(m00_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m00_duplicates <- m00_merged %>% 
    group_by(SITE, SCRNID) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH00") %>% 
    select(FORM, SITE, SCRNID, M00_SCRN_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m00_merged <- m00_merged %>% anti_join(m00_duplicates, by = c("SITE","SCRNID"))
  
  # Export data in .RData format
  save(m00_merged, file= paste0(path_to_save, "m00_merged",".RData",sep = ""))
  
}

#******M01
# Compile all site MNH00 data into list  
for (x in site_vec) {
  if (exists(paste("mnh01_", x, sep = ""))==TRUE){
    
    m01 <- mget(ls(pattern = "mnh01_.*"))
    
  }
}

if (exists("m01") == TRUE) {
  
  # Assign field types (Numeric, Date)
  # From data dictionary, extract all MNH00 variables for both Numeric and Date field types 
  data_dict_m01 <- data_dict %>% filter(Form == "MNH01") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m01, function(df) {
    m01_dd_numeric <- data_dict_m01 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m01_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  })
  
  date <- lapply(numeric, function(df) {
    m01_dd_date <- data_dict_m01 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m01_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) 
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m01 = date
  
  # Remove duplicate IDs 
  # Add form number prefix to each variable (ex. "MNH00_")
  m01_rbind <- lapply(m01, function(x) x %>% 
                        #add  "M##_"
                        select(SCRNID, MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M01_", .), -c(1:3)) %>% 
                        replace_with_na(replace = list(MOMID = c("n/a"),
                                                       PREGID = c("n/a"))) %>% 
                        #                        group_by(SCRNID, MOMID, PREGID, M01_US_VISIT) %>%  
                        #                        arrange(desc(M01_US_OHOSTDAT)) %>% 
                        #                        slice(1) %>% 
                        ungroup() %>% 
                        distinct()
  )
  
  # Add "SITE" variable to the beginning of data
  for(i in names(m01_rbind)){
    m01_rbind[[i]]$SITE <- paste(gsub("mnh01_","",i))
  }
  
  # Extract list of all variable names (will use this to match the order of varnames in each dataset)
  allNms <- unique(unlist(lapply(m01_rbind, names)))
  
  # Merge all MNH01 forms together 
  m01_merged <- do.call(rbind,c(lapply(m01_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # Make SITE the first variable 
  m01_merged <- m01_merged %>% relocate(SITE)
  
  # Extract missing varnames from site data
  m01_missing <- lapply(m01_rbind, function(x) setdiff(allNms, colnames(x)))
  
  
  # Extract duplicates into their own dataset
  m01_duplicates <- m01_merged %>% 
    group_by(SITE, SCRNID, MOMID, PREGID, M01_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M01_TYPE_VISIT != 13) %>% 
    mutate(FORM = "MNH01") %>% 
    select(FORM, SITE, SCRNID, MOMID, PREGID, M01_TYPE_VISIT, M01_US_OHOSTDAT)
  
  # Extract duplicates from merged data 
  m01_merged <- m01_merged %>% anti_join(m01_duplicates, by = c("SITE","SCRNID", "MOMID", "PREGID", "M01_TYPE_VISIT"))

  # Export data in .RData format
  save(m01_merged, file= paste0(path_to_save, "m01_merged",".RData",sep = ""))
  
}

#******M02 

# Compile all site MNH02 data into list 
for (x in site_vec) {
  if (exists(paste("mnh02_", x, sep = ""))==TRUE){
    
    m02 <- mget(ls(pattern = "mnh02_.*"))
    
  }
}

if (exists("m02") == TRUE) {
  # Assign field types (Numeric, Date)
  # From data dictionary, extract all MNH00 variables for both Numeric and Date field types
  m02_names = as.vector(names(m02))
  
  # assign field types
  data_dict_m02 <- data_dict %>% filter(Form == "MNH02") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m02, function(df) {
    m02_dd_numeric <- data_dict_m02 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m02_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  })
  
  date <- lapply(numeric, function(df) {
    m02_dd_date <- data_dict_m02 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m02_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m02 = date
  
  # Add form number prefix to each variable (ex. "MNH00_")  
  m02_rbind <- lapply(m02, function(x) x %>% 
                        #add  "M##_"
                        select(SCRNID, MOMID, PREGID, everything()) %>% 
                        relocate(c(MOMID,PREGID), .after = SCRNID) %>% 
                        rename_with( ~ paste0("M02_", .), -c(1:3)) %>% 
                        replace_with_na(replace = list(MOMID = c("n/a"),
                                                       PREGID = c("n/a"))) %>% 
                        group_by(SCRNID, MOMID, PREGID) %>% 
                        arrange(desc(M02_SCRN_OBSSTDAT)) %>% 
                        slice(1) %>% 
                        ungroup() %>% 
                        distinct()
  )
  
  # Add "SITE" variable to the beginning of data 
  for(i in names(m02_rbind)){
    m02_rbind[[i]]$SITE <- paste(gsub("mnh02_","",i))
  }
  # Extract list of all variable names (will use this to match the order of varnames in each dataset)
  allNms <- unique(unlist(lapply(m02_rbind, names)))
  
  # Merge all MNH02 forms together 
  m02_merged <- do.call(rbind,c(lapply(m02_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # Make SITE the first variable 
  m02_merged <- m02_merged %>% relocate(SITE)
  
  # Extract missing varnames from site data
  m02_missing <- lapply(m02_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m02_duplicates <- m02_merged %>% 
    group_by(SITE, SCRNID, MOMID, PREGID) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH02") %>% 
    select(FORM, SITE, SCRNID, MOMID, PREGID, M02_SCRN_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m02_merged <- m02_merged %>% anti_join(m02_duplicates, by = c("SITE","SCRNID", "MOMID", "PREGID"))
  
  # Export data in .RData format
  save(m02_merged, file= paste0(path_to_save, "m02_merged",".RData",sep = ""))
  
}


#******M03 

# get list of all the MNH03 forms 
for (x in site_vec) {
  if (exists(paste("mnh03_", x, sep = ""))==TRUE){
    
    m03 <- mget(ls(pattern = "mnh03_.*"))
    
  }
}

if (exists("m03") == TRUE) {
  # create a list of data frame names as string
  m03_names = as.vector(names(m03))
  
  # assign field types
  data_dict_m03 <- data_dict %>% filter(Form == "MNH03") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m03, function(df) {
    m03_dd_numeric <- data_dict_m03 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m03_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  })
  
  date <- lapply(numeric, function(df) {
    m03_dd_date <- data_dict_m03 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m03_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m03 = date
  
  # remove duplicates and add prefix  
  m03_rbind <- lapply(m03, function(x) x %>% 
                        #add  "M##_"
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M03_", .), -c(1:2)) %>% 
                        #remove previous case for duplicates
                        group_by(MOMID, PREGID) %>% 
                        arrange(desc(M03_SD_OBSSTDAT)) %>% 
                        slice(1) %>% 
                        ungroup() %>% 
                        distinct() 
  )
  
  # add in site variable 
  for(i in names(m03_rbind)){
    m03_rbind[[i]]$SITE <- paste(gsub("mnh03_","",i))
  }
  # get all variable names
  allNms <- unique(unlist(lapply(m03_rbind, names)))
  
  # merge all MNH03 forms together 
  m03_merged <- do.call(rbind,c(lapply(m03_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m03_merged <- m03_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m03_missing <- lapply(m03_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m03_duplicates <- m03_merged %>% 
    group_by(SITE, MOMID, PREGID) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH03") %>% 
    select(FORM, SITE, MOMID, PREGID, M03_SD_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m03_merged <- m03_merged %>% anti_join(m03_duplicates, by = c("SITE", "MOMID", "PREGID"))
  
  
  ## export data 
  save(m03_merged, file= paste0(path_to_save, "m03_merged",".RData",sep = ""))
  
}

#******M04 

# get list of all the MNH04 forms 
for (x in site_vec) {
  if (exists(paste("mnh04_", x, sep = ""))==TRUE){
    
    m04 <- mget(ls(pattern = "mnh04_.*"))
    
  }
}

if (exists("m04") == TRUE) {
  # create a list of data frame names as string
  m04_names = as.vector(names(m04))
  
  # assign field types
  data_dict_m04 <- data_dict %>% filter(Form == "MNH04") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m04, function(df) {
    m04_dd_numeric <- data_dict_m04 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m04_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  ## pakistan had weird dates for tetanus -- remove columns 
  #numeric <- lapply(numeric, function(x) { x["TETANUS_CMSTDAT"] <- NULL; x })
  
  date <- lapply(numeric, function(df) {
    m04_dd_date <- data_dict_m04 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m04_dd_date
    #date <- lapply(df, function(x) x %>% select(matches(str_c(paste("^",m04_dd_date,"$", sep = "")))))
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m04 = date
  
  
  # remove duplicates and add prefix  
  m04_rbind <- lapply(m04, function(x) x %>% 
                        #add  "M##_"
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M04_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m04_rbind)){
    m04_rbind[[i]]$SITE <- paste(gsub("mnh04_","",i))
    
  }
  # get all variable names
  allNms <- unique(unlist(lapply(m04_rbind, names)))
  
  # merge all MNH04 forms together 
  m04_merged <- do.call(rbind,c(lapply(m04_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m04_merged <- m04_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m04_missing <- lapply(m04_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m04_duplicates <- m04_merged %>% 
    group_by(SITE, MOMID, PREGID, M04_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M04_TYPE_VISIT != 13) %>% 
    mutate(FORM = "MNH04") %>% 
    select(FORM, SITE, MOMID, PREGID, M04_TYPE_VISIT, M04_ANC_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m04_merged <- m04_merged %>% anti_join(m04_duplicates, by = c("SITE", "MOMID", "PREGID", "M04_TYPE_VISIT"))
  
  ## export data 
  save(m04_merged, file= paste0(path_to_save, "m04_merged",".RData",sep = ""))
  
}

#******M05 

# get list of all the MNH05 forms 
for (x in site_vec) {
  if (exists(paste("mnh05_", x, sep = ""))==TRUE){
    
    m05 <- mget(ls(pattern = "mnh05_.*"))
    
  }
}

if (exists("m05") == TRUE) {
  # create a list of data frame names as string
  m05_names = as.vector(names(m05))
  
  # assign field types
  data_dict_m05 <- data_dict %>% filter(Form == "MNH05") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m05, function(df) {
    m05_dd_numeric <- data_dict_m05 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m05_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m05_dd_date <- data_dict_m05 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m05_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m05 = date
  
  # remove duplicates and add prefix  
  m05_rbind <- lapply(m05, function(x) x %>% 
                        #add  "M##_"
                        select(MOMID, PREGID,  everything()) %>% 
                        rename_with( ~ paste0("M05_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m05_rbind)){
    m05_rbind[[i]]$SITE <- paste(gsub("mnh05_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m05_rbind, names)))
  
  # merge all MNH05 forms together 
  m05_merged <- do.call(rbind,c(lapply(m05_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m05_merged <- m05_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m05_missing <- lapply(m05_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m05_duplicates <- m05_merged %>% 
    group_by(SITE, MOMID, PREGID, M05_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M05_TYPE_VISIT != 13) %>% 
    mutate(FORM = "MNH05") %>% 
    select(FORM, SITE, MOMID, PREGID, M05_TYPE_VISIT, M05_ANT_PEDAT)
  
  # Extract duplicates from merged data 
  m05_merged <- m05_merged %>% anti_join(m05_duplicates, by = c("SITE", "MOMID", "PREGID", "M05_TYPE_VISIT"))
  
  ## export data 
  save(m05_merged, file= paste0(path_to_save, "m05_merged",".RData",sep = ""))
  
}


#******M06 

# get list of all the MNH06 forms 
for (x in site_vec) {
  if (exists(paste("mnh06_", x, sep = ""))==TRUE){
    
    m06 <- mget(ls(pattern = "mnh06_.*"))
    
  }
}

if (exists("m06") == TRUE) {
  # create a list of data frame names as string
  m06_names = as.vector(names(m06))
  
  # assign field types
  data_dict_m06 <- data_dict %>% filter(Form == "MNH06") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m06, function(df) {
    m06_dd_numeric <- data_dict_m06 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m06_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m06_dd_date <- data_dict_m06 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m06_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m06 = date
  
  # remove duplicates and add prefix  
  m06_rbind <- lapply(m06, function(x) x %>% 
                        #add  "M##_"
                       #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M06_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m06_rbind)){
    m06_rbind[[i]]$SITE <- paste(gsub("mnh06_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m06_rbind, names)))
  
  # merge all MNH06 forms together 
  m06_merged <- do.call(rbind,c(lapply(m06_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m06_merged <- m06_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m06_missing <- lapply(m06_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m06_duplicates <- m06_merged %>% 
    group_by(SITE, MOMID, PREGID, M06_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M06_TYPE_VISIT != 13,  M06_TYPE_VISIT != 14) %>% 
    mutate(FORM = "MNH06") %>% 
    select(FORM, SITE, MOMID, PREGID, M06_TYPE_VISIT, M06_DIAG_VSDAT)
  
  # Extract duplicates from merged data 
  m06_merged <- m06_merged %>% anti_join(m06_duplicates, by = c("SITE", "MOMID", "PREGID", "M06_TYPE_VISIT"))
  
  ## export data 
  save(m06_merged, file= paste0(path_to_save, "m06_merged",".RData",sep = ""))
  
}

#******M07 

# get list of all the MNH07 forms 
for (x in site_vec) {
  if (exists(paste("mnh07_", x, sep = ""))==TRUE){
    
    m07 <- mget(ls(pattern = "mnh07_.*"))
    
  }
}


if (exists("m07") == TRUE) {
  # create a list of data frame names as string
  m07_names = as.vector(names(m07))
  
  # assign field types
  data_dict_m07 <- data_dict %>% filter(Form == "MNH07") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m07, function(df) {
    m07_dd_numeric <- data_dict_m07 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m07_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m07_dd_date <- data_dict_m07 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m07_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m07 = date
  
  # remove duplicates and add prefix  
  m07_rbind <- lapply(m07, function(x) x %>% 
                        #add  "M##_"
                       #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M07_", .), -c(1:2)) %>% 
                        #remove previous case for duplicates
                        #group_by(MOMID, PREGID, M07_TYPE_VISIT) %>% ## UPDATED 06/12
                        #arrange(desc(M07_MAT_SPEC_COLLECT_DAT)) %>% 
                        #slice(1) %>% 
                        ungroup() %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m07_rbind)){
    m07_rbind[[i]]$SITE <- paste(gsub("mnh07_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m07_rbind, names)))
  
  # merge all MNH07 forms together 
  m07_merged <- do.call(rbind,c(lapply(m07_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m07_merged <- m07_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m07_missing <- lapply(m07_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m07_duplicates <- m07_merged %>% 
    group_by(SITE, MOMID, PREGID, M07_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M07_TYPE_VISIT != 13,  M07_TYPE_VISIT != 14) %>% 
    mutate(FORM = "MNH07") %>% 
    select(FORM, SITE, MOMID, PREGID, M07_TYPE_VISIT, M07_MAT_SPEC_COLLECT_DAT)
  
  # Extract duplicates from merged data 
  m07_merged <- m07_merged %>% anti_join(m07_duplicates, by = c("SITE", "MOMID", "PREGID", "M07_TYPE_VISIT"))
  
  ## export data 
  save(m07_merged, file= paste0(path_to_save, "m07_merged",".RData",sep = ""))
  
}

#******M08 

# get list of all the MNH08 forms 
for (x in site_vec) {
  if (exists(paste("mnh08_", x, sep = ""))==TRUE){
    
    m08 <- mget(ls(pattern = "mnh08_.*"))
    
  }
}

if (exists("m08") == TRUE) {
  # create a list of data frame names as string
  m08_names = as.vector(names(m08))
  
  # assign field types
  data_dict_m08 <- data_dict %>% filter(Form == "MNH08") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m08, function(df) {
    m08_dd_numeric <- data_dict_m08 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m08_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m08_dd_date <- data_dict_m08 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m08_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m08 = date
  
  # remove duplicates and add prefix  
  m08_rbind <- lapply(m08, function(x) x %>% 
                        #add  "M##_"
                       #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M08_", .), -c(1:2)) %>% 
                        #remove previous case for duplicates
                        # group_by(MOMID, PREGID, M08_TYPE_VISIT) %>% 
                        #arrange(desc(M08_LBSTDAT)) %>% 
                        #slice(1) %>% 
                        ungroup() %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m08_rbind)){
    m08_rbind[[i]]$SITE <- paste(gsub("mnh08_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m08_rbind, names)))
  
  # merge all MNH08 forms together 
  m08_merged <- do.call(rbind,c(lapply(m08_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m08_merged <- m08_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m08_missing <- lapply(m08_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m08_duplicates <- m08_merged %>% 
    group_by(SITE, MOMID, PREGID, M08_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M08_TYPE_VISIT != 13,  M08_TYPE_VISIT != 14) %>% 
    mutate(FORM = "MNH08") %>% 
    select(FORM, SITE, MOMID, PREGID, M08_TYPE_VISIT, M08_LBSTDAT)
  
  # Extract duplicates from merged data 
  m08_merged <- m08_merged %>% anti_join(m08_duplicates, by = c("SITE", "MOMID", "PREGID", "M08_TYPE_VISIT"))
  
  ## export data 
  save(m08_merged, file= paste0(path_to_save, "m08_merged",".RData",sep = ""))
  
}

#******M09 
# get list of all the MNH09 forms 
for (x in site_vec) {
  if (exists(paste("mnh09_", x, sep = ""))==TRUE){
    
    m09 <- mget(ls(pattern = "mnh09_.*"))
    
  }
}

if (exists("m09") == TRUE) {
  # create a list of data frame names as string
  m09_names = as.vector(names(m09))
  
  # assign field types
  data_dict_m09 <- data_dict %>% filter(Form == "MNH09") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m09, function(df) {
    m09_dd_numeric <- data_dict_m09 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m09_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m09_dd_date <- data_dict_m09 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m09_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m09 = date
  
  # remove duplicates and add prefix  
  m09_rbind <- lapply(m09, function(x) x %>% 
                        #add  "M##_"
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M09_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m09_rbind)){
    m09_rbind[[i]]$SITE <- paste(gsub("mnh09_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m09_rbind, names)))
  
  # merge all MNH09 forms together 
  m09_merged <- do.call(rbind,c(lapply(m09_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable
  m09_merged <- m09_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m09_missing <- lapply(m09_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m09_duplicates <- m09_merged %>% 
    group_by(SITE, MOMID, PREGID) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH09", M09_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID,M09_TYPE_VISIT, M09_MAT_LD_OHOSTDAT)
  
  # Extract duplicates from merged data 
  m09_merged <- m09_merged %>% anti_join(m09_duplicates, by = c("SITE", "MOMID", "PREGID"))
  
  ## export data 
  save(m09_merged, file= paste0(path_to_save, "m09_merged",".RData",sep = ""))
  
}

#******M10 

# get list of all the MNH10 forms 
for (x in site_vec) {
  if (exists(paste("mnh10_", x, sep = ""))==TRUE){
    
    m10 <- mget(ls(pattern = "mnh10_.*"))
    
  }
}

if (exists("m10") == TRUE) {
  # create a list of data frame names as string
  m10_names = as.vector(names(m10))
  
  ## get list of data that have duplicates in m10 
  dup_m10 <- lapply(m10, function(x) x[duplicated(x[c("MOMID","PREGID", "FORMCOMPLDAT_MNH10")]),] )
  
  # assign field types
  data_dict_m10 <- data_dict %>% filter(Form == "MNH10") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m10, function(df) {
    m10_dd_numeric <- data_dict_m10 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m10_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m10_dd_date <- data_dict_m10 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m10_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m10 = date
  
  # remove duplicates and add prefix  
  m10_rbind <- lapply(m10, function(x) x %>% 
                        #add  "M##_"
                        #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M10_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m10_rbind)){
    m10_rbind[[i]]$SITE <- paste(gsub("mnh10_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m10_rbind, names)))
  
  # merge all MNH10 forms together 
  m10_merged <- do.call(rbind,c(lapply(m10_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m10_merged <- m10_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m10_missing <- lapply(m10_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m10_duplicates <- m10_merged %>% 
    group_by(SITE, MOMID, PREGID) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH10", M10_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID, M10_TYPE_VISIT, M10_VISIT_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m10_merged <- m10_merged %>% anti_join(m10_duplicates, by = c("SITE", "MOMID", "PREGID"))
  
  ## export data 
  save(m10_merged, file= paste0(path_to_save, "m10_merged",".RData",sep = ""))
  
}

#******M11 

# get list of all the MNH11 forms 

for (x in site_vec) {
  if (exists(paste("mnh11_", x, sep = ""))==TRUE){
    
    m11 <- mget(ls(pattern = "mnh11_.*"))
    
  }
}

if (exists("m11") == TRUE) {
  # create a list of data frame names as string
  m11_names = as.vector(names(m11))
  
  # assign field types
  data_dict_m11 <- data_dict %>% filter(Form == "MNH11") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m11, function(df) {
    m11_dd_numeric <- data_dict_m11 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m11_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m11_dd_date <- data_dict_m11 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m11_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m11 = date
  
  # remove duplicates and add prefix  
  m11_rbind <- lapply(m11, function(x) x %>% 
                        #add  "M##_"
                        #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID, INFANTID, everything()) %>% 
                        rename_with( ~ paste0("M11_", .), -c(1:3)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m11_rbind)){
    m11_rbind[[i]]$SITE <- paste(gsub("mnh11_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m11_rbind, names)))
  
  # merge all MNH11 forms together 
  m11_merged <- do.call(rbind,c(lapply(m11_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m11_merged <- m11_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m11_missing <- lapply(m11_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m11_duplicates <- m11_merged %>% 
    group_by(SITE, MOMID, PREGID, INFANTID) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH11", M11_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID, M11_TYPE_VISIT, M11_VISIT_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m11_merged <- m11_merged %>% anti_join(m11_duplicates, by = c("SITE", "MOMID", "PREGID", "INFANTID"))
  
  ## export data 
  save(m11_merged, file= paste0(path_to_save, "m11_merged",".RData",sep = ""))
  
}

#******M12 

# get list of all the MNH12 forms 
for (x in site_vec) {
  if (exists(paste("mnh12_", x, sep = ""))==TRUE){
    
    m12 <- mget(ls(pattern = "mnh12_.*"))
    
  }
}

if (exists("m12") == TRUE) {
  # create a list of data frame names as string
  m12_names = as.vector(names(m12))
  
  # assign field types
  data_dict_m12 <- data_dict %>% filter(Form == "MNH12") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m12, function(df) {
    m12_dd_numeric <- data_dict_m12 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m12_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m12_dd_date <- data_dict_m12 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m12_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m12 = date
  
  # remove duplicates and add prefix  
  m12_rbind <- lapply(m12, function(x) x %>% 
                        #add  "M##_"
                       #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M12_", .), -c(1:2)) %>% 
                        distinct()
                      # group_by(MOMID, PREGID, M12_PNC_N_VISIT) %>% 
                      # arrange(desc(M12_VISIT_OBSSTDAT)) %>% 
                      # slice(1) %>% 
                      # ungroup() %>% 
                      # distinct()
  )
  
  # add in site variable 
  for(i in names(m12_rbind)){
    m12_rbind[[i]]$SITE <- paste(gsub("mnh12_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m12_rbind, names)))
  
  # merge all MNH12 forms together 
  m12_merged <- do.call(rbind,c(lapply(m12_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m12_merged <- m12_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m12_missing <- lapply(m12_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m12_duplicates <- m12_merged %>% 
    group_by(SITE, MOMID, PREGID, M12_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M12_TYPE_VISIT != 13,  M12_TYPE_VISIT != 14) %>% 
    mutate(FORM = "MNH12") %>% 
    select(FORM, SITE, MOMID, PREGID, M12_TYPE_VISIT, M12_VISIT_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m12_merged <- m12_merged %>% anti_join(m12_duplicates, by = c("SITE", "MOMID", "PREGID", "M12_TYPE_VISIT"))
  
  
  ## export data 
  save(m12_merged, file= paste0(path_to_save, "m12_merged",".RData",sep = ""))
  
}

#******M13
#* INFANT FORM - INFANTID

# get list of all the MNH13 forms
for (x in site_vec) {
  if (exists(paste("mnh13_", x, sep = ""))==TRUE){
    
    m13 <- mget(ls(pattern = "mnh13_.*"))
    
  }
}

if (exists("m13") == TRUE) {
  # create a list of data frame names as string
  m13_names = as.vector(names(m13))
  
  # assign field types
  data_dict_m13 <- data_dict %>% filter(Form == "MNH13") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m13, function(df) {
    m13_dd_numeric <- data_dict_m13 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m13_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  })
  
  date <- lapply(numeric, function(df) {
    m13_dd_date <- data_dict_m13 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m13_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m13 = date
  
  # remove duplicates and add prefix
  m13_rbind <- lapply(m13, function(x) x %>%
                        #add  "M##_"
                        #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID, INFANTID, everything()) %>%
                        rename_with( ~ paste0("M13_", .), -c(1:3)) %>%
                        # group_by(MOMID, PREGID, INFANTID, M13_PNC_N_VISIT) %>%
                        # arrange(desc(M13_VISIT_OBSSTDAT)) %>%
                        # slice(1) %>%
                        ungroup() %>%
                        distinct()
  )
  
  # add in site variable
  for(i in names(m13_rbind)){
    m13_rbind[[i]]$SITE <- paste(gsub("mnh13_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m13_rbind, names)))
  
  # merge all MNH13 forms together
  m13_merged <- do.call(rbind,c(lapply(m13_rbind,function(x)
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable
  m13_infantmerged <- m13_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m13_missing <- lapply(m13_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m13_duplicates <- m13_merged %>% 
    group_by(SITE, MOMID, PREGID,INFANTID, M13_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M13_TYPE_VISIT != 13,  M13_TYPE_VISIT != 14) %>% 
    mutate(FORM = "MNH13") %>% 
    select(FORM, SITE, MOMID, PREGID, INFANTID, M13_TYPE_VISIT, M13_VISIT_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m13_infantmerged <- m13_infantmerged %>% anti_join(m13_duplicates, by = c("SITE", "MOMID", "PREGID","INFANTID", "M13_TYPE_VISIT"))
  
  ## export data
  save(m13_infantmerged, file= paste0(path_to_save, "m13_infantmerged",".RData",sep = ""))
  
}


#******M14
#* INFANT FORM - INFANTID

# get list of all the MNH14 forms
for (x in site_vec) {
  if (exists(paste("mnh14_", x, sep = ""))==TRUE){
    
    m14 <- mget(ls(pattern = "mnh14_.*"))
    
  }
}

if (exists("m14") == TRUE) {
  # create a list of data frame names as string
  m14_names = as.vector(names(m14))
  
  # assign field types
  data_dict_m14 <- data_dict %>% filter(Form == "MNH14") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m14, function(df) {
    m14_dd_numeric <- data_dict_m14 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m14_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  })
  
  date <- lapply(numeric, function(df) {
    m14_dd_date <- data_dict_m14 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m14_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m14 = date
  
  # remove duplicates and add prefix
  m14_rbind <- lapply(m14, function(x) x %>%
                        #add  "M##_"
                        #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID,INFANTID, everything()) %>%
                        rename_with( ~ paste0("M14_", .), -c(1:3)) %>%
                        # group_by(MOMID, PREGID,INFANTID, M14_POC_VISIT) %>%
                        # arrange(desc(M14_VISIT_OBSSTDAT)) %>%
                        # slice(1) %>%
                        ungroup() %>%
                        distinct()
  )
  
  # add in site variable
  for(i in names(m14_rbind)){
    m14_rbind[[i]]$SITE <- paste(gsub("mnh14_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m14_rbind, names)))
  
  # merge all MNH14 forms together
  m14_merged <- do.call(rbind,c(lapply(m14_rbind,function(x)
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable
  m14_infantmerged <- m14_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m14_missing <- lapply(m14_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m14_duplicates <- m14_merged %>% 
    group_by(SITE, MOMID, PREGID,INFANTID, M14_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M14_TYPE_VISIT != 13,  M14_TYPE_VISIT != 14) %>% 
    mutate(FORM = "MNH14") %>% 
    select(FORM, SITE, MOMID, PREGID, INFANTID, M14_TYPE_VISIT, M14_VISIT_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m14_infantmerged <- m14_infantmerged %>% anti_join(m14_duplicates, by = c("SITE", "MOMID", "PREGID","INFANTID", "M14_TYPE_VISIT"))
  
  ## export data
  save(m14_infantmerged, file= paste0(path_to_save, "m14_infantmerged",".RData",sep = ""))
  
}

#******M15
#* INFANT FORM - INFANTID

# get list of all the MNH15 forms
for (x in site_vec) {
  if (exists(paste("mnh15_", x, sep = ""))==TRUE){
    
    m15 <- mget(ls(pattern = "mnh15_.*"))
    
  }
}

if (exists("m15") == TRUE) {
  # create a list of data frame names as string
  m15_names = as.vector(names(m15))
  
  # assign field types
  data_dict_m15 <- data_dict %>% filter(Form == "MNH15") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m15, function(df) {
    m15_dd_numeric <- data_dict_m15 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m15_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  })
  
  date <- lapply(numeric, function(df) {
    m15_dd_date <- data_dict_m15 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m15_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m15 = date
  
  # remove duplicates and add prefix
  m15_rbind <- lapply(m15, function(x) x %>%
                        #add  "M##_"
                       #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID,INFANTID, everything()) %>%
                        rename_with( ~ paste0("M15_", .), -c(1:3)) %>%
                        # group_by(MOMID, PREGID,INFANTID, M15_OBSTERM) %>%
                        # arrange(desc(M15_OBSSTDAT)) %>%
                        # slice(1) %>%
                        ungroup() %>%
                        distinct()
  )
  
  # add in site variable
  for(i in names(m15_rbind)){
    m15_rbind[[i]]$SITE <- paste(gsub("mnh15_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m15_rbind, names)))
  
  # merge all MNH15 forms together
  m15_merged <- do.call(rbind,c(lapply(m15_rbind,function(x)
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable
  m15_infantmerged <- m15_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m15_missing <- lapply(m15_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m15_duplicates <- m15_merged %>% 
    group_by(SITE, MOMID, PREGID,INFANTID, M15_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M15_TYPE_VISIT != 13,  M15_TYPE_VISIT != 14) %>% 
    mutate(FORM = "MNH15") %>% 
    select(FORM, SITE, MOMID, PREGID, INFANTID, M15_TYPE_VISIT, M15_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m15_infantmerged <- m15_infantmerged %>% anti_join(m15_duplicates, by = c("SITE", "MOMID", "PREGID","INFANTID", "M15_TYPE_VISIT"))
  
  ## export data
  save(m15_infantmerged, file= paste0(path_to_save, "m15_infantmerged",".RData",sep = ""))
  
}

#******M16

# get list of all the MNH16 forms 
for (x in site_vec) {
  if (exists(paste("mnh16_", x, sep = ""))==TRUE){
    
    m16 <- mget(ls(pattern = "mnh16_.*"))
    
  }
}

if (exists("m16") == TRUE) {
  # create a list of data frame names as string
  m16_names = as.vector(names(m16))
  
  # assign field types
  data_dict_m16 <- data_dict %>% filter(Form == "MNH16") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m16, function(df) {
    m16_dd_numeric <- data_dict_m16 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m16_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m16_dd_date <- data_dict_m16 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m16_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m16 = date
  
  # remove duplicates and add prefix  
  m16_rbind <- lapply(m16, function(x) x %>% 
                        #add  "M##_"
                        #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M16_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m16_rbind)){
    m16_rbind[[i]]$SITE <- paste(gsub("mnh16_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m16_rbind, names)))
  
  # merge all MNH16 forms together 
  m16_merged <- do.call(rbind,c(lapply(m16_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m16_merged <- m16_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m16_missing <- lapply(m16_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m16_duplicates <- m16_merged %>% 
    group_by(SITE, MOMID, PREGID) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH16", M16_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID, M16_TYPE_VISIT, M16_VISDAT)
  
  # Extract duplicates from merged data 
  m16_merged <- m16_merged %>% anti_join(m16_duplicates, by = c("SITE", "MOMID", "PREGID"))
  
  ## export data 
  save(m16_merged, file= paste0(path_to_save, "m16_merged",".RData",sep = ""))
  
}

#******M17

# get list of all the MNH17 forms 
for (x in site_vec) {
  if (exists(paste("mnh17_", x, sep = ""))==TRUE){
    
    m17 <- mget(ls(pattern = "mnh17_.*"))
    
  }
}

if (exists("m17") == TRUE) {
  # create a list of data frame names as string
  m17_names = as.vector(names(m17))
  
  # assign field types
  data_dict_m17 <- data_dict %>% filter(Form == "MNH17") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m17, function(df) {
    m17_dd_numeric <- data_dict_m17 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m17_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m17_dd_date <- data_dict_m17 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m17_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m17 = date
  
  # remove duplicates and add prefix  
  m17_rbind <- lapply(m17, function(x) x %>% 
                        #add  "M##_"
                        #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M17_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m17_rbind)){
    m17_rbind[[i]]$SITE <- paste(gsub("mnh17_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m17_rbind, names)))
  
  # merge all MNH17 forms together 
  m17_merged <- do.call(rbind,c(lapply(m17_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m17_merged <- m17_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m17_missing <- lapply(m17_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m17_duplicates <- m17_merged %>% 
    group_by(SITE, MOMID, PREGID) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH17", M17_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID, M17_TYPE_VISIT, M17_VISDAT)
  
  # Extract duplicates from merged data 
  m17_merged <- m17_merged %>% anti_join(m17_duplicates, by = c("SITE", "MOMID", "PREGID"))
  
  
  ## export data 
  save(m17_merged, file= paste0(path_to_save, "m17_merged",".RData",sep = ""))
  
}


#******M18

# get list of all the MNH18 forms 
for (x in site_vec) {
  if (exists(paste("mnh18_", x, sep = ""))==TRUE){
    
    m18 <- mget(ls(pattern = "mnh18_.*"))
    
  }
}

if (exists("m18") == TRUE) {
  # create a list of data frame names as string
  m18_names = as.vector(names(m18))
  
  # assign field types
  data_dict_m18 <- data_dict %>% filter(Form == "MNH18") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m18, function(df) {
    m18_dd_numeric <- data_dict_m18 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m18_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m18_dd_date <- data_dict_m18 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m18_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m18 = date
  
  # remove duplicates and add prefix  
  m18_rbind <- lapply(m18, function(x) x %>% 
                        #add  "M##_"
                        #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M18_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m18_rbind)){
    m18_rbind[[i]]$SITE <- paste(gsub("mnh18_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m18_rbind, names)))
  
  # merge all MNH18 forms together 
  m18_merged <- do.call(rbind,c(lapply(m18_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m18_merged <- m18_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m18_missing <- lapply(m18_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m18_duplicates <- m18_merged %>% 
    group_by(SITE, MOMID, PREGID) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH18", M18_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID, M18_TYPE_VISIT, M18_VISDAT)
  
  # Extract duplicates from merged data 
  m18_merged <- m18_merged %>% anti_join(m18_duplicates, by = c("SITE", "MOMID", "PREGID"))
  
  ## export data 
  save(m18_merged, file= paste0(path_to_save, "m18_merged",".RData",sep = ""))
  
}


#******M19

# get list of all the MNH19 forms 
for (x in site_vec) {
  if (exists(paste("mnh19_", x, sep = ""))==TRUE){
    
    m19 <- mget(ls(pattern = "mnh19_.*"))
    
  }
}

if (exists("m19") == TRUE) {
  # create a list of data frame names as string
  m19_names = as.vector(names(m19))
  
  # assign field types
  data_dict_m19 <- data_dict %>% filter(Form == "MNH19") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m19, function(df) {
    m19_dd_numeric <- data_dict_m19 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m19_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m19_dd_date <- data_dict_m19 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m19_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m19 = date
  
  # remove duplicates and add prefix  
  m19_rbind <- lapply(m19, function(x) x %>% 
                        #add  "M##_"
                        #filter(TYPE_VISIT != 13,  TYPE_VISIT != 14) %>% 
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M19_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m19_rbind)){
    m19_rbind[[i]]$SITE <- paste(gsub("mnh19_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m19_rbind, names)))
  
  # merge all MNH19 forms together 
  m19_merged <- do.call(rbind,c(lapply(m19_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m19_merged <- m19_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m19_missing <- lapply(m19_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m19_duplicates <- m19_merged %>% 
    group_by(SITE, MOMID, PREGID, M19_OBSSTDAT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH19", M19_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID, M19_TYPE_VISIT, M19_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m19_merged <- m19_merged %>% anti_join(m19_duplicates, by = c("SITE", "MOMID", "PREGID", "M19_OBSSTDAT"))
  
  ## export data 
  save(m19_merged, file= paste0(path_to_save, "m19_merged",".RData",sep = ""))
  
}

#******M20
#* INFANT FORM - INFANTID

# get list of all the MNH20 forms
for (x in site_vec) {
  if (exists(paste("mnh20_", x, sep = ""))==TRUE){
    
    m20 <- mget(ls(pattern = "mnh20_.*"))
    
  }
}

if (exists("m20") == TRUE) {
  # create a list of data frame names as string
  m20_names = as.vector(names(m20))
  
  # assign field types
  data_dict_m20 <- data_dict %>% filter(Form == "MNH20") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m20, function(df) {
    m20_dd_numeric <- data_dict_m20 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m20_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  })
  
  date <- lapply(numeric, function(df) {
    m20_dd_date <- data_dict_m20 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m20_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m20 = date
  
  # remove duplicates and add prefix
  m20_rbind <- lapply(m20, function(x) x %>%
                        #add  "M##_"
                        select(INFANTID, everything()) %>%
                        rename_with( ~ paste0("M20_", .), -c(1:3)) %>%
                        group_by(INFANTID) %>%
                        arrange(desc(M20_OBSSTDAT)) %>%
                        slice(1) %>%
                        ungroup() %>%
                        distinct()
  )
  
  # add in site variable
  for(i in names(m20_rbind)){
    m20_rbind[[i]]$SITE <- paste(gsub("mnh20_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m20_rbind, names)))
  
  # merge all MNH20 forms together
  m20_merged <- do.call(rbind,c(lapply(m20_rbind,function(x)
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable
  m20_infantmerged <- m20_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m20_missing <- lapply(m20_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m20_duplicates <- m20_merged %>% 
    group_by(SITE, MOMID, PREGID, INFANTID, M20_OBSSTDAT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH20", M20_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID, INFANTID, M20_TYPE_VISIT, M20_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m20_merged <- m20_merged %>% anti_join(m20_duplicates, by = c("SITE", "MOMID", "PREGID", "INFANTID", "M20_OBSSTDAT"))
  
  ## export data
  save(m20_infantmerged, file= paste0(path_to_save, "m20_infantmerged",".RData",sep = ""))
  
}




#******M21
# get list of all the MNH21 forms 
for (x in site_vec) {
  if (exists(paste("mnh21_", x, sep = ""))==TRUE){
    
    m21 <- mget(ls(pattern = "mnh21_.*"))
    
  }
}

if (exists("m21") == TRUE) {
  # create a list of data frame names as string
  m21_names = as.vector(names(m21))
  
  # assign field types
  data_dict_m21 <- data_dict %>% filter(Form == "MNH21") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m21, function(df) {
    m21_dd_numeric <- data_dict_m21 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m21_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m21_dd_date <- data_dict_m21 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m21_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m21 = date
  
  # remove duplicates and add prefix  
  m21_rbind <- lapply(m21, function(x) x %>% 
                        #add  "M##_"
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M21_", .), -c(1:3)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m21_rbind)){
    m21_rbind[[i]]$SITE <- paste(gsub("mnh21_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m21_rbind, names)))
  
  # merge all MNH21 forms together 
  m21_merged <- do.call(rbind,c(lapply(m21_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m21_merged <- m21_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m21_missing <- lapply(m21_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m21_duplicates <- m21_merged %>% 
    group_by(SITE, MOMID, PREGID, INFANTID, M21_AESTDAT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH21", M21_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID, INFANTID, M21_TYPE_VISIT, M21_AESTDAT)
  
  # Extract duplicates from merged data 
  m21_merged <- m21_merged %>% anti_join(m21_duplicates, by = c("SITE", "MOMID", "PREGID", "INFANTID", "M21_AESTDAT"))
  
  
  ## export data 
  save(m21_merged, file= paste0(path_to_save, "m21_merged",".RData",sep = ""))
  
}

#******M22
#* INFANT FORM - INFANTID

# get list of all the MNH22 forms
for (x in site_vec) {
  if (exists(paste("mnh22_", x, sep = ""))==TRUE){
    
    m22 <- mget(ls(pattern = "mnh22_.*"))
    
  }
}

if (exists("m22") == TRUE) {
  # create a list of data frame names as string
  m22_names = as.vector(names(m22))
  
  # assign field types
  data_dict_m22 <- data_dict %>% filter(Form == "MNH22") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m22, function(df) {
    m22_dd_numeric <- data_dict_m22 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m22_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  })
  
  date <- lapply(numeric, function(df) {
    m22_dd_date <- data_dict_m22 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m22_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m22 = date
  
  # remove duplicates and add prefix
  m22_rbind <- lapply(m22, function(x) x %>%
                        #add  "M##_"
                        select(INFANTID, MOMID, PREGID, everything()) %>%
                        rename_with( ~ paste0("M22_", .), -c(1:3)) %>%
                        # group_by(INFANTID) %>%
                        # arrange(desc(M22_DVSTDAT)) %>%
                        # slice(1) %>%
                        ungroup() %>%
                        distinct()
  )
  
  # add in site variable
  for(i in names(m22_rbind)){
    m22_rbind[[i]]$SITE <- paste(gsub("mnh22_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m22_rbind, names)))
  
  # merge all MNH22 forms together
  m22_merged <- do.call(rbind,c(lapply(m22_rbind,function(x)
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable
  m22_infantmerged <- m22_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m22_missing <- lapply(m22_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m22_duplicates <- m22_merged %>% 
    group_by(SITE, MOMID, PREGID, INFANTID, M22_DVSTDAT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH22", M22_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID, INFANTID, M22_TYPE_VISIT, M22_DVSTDAT)
  
  # Extract duplicates from merged data 
  m22_merged <- m22_merged %>% anti_join(m22_duplicates, by = c("SITE", "MOMID", "PREGID", "INFANTID", "M22_DVSTDAT"))
  
  ## export data
  save(m22_merged, file= paste0(path_to_save, "m22_infantmerged",".RData",sep = ""))
  
}

#******M23
# get list of all the MNH23 forms 
for (x in site_vec) {
  if (exists(paste("mnh23_", x, sep = ""))==TRUE){
    
    m23 <- mget(ls(pattern = "mnh23_.*"))
    
  }
}

if (exists("m23") == TRUE) {
  # create a list of data frame names as string
  m23_names = as.vector(names(m23))
  
  # assign field types
  data_dict_m23 <- data_dict %>% filter(Form == "MNH23") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m23, function(df) {
    m23_dd_numeric <- data_dict_m23 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m23_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m23_dd_date <- data_dict_m23 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m23_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m23 = date
  
  # remove duplicates and add prefix  
  m23_rbind <- lapply(m23, function(x) x %>% 
                        #add  "M##_"
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M23_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m23_rbind)){
    m23_rbind[[i]]$SITE <- paste(gsub("mnh23_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m23_rbind, names)))
  
  # merge all MNH23 forms together 
  m23_merged <- do.call(rbind,c(lapply(m23_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m23_merged <- m23_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m23_missing <- lapply(m23_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m23_duplicates <- m23_merged %>% 
    group_by(SITE, MOMID, PREGID) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH23", M23_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID, M23_TYPE_VISIT, M23_CLOSE_DSSTDAT)
  
  # Extract duplicates from merged data 
  m23_merged <- m23_merged %>% anti_join(m23_duplicates, by = c("SITE", "MOMID", "PREGID"))
  
  ## export data 
  save(m23_merged, file= paste0(path_to_save, "m23_merged",".RData",sep = ""))
  
}

#******M24
# #* INFANT FORM - INFANTID
# get list of all the MNH24 forms
for (x in site_vec) {
  if (exists(paste("mnh24_", x, sep = ""))==TRUE){
    
    m24 <- mget(ls(pattern = "mnh24_.*"))
    
  }
}

if (exists("m24") == TRUE) {
  # create a list of data frame names as string
  m24_names = as.vector(names(m24))
  
  # assign field types
  data_dict_m24 <- data_dict %>% filter(Form == "MNH24") %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m24, function(df) {
    m24_dd_numeric <- data_dict_m24 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
    num <- colnames(df) %in% m24_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  })
  
  date <- lapply(numeric, function(df) {
    m24_dd_date <- data_dict_m24 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
    date <- colnames(df) %in% m24_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  
  m24 = date
  
  # remove duplicates and add prefix
  m24_rbind <- lapply(m24, function(x) x %>%
                        #add  "M##_"
                        select(INFANTID, everything()) %>%
                        rename_with( ~ paste0("M24_", .), -c(1:3)) %>%
                        # group_by(INFANTID) %>%
                        # arrange(desc(M24_CLOSE_DSSTDAT)) %>%
                        # slice(1) %>%
                        ungroup() %>%
                        distinct()
  )
  
  # add in site variable
  for(i in names(m24_rbind)){
    m24_rbind[[i]]$SITE <- paste(gsub("mnh24_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m24_rbind, names)))
  
  # merge all MNH24 forms together
  m24_merged <- do.call(rbind,c(lapply(m24_rbind,function(x)
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable
  m24_infantmerged <- m24_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m24_missing <- lapply(m24_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m24_duplicates <- m24_merged %>% 
    group_by(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    mutate(FORM = "MNH24", M24_TYPE_VISIT = NA) %>% 
    select(FORM, SITE, MOMID, PREGID, INFANTID, M24_TYPE_VISIT, M24_CLOSE_DSSTDAT)
  
  # Extract duplicates from merged data 
  m24_merged <- m24_merged %>% anti_join(m24_duplicates, by = c("SITE", "MOMID", "PREGID","INFANTID", "M24_CLOSE_DSSTDAT"))
  
  ## export data
  save(m24_infantmerged, file= paste0(path_to_save, "m24_infantmerged",".RData",sep = ""))
  
}

#******M25
# get list of all the MNH25 forms 
for (x in site_vec) {
  if (exists(paste("mnh25_", x, sep = ""))==TRUE){
    
    m25 <- mget(ls(pattern = "mnh25_.*"))
    
  }
}

if (exists("m25") == TRUE) {
  # create a list of data frame names as string
  m25_names = as.vector(names(m25))
  
  # assign field types
  MNH25_forms = c("MNH25_Ghana", "MNH25_Kenya", "MNH25_Pakistan", "MNH25_India","MNH25_Zambia")
  data_dict_m25 <- data_dict %>% filter(Form %in% MNH25_forms) %>% select(`Variable Name`, FieldType)
  
  numeric <- lapply(m25, function(df) {
    m25_dd_numeric <- data_dict_m25 %>% filter(FieldType == "Number") %>% distinct(`Variable Name`) %>% pull(`Variable Name`)
    num <- colnames(df) %in% m25_dd_numeric
    df[num] <- lapply(df[num], as.numeric)
    df
  }) 
  
  date <- lapply(numeric, function(df) {
    m25_dd_date <- data_dict_m25 %>% filter(FieldType == "Date")%>% distinct(`Variable Name`) %>% pull(`Variable Name`)
    date <- colnames(df) %in% m25_dd_date
    df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
    df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
    df[date] <- lapply(df[date], ymd)
    df
  })
  
  m25 = date
  
  # remove duplicates and add prefix  
  m25_rbind <- lapply(m25, function(x) x %>% 
                        #add  "M##_"
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M25_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m25_rbind)){
    m25_rbind[[i]]$SITE <- paste(gsub("mnh25_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m25_rbind, names)))
  
  # merge all MNH25 forms together 
  m25_merged <- do.call(rbind,c(lapply(m25_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m25_merged <- m25_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m25_missing <- lapply(m25_rbind, function(x) setdiff(allNms, colnames(x)))
  
  # Extract duplicates into their own dataset
  m25_duplicates <- m25_merged %>% 
    group_by(SITE, MOMID, PREGID, M25_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M25_TYPE_VISIT!=13, M25_TYPE_VISIT!=14) %>% 
    mutate(FORM = "MNH25") %>% 
    select(FORM, SITE, MOMID, PREGID, M25_TYPE_VISIT, M25_OBSSTDAT)
  
  # Extract duplicates from merged data 
  m25_merged <- m25_merged %>% anti_join(m25_duplicates, by = c("SITE", "MOMID", "PREGID","M25_TYPE_VISIT"))
  
  
  ## export data 
  save(m25_merged, file= paste0(path_to_save, "m25_merged",".RData",sep = ""))
  
}



#******M26
# get list of all the MNH26 forms 
for (x in site_vec) {
  if (exists(paste("mnh26_", x, sep = ""))==TRUE){
    
    m26 <- mget(ls(pattern = "mnh26_.*"))
    
  }
}

if (exists("m26") == TRUE) {
  # create a list of data frame names as string
  m26_names = as.vector(names(m26))
  
  # assign field types
  #MNH26_forms = c("MNH26_Ghana", "MNH26_Kenya", "MNH26_Pakistan", "MNH26_India","MNH26_Zambia")
  #data_dict_m26 <- data_dict %>% filter(Form %in% MNH26_forms) %>% select(`Variable Name`, FieldType)
  # 
  # numeric <- lapply(m26, function(df) {
  #   m26_dd_numeric <- data_dict_m26 %>% filter(FieldType == "Number") %>% pull(`Variable Name`)
  #   num <- colnames(df) %in% m26_dd_numeric
  #   df[num] <- lapply(df[num], as.numeric)
  #   df
  # }) 
  
  # date <- lapply(numeric, function(df) {
  #   m26_dd_date <- data_dict_m26 %>% filter(FieldType == "Date") %>% pull(`Variable Name`)
  #   date <- colnames(df) %in% m26_dd_date
  #   df[date] <- lapply(df[date], parse_date_time, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))
  # df[date] <- lapply(df[date], function(x) gsub("2007-07-07", ymd("1907-07-07"), x, fixed=TRUE)) ## 09/05 updates for ke data
  # df[date] <- lapply(df[date], ymd)
  #   df
  # })
  # 
  m26 = m26 ## to fix once all sites have harmonized m26
  
  # remove duplicates and add prefix  
  m26_rbind <- lapply(m26, function(x) x %>% 
                        #add  "M##_"
                        select(MOMID, PREGID, everything()) %>% 
                        rename_with( ~ paste0("M26_", .), -c(1:2)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m26_rbind)){
    m26_rbind[[i]]$SITE <- paste(gsub("mnh26_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m26_rbind, names)))
  
  # merge all MNH26 forms together 
  m26_merged <- do.call(rbind,c(lapply(m26_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m26_merged <- m26_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m26_missing <- lapply(m26_rbind, function(x) setdiff(allNms, colnames(x)))
  
  
  # Extract duplicates into their own dataset
  m26_duplicates <- m26_merged %>% 
    group_by(SITE, MOMID, PREGID, M26_TYPE_VISIT) %>% 
    mutate(n = n()) %>% 
    filter(n > 1, M26_TYPE_VISIT!=13, M26_TYPE_VISIT!=14) %>% 
    mutate(FORM = "MNH26") %>% 
    select(FORM, SITE, MOMID, PREGID, M26_TYPE_VISIT, M26_FTGE_OBSTDAT)
  
  # Extract duplicates from merged data 
  m26_merged <- m26_merged %>% anti_join(m26_duplicates, by = c("SITE", "MOMID", "PREGID","M26_TYPE_VISIT"))
  
  
  ## export data 
  save(m26_merged, file= paste0(path_to_save, "m26_merged",".RData",sep = ""))
  
}

#******M28
# get list of all the MNH26 forms 
for (x in site_vec) {
  if (exists(paste("mnh28_", x, sep = ""))==TRUE){
    
    m28 <- mget(ls(pattern = "mnh28_.*"))
    
  }
}
if (exists("m28") == TRUE) {
  # create a list of data frame names as string
  m28_names = as.vector(names(m28))
  
  # remove duplicates and add prefix  
  m28_rbind <- lapply(m28, function(x) x %>% 
                        #add  "M##_"
                        ## only pull subset of data 
                        select(MOMID, PREGID,INFANTID, ID10104, ID10105,
                               ID10109, ID10110, ID10114, ID10115, ID10116) %>% 
                        select(MOMID, PREGID, INFANTID, everything()) %>% 
                        rename_with( ~ paste0("M28_", .), -c(1:3)) %>% 
                        distinct()
  )
  
  # add in site variable 
  for(i in names(m28_rbind)){
    m28_rbind[[i]]$SITE <- paste(gsub("mnh28_","",i))
  }
  
  # get all variable names
  allNms <- unique(unlist(lapply(m28_rbind, names)))
  
  # merge all MNH26 forms together 
  m28_merged <- do.call(rbind,c(lapply(m28_rbind,function(x) 
    data.frame(c(x, sapply(setdiff(allNms, names(x)),
                           function(y) NA)))), make.row.names=FALSE))
  
  # make site the first variable 
  m28_merged <- m28_merged %>% relocate(SITE)
  
  ## get the variables that are missing from sites
  m28_missing <- lapply(m28_rbind, function(x) setdiff(allNms, colnames(x)))
  
  ## export data 
  save(m28_merged, file= paste0(path_to_save, "m28_merged",".RData",sep = ""))
  
}



## get table by form - this will show which sites have imported each form 
form_site <- mget(ls(pattern = "_names*"))

# get list of all missing variables - this will show all the missing variables by form and site 
varname_missing <- mget(ls(pattern = "._missing*"))

varname_duplicates <- mget(ls(pattern = "_duplicates*"))


# get list of all merged data 
#mat_data_merged <- mget(ls(pattern = "._merged*")) 
#inf_data_merged <- mget(ls(pattern = "._infantmerged*")) 

## look at all ANC visits by site 
table(m04_merged$SITE, m04_merged$M04_TYPE_VISIT)

## look at all PNC visits by site 
table(m12_merged$SITE, m12_merged$M12_TYPE_VISIT)

## look at all PNC visits by site 
table(m13_merged$SITE, m13_merged$M13_TYPE_VISIT)

## look at all m25 visits by site 
table(m25_merged$SITE, m25_merged$M25_TYPE_VISIT)

table(m05_merged$SITE, m05_merged$M05_TYPE_VISIT)
table(m26_merged$SITE, m26_merged$M26_TYPE_VISIT)

#*****************************************************************************
m13_merged = m13_infantmerged
m14_merged = m14_infantmerged
m15_merged = m15_infantmerged
m20_merged = m20_infantmerged
m24_merged = m24_infantmerged

# ## export as .CSV into network drive
# 1. place all merged datasets into list
files_list <- mget(ls(pattern = "_merged"))

# 2. rename
names(files_list) = str_replace(names(files_list),  "m", "mnh")

# 3. set working directory
# first need to make subfolder with upload date
maindir <- paste0("~/Monitoring Report/data/merged/", sep = "")
subdir = UploadDate
dir.create(file.path(maindir, subdir), showWarnings = FALSE)

setwd(paste0("~/Monitoring Report/data/merged/" ,UploadDate))

# 4. export to csv
lapply(1:length(files_list), function(i) write.csv(files_list[[i]],
                                                   file = paste0(names(files_list[i]), ".csv"),
                                                   row.names = FALSE))
