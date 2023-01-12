#*****************************************************************************
#*Read raw data
#*Clean data for monitoring report
#*1. temporarily remove duplicates
#*2. add prefix "M##_" to var names 
#*3. add "SITE" var
#*4. revise, add, remove vars to make sure vars in data are in align with CRFs (among sites)
#*5. extract MOMID and PREGID from mnh02 and add to mnh00 and mnh01, so all data can be linked together
#*Save data (CO_mnh##.csv)
#*****************************************************************************

rm(list = ls())
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(data.table)
library(lubridate)
library(naniar)

#*****************************************************************************
#*set directory and read data 
#*****************************************************************************

#************************PAKISTAN************************
setwd("~/Documents/GWU/Report/2023-01-06") #change folder name every time

PA_mnh00_raw<-read_csv("PA/mnh00.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh01_raw<-read_csv("PA/mnh01.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh02_raw<-read_csv("PA/mnh02.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh03_raw<-read_csv("PA/mnh03.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh04_raw<-read_csv("PA/mnh04.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh05_raw<-read_csv("PA/mnh05.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh06_raw<-read_csv("PA/mnh06.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh08_raw<-read_csv("PA/mnh08.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh25_raw<-read_csv("PA/mnh25.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh26_raw<-read_csv("PA/mnh26.csv", col_names=TRUE, show_col_types = FALSE)

#************************Kenya************************
KE_mnh00_raw<-read_csv("KE/MNH00.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh01_raw<-read_csv("KE/MNH01.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh02_raw<-read_csv("KE/MNH02.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh03_raw<-read_csv("KE/MNH03.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh04_raw<-read_csv("KE/MNH04.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh05_raw<-read_csv("KE/MNH05.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh06_raw<-read_csv("KE/MNH06.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh07_raw<-read_csv("KE/MNH07.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh08_raw<-read_csv("KE/MNH08.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh19_raw<-read_csv("KE/MNH19.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh25_raw<-read_csv("KE/MNH25.csv", col_names=TRUE, show_col_types = FALSE)


#*****************************************************************************
#*clean data 
#*1. temporarilly remove duplicates
#*2. add prefix "M##_" to var names 
#*3. add "SITE" var
#*4. revise, add, remove vars to make sure vars in data are in align with CRFs (among sites)
#*5. extract MOMID and PREGID from mnh02 and add to mnh00 and mnh01, so all data can be linked together
#*****************************************************************************

#************************PAKISTAN************************
#************Jan06 data
#******M00
#list duplicated ID
dup00 <- PA_mnh00_raw[duplicated(PA_mnh00_raw[c("SCRNID")]),] #, "SCRN_OBSSTDAT"

PA_mnh00 <- PA_mnh00_raw %>% 
#remove ALL duplicates (remove all)
  subset(!(SCRNID %in% dup00$SCRNID)) %>% 
#add site and "M##_"
  mutate(SITE="Pakistan") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M00_", .), -c(1:4)) %>% 
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>% 
  distinct()

#******M01
dup01 <- PA_mnh01_raw[duplicated(PA_mnh01_raw[c("SCRNID","US_OHOSTDAT")]),] 

PA_mnh01 <- PA_mnh01_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(SCRNID %in% dup01$SCRNID)) %>% 
  #add site and M##_
  mutate(SITE="Pakistan") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M01_", .), -c(1:4)) %>% 
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>% 
  distinct()

#******M02 
dup02 <- PA_mnh02_raw[duplicated(PA_mnh02_raw[c("SCRNID")]),] #,"SCRN_OBSSTDAT"

PA_mnh02 <- PA_mnh02_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(SCRNID %in% dup02$SCRNID)) %>% 
  #add site and M##_
  mutate(SITE="Pakistan") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M02_", .), -c(1:4)) %>% 
  distinct()

#******M03
dup03 <- PA_mnh03_raw[duplicated(PA_mnh03_raw[c("MOMID","PREGID")]),] 

PA_mnh03 <- PA_mnh03_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup03$MOMID) & !(PREGID %in% dup03$PREGID)) %>% 
  #rename var to match CRFs
  rename(STOVE_FUEL_SPFY_FCORRES_88 = STOVE_FUEL_SPFY_FCORRES) %>% 
  rename_with (toupper) %>% 
  #add site and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M03_", .), -c(1:3)) %>% 
  distinct() 

#******M04
dup04 <- PA_mnh04_raw[duplicated(PA_mnh04_raw[c("MOMID","PREGID", "TYPE_VISIT", "ANC_OBSSTDAT")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

#add SITE var and M##_
PA_mnh04 <- PA_mnh04_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup04$MOMID) & !(PREGID %in% dup04$PREGID)) %>% 
  #add site and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M04_", .), -c(1:3)) %>% 
  distinct()

#******M05
dup05 <- PA_mnh05_raw[duplicated(PA_mnh05_raw[c("MOMID","PREGID", "TYPE_VISIT", "ANT_PEDAT")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

PA_mnh05 <- PA_mnh05_raw %>%  
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup05$MOMID) & !(PREGID %in% dup05$PREGID)) %>% 
  #add SITE var and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M05_", .), -c(1:3)) %>% 
  distinct()

#******M06
dup06 <- PA_mnh06_raw[duplicated(PA_mnh06_raw[c("MOMID","PREGID", "TYPE_VISIT", "DIAG_VSDAT")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

PA_mnh06 <- PA_mnh06_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup06$MOMID) & !(PREGID %in% dup06$PREGID)) %>% 
  #add SITE var and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M06_", .), -c(1:3)) %>% 
  distinct()

#******M08
dup08 <- PA_mnh08_raw[duplicated(PA_mnh08_raw[c("MOMID","PREGID", "VISIT_LBSTDAT", "LBSTDAT")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

PA_mnh08 <- PA_mnh08_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup08$MOMID) & !(PREGID %in% dup08$PREGID)) %>% 
  #add SITE var and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M08_", .), -c(1:3)) %>% 
  distinct()

#******M25
dup25 <- PA_mnh25_raw[duplicated(PA_mnh25_raw[c("MOMID","PREGID", "ANC_VISIT_N")]),] #wait for decision on TYPE_VISIT == 13 multiple visits
# dup25 <- PA_mnh25_raw[duplicated(PA_mnh25_raw[c("MOMID", "ANC_VISIT_N")]),] 

PA_mnh25 <- PA_mnh25_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup25$MOMID)) %>% 
  # add SITE and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M25_", .), -c(1:3)) %>% 
  distinct()

#******M26
dup26 <- PA_mnh26_raw[duplicated(PA_mnh26_raw[c("MOMID","PREGID")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

# add SITE and M##_
PA_mnh26 <- PA_mnh26_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup26$MOMID)) %>% 
  rename_with (toupper) %>% 
  mutate(SITE="Pakistan") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M26_", .), -c(1:4)) %>% 
  distinct()

#************************Kenya************************
#************Dec22 data

#******M00
#list duplicated ID
dup00 <- KE_mnh00_raw[duplicated(KE_mnh00_raw[c("SCRNID")]),] #, "SCRN_OBSSTDAT"

KE_mnh00 <- KE_mnh00_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(SCRNID %in% dup00$SCRNID)) %>% 
  #add site and "M##_"
  mutate(SITE="Kenya") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M00_", .), -c(1:4)) %>% 
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>% 
  distinct()


#******M01
dup01 <- KE_mnh01_raw[duplicated(KE_mnh01_raw[c("SCRNID","US_OHOSTDAT")]),] 

KE_mnh01 <- KE_mnh01_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(SCRNID %in% dup01$SCRNID)) %>% 
  #add site and M##_
  mutate(SITE = "Kenya") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M01_", .), -c(1:4)) %>% 
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>% 
  distinct()

#******M02 
dup02 <- KE_mnh02_raw[duplicated(KE_mnh02_raw[c("SCRNID")]),] #,"SCRN_OBSSTDAT"

KE_mnh02 <- KE_mnh02_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(SCRNID %in% dup02$SCRNID)) %>% 
  mutate(SITE="Kenya") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M02_", .), -c(1:4)) %>% 
  distinct()

#******M03
dup03 <- KE_mnh03_raw[duplicated(KE_mnh03_raw[c("MOMID","PREGID")]),] 

KE_mnh03 <- KE_mnh03_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup03$MOMID) & !(PREGID %in% dup03$PREGID)) %>% 
  #rename var to match CRFs
  rename_with (toupper) %>% 
  select(-c(LAND_USE_FCORRES,
           STOVE_FUEL_FCORRES)) %>% 
  #add site and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M03_", .), -c(1:3)) %>% 
  distinct() 

#******M04
dup04 <- KE_mnh04_raw[duplicated(KE_mnh04_raw[c("MOMID","PREGID", "TYPE_VISIT", "ANC_OBSSTDAT")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

#add SITE var and M##_
KE_mnh04 <- KE_mnh04_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup04$MOMID) & !(PREGID %in% dup04$PREGID)) %>% 
  #add site and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M04_", .), -c(1:3)) %>% 
  distinct()

#******M05
dup05 <- KE_mnh05_raw[duplicated(KE_mnh05_raw[c("MOMID","PREGID", "TYPE_VISIT", "ANT_PEDAT")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

KE_mnh05 <- KE_mnh05_raw %>%  
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup05$MOMID) & !(PREGID %in% dup05$PREGID)) %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M05_", .), -c(1:3)) %>% 
  distinct()

#******M06
dup06 <- KE_mnh06_raw[duplicated(KE_mnh06_raw[c("MOMID","PREGID", "TYPE_VISIT", "DIAG_VSDAT")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

KE_mnh06 <- KE_mnh06_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup06$MOMID) & !(PREGID %in% dup06$PREGID)) %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M06_", .), -c(1:3)) %>% 
  distinct()

#******M07
dup07 <- KE_mnh07_raw[duplicated(KE_mnh07_raw[c("MOMID","PREGID", "MAT_SPEC_COLLECT_VISIT", "MAT_SPEC_COLLECT_DAT")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

KE_mnh07 <- KE_mnh07_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup07$MOMID) & !(PREGID %in% dup07$PREGID)) %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M07_", .), -c(1:3)) %>% 
  distinct()

#******M08
dup08 <- KE_mnh08_raw[duplicated(KE_mnh08_raw[c("MOMID","PREGID", "VISIT_LBSTDAT", "LBSTDAT")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

KE_mnh08 <- KE_mnh08_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup08$MOMID) & !(PREGID %in% dup08$PREGID)) %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M08_", .), -c(1:3)) %>% 
  distinct()

#******M19
dup19 <- KE_mnh19_raw[duplicated(KE_mnh19_raw[c("MOMID","PREGID", "OBSSTDAT")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

KE_mnh19 <- KE_mnh19_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup19$MOMID) & !(PREGID %in% dup19$PREGID)) %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M19_", .), -c(1:3)) %>% 
  distinct()

#******M25
dup25 <- KE_mnh25_raw[duplicated(KE_mnh25_raw[c("MOMID","PREGID", "ANC_VISIT_N")]),] #wait for decision on TYPE_VISIT == 13 multiple visits

KE_mnh25 <- KE_mnh25_raw %>% 
  #remove ALL duplicates (remove all)
  subset(!(MOMID %in% dup25$MOMID) & !(PREGID %in% dup25$PREGID))%>%
  # add SITE and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M25_", .), -c(1:3)) %>% 
  distinct()


#*****************************************************************************
#*Create empty data in order to merge (This part will be removed once we have all forms for each site)
#*****************************************************************************
#******M07
PA_mnh07 <- KE_mnh07[0,]
PA_mnh07[1,] <- NA
PA_mnh07$SITE = "Pakistan"

#******M19
PA_mnh19 <- KE_mnh19[0,]
PA_mnh19[1,] <- NA
PA_mnh19$SITE = "Pakistan"

#******M26
KE_mnh26 <- PA_mnh26[0,]
KE_mnh26[1,] <- NA
KE_mnh26$SITE = "Kenya"

#*****************************************************************************
#*Save clean data for later use
#*output: CO_mnh##.csv
#*****************************************************************************

#save data by form by site
write_csv(PA_mnh00, file = "cleaned_data/PA_mnh00.csv", col_names = TRUE, "")
write_csv(PA_mnh01, file = "cleaned_data/PA_mnh01.csv", col_names = TRUE, "")
write_csv(PA_mnh02, file = "cleaned_data/PA_mnh02.csv", col_names = TRUE, "")
write_csv(PA_mnh03, file = "cleaned_data/PA_mnh03.csv", col_names = TRUE, "")
write_csv(PA_mnh04, file = "cleaned_data/PA_mnh04.csv", col_names = TRUE, "")
write_csv(PA_mnh05, file = "cleaned_data/PA_mnh05.csv", col_names = TRUE, "")
write_csv(PA_mnh06, file = "cleaned_data/PA_mnh06.csv", col_names = TRUE, "")
write_csv(PA_mnh07, file = "cleaned_data/PA_mnh07.csv", col_names = TRUE, "")
write_csv(PA_mnh08, file = "cleaned_data/PA_mnh08.csv", col_names = TRUE, "")
write_csv(PA_mnh19, file = "cleaned_data/PA_mnh19.csv", col_names = TRUE, "")
write_csv(PA_mnh25, file = "cleaned_data/PA_mnh25.csv", col_names = TRUE, "")
write_csv(PA_mnh26, file = "cleaned_data/PA_mnh26.csv", col_names = TRUE, "")

write_csv(KE_mnh00, file = "cleaned_data/KE_mnh00.csv", col_names = TRUE, "")
write_csv(KE_mnh01, file = "cleaned_data/KE_mnh01.csv", col_names = TRUE, "")
write_csv(KE_mnh02, file = "cleaned_data/KE_mnh02.csv", col_names = TRUE, "")
write_csv(KE_mnh03, file = "cleaned_data/KE_mnh03.csv", col_names = TRUE, "")
write_csv(KE_mnh04, file = "cleaned_data/KE_mnh04.csv", col_names = TRUE, "")
write_csv(KE_mnh05, file = "cleaned_data/KE_mnh05.csv", col_names = TRUE, "")
write_csv(KE_mnh06, file = "cleaned_data/KE_mnh06.csv", col_names = TRUE, "")
write_csv(KE_mnh07, file = "cleaned_data/KE_mnh07.csv", col_names = TRUE, "")
write_csv(KE_mnh08, file = "cleaned_data/KE_mnh08.csv", col_names = TRUE, "")
write_csv(KE_mnh19, file = "cleaned_data/KE_mnh19.csv", col_names = TRUE, "")
write_csv(KE_mnh25, file = "cleaned_data/KE_mnh25.csv", col_names = TRUE, "")
write_csv(KE_mnh26, file = "cleaned_data/KE_mnh26.csv", col_names = TRUE, "")



