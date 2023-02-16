#*****************************************************************************
#*Read raw data
#*Clean data for monitoring report
#*1. temp remove duplicates
#*2. add prefix "M##_" to var names 
#*3. add "SITE" var
#*****************************************************************************

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

setwd("~/Documents/GWU/Report/2023-02-03") #change folder name every time

#************************PAKISTAN************************
PA_mnh00_raw<-read_csv("PA/mnh00.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh01_raw<-read_csv("PA/mnh01.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh02_raw<-read_csv("PA/mnh02.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh03_raw<-read_csv("PA/mnh03.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh04_raw<-read_csv("PA/mnh04.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh05_raw<-read_csv("PA/mnh05.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh06_raw<-read_csv("PA/mnh06.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh07_raw<-read_csv("PA/mnh07.csv", col_names=TRUE, show_col_types = FALSE)
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
KE_mnh09_raw<-read_csv("KE/MNH09.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh10_raw<-read_csv("KE/MNH10.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh11_raw<-read_csv("KE/MNH11.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh12_raw<-read_csv("KE/MNH12.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh17_raw<-read_csv("KE/MNH17.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh19_raw<-read_csv("KE/MNH19.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh21_raw<-read_csv("KE/MNH21.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh25_raw<-read_csv("KE/MNH25.csv", col_names=TRUE, show_col_types = FALSE)

#************************Zambia************************
ZA_mnh00_raw<-read_csv("ZA/mnh00.csv", col_names=TRUE, show_col_types = FALSE)
ZA_mnh01_raw<-read_csv("ZA/mnh01.csv", col_names=TRUE, show_col_types = FALSE)
ZA_mnh02_raw<-read_csv("ZA/mnh02.csv", col_names=TRUE, show_col_types = FALSE)
ZA_mnh03_raw<-read_csv("ZA/mnh03.csv", col_names=TRUE, show_col_types = FALSE) 
ZA_mnh05_raw<-read_csv("ZA/MNH05.csv", col_names=TRUE, show_col_types = FALSE)
ZA_mnh07_raw<-read_csv("ZA/MNH07.csv", col_names=TRUE, show_col_types = FALSE)
ZA_mnh08_raw<-read_csv("ZA/MNH08.csv", col_names=TRUE, show_col_types = FALSE)

#************************Ghana************************
GH_mnh00_raw<-read_csv("GH/mnh00.csv", col_names=TRUE, show_col_types = FALSE)
GH_mnh01_raw<-read_csv("GH/mnh01.csv", col_names=TRUE, show_col_types = FALSE)
GH_mnh02_raw<-read_csv("GH/mnh02.csv", col_names=TRUE, show_col_types = FALSE)
GH_mnh03_raw<-read_csv("GH/mnh03.csv", col_names=TRUE, show_col_types = FALSE) 

#*****************************************************************************
#*clean data 
#*1. temporarilly remove duplicates (removing older case)
#*2. add prefix "M##_" to var names 
#*3. add "SITE" var
#*****************************************************************************

#************************PAKISTAN************************
#******M00
#************new upload date
#list duplicated ID
dup00 <- PA_mnh00_raw[duplicated(PA_mnh00_raw[c("SCRNID")]) | duplicated(PA_mnh00_raw[c("SCRNID")],fromLast = TRUE),] 

PA_mnh00 <- PA_mnh00_raw %>% 
#add site and "M##_"
  mutate(SITE="Pakistan") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M00_", .), -c(1:4)) %>% 
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>% 
  distinct()

#******M01
dup01 <- PA_mnh01_raw[duplicated(PA_mnh01_raw[c("SCRNID","US_OHOSTDAT")]) | duplicated(PA_mnh01_raw[c("SCRNID","US_OHOSTDAT")],fromLast = TRUE),] 

PA_mnh01 <- PA_mnh01_raw %>% 
  #add site and M##_
  mutate(SITE="Pakistan") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M01_", .), -c(1:4)) %>% 
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>% 
  distinct()

#******M02 
dup02 <- PA_mnh02_raw[duplicated(PA_mnh02_raw[c("SCRNID")]) | duplicated(PA_mnh02_raw[c("SCRNID")],fromLast = TRUE),] 

PA_mnh02 <- PA_mnh02_raw %>% 
  #add site and M##_
  mutate(SITE="Pakistan") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M02_", .), -c(1:4)) %>% 
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>% 
  group_by(SCRNID, MOMID, PREGID, SITE) %>% 
  arrange(desc(dmy(M02_SCRN_OBSSTDAT))) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct()

#******M03
dup03 <- PA_mnh03_raw[duplicated(PA_mnh03_raw[c("MOMID","PREGID")]) | duplicated(PA_mnh03_raw[c("MOMID","PREGID")],fromLast = TRUE),] 

PA_mnh03 <- PA_mnh03_raw %>% 
  rename_with (toupper) %>% 
  #add site and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M03_", .), -c(1:3)) %>% 
  #remove previous case for duplicates
  group_by(MOMID, PREGID, SITE) %>% 
  arrange(desc(dmy(M03_SD_OBSSTDAT))) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct() 

#******M04
dup04 <- PA_mnh04_raw[duplicated(PA_mnh04_raw[c("MOMID","PREGID", "TYPE_VISIT"
                                                )]),] 

#add SITE var and M##_
PA_mnh04 <- PA_mnh04_raw %>% 
  #add site and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M04_", .), -c(1:3)) %>% 
  distinct()

#******M05
dup05 <- PA_mnh05_raw[duplicated(PA_mnh05_raw[c("MOMID","PREGID", "TYPE_VISIT"
                                                )]),] 

PA_mnh05 <- PA_mnh05_raw %>%  
  #add SITE var and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M05_", .), -c(1:3)) %>% 
  distinct()

#******M06
dup06 <- PA_mnh06_raw[duplicated(PA_mnh06_raw[c("MOMID","PREGID", "TYPE_VISIT"
                                                )]),] 

PA_mnh06 <- PA_mnh06_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M06_", .), -c(1:3)) %>% 
  distinct()

#******M07
dup07 <- PA_mnh07_raw[duplicated(PA_mnh07_raw[c("MOMID","PREGID","VISIT_LBSTDAT")]) | 
                        duplicated(PA_mnh07_raw[c("MOMID","PREGID","VISIT_LBSTDAT")],fromLast = TRUE),] 

PA_mnh07 <- PA_mnh07_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M07_", .), -c(1:3)) %>% 
  #remove previous case for duplicates
  group_by(MOMID, PREGID, SITE, M07_VISIT_LBSTDAT) %>% 
  arrange(desc(M07_MAT_SPEC_COLLECT_DAT)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct()

#******M08
dup08 <- PA_mnh08_raw[duplicated(PA_mnh08_raw[c("MOMID","PREGID", "VISIT_LBSTDAT"
                                                )]),] 

PA_mnh08 <- PA_mnh08_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M08_", .), -c(1:3)) %>% 
  #remove previous case for duplicates
  group_by(MOMID, PREGID, SITE, M08_VISIT_LBSTDAT) %>% 
  arrange(desc(M08_LBSTDAT)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct()

#******M25
dup25 <- PA_mnh25_raw[duplicated(PA_mnh25_raw[c("MOMID","PREGID", "ANC_VISIT_N")]),] 

PA_mnh25 <- PA_mnh25_raw %>% 
  # add SITE and M##_
  mutate(SITE="Pakistan") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M25_", .), -c(1:3)) %>% 
  distinct()

#******M26
dup26 <- PA_mnh26_raw[duplicated(PA_mnh26_raw[c("MOMID","PREGID", "FTGE_OBSTDAT")]),] 

# add SITE and M##_
PA_mnh26 <- PA_mnh26_raw %>% 
  rename_with (toupper) %>% 
  mutate(SITE="Pakistan") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M26_", .), -c(1:4)) %>% 
  distinct()

#************************Kenya************************
#************new upload date

#******M00
#list duplicated ID
dup00 <- KE_mnh00_raw[duplicated(KE_mnh00_raw[c("SCRNID"
                                              )]),]

KE_mnh00 <- KE_mnh00_raw %>% 
  #add site and "M##_"
  mutate(SITE="Kenya") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M00_", .), -c(1:4)) %>% 
  #replace n/a in MOMID and PREGID with NA, so info can be filled later
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>% 
  #remove previous case for duplicates
  group_by(SCRNID,SITE) %>% 
  arrange(desc(M00_SCRN_OBSSTDAT)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct()


#******M01
dup01 <- KE_mnh01_raw[duplicated(KE_mnh01_raw[c("SCRNID","US_OHOSTDAT")]),] 

KE_mnh01 <- KE_mnh01_raw %>% 
  #add site and M##_
  mutate(SITE = "Kenya") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M01_", .), -c(1:4)) %>% 
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>% 
  distinct()

#******M02 
dup02 <- KE_mnh02_raw[duplicated(KE_mnh02_raw[c("SCRNID")]),] 

KE_mnh02 <- KE_mnh02_raw %>% 
  mutate(SITE="Kenya") %>% 
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M02_", .), -c(1:4)) %>% 
  distinct()

#******M03
dup03 <- KE_mnh03_raw[duplicated(KE_mnh03_raw[c("MOMID","PREGID")]),] 

KE_mnh03 <- KE_mnh03_raw %>% 
  #rename var to match CRFs
  rename_with (toupper) %>% 
  #add site and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M03_", .), -c(1:3)) %>% 
  distinct() 

#******M04
dup04 <- KE_mnh04_raw[duplicated(KE_mnh04_raw[c("MOMID","PREGID", "TYPE_VISIT"
                                                )]),] 

#add SITE var and M##_
KE_mnh04 <- KE_mnh04_raw %>% 
  #add site and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M04_", .), -c(1:3)) %>% 
  distinct()

#******M05
dup05 <- KE_mnh05_raw[duplicated(KE_mnh05_raw[c("MOMID","PREGID", "TYPE_VISIT"
                                                )]),] 

KE_mnh05 <- KE_mnh05_raw %>%  
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M05_", .), -c(1:3)) %>% 
  distinct()

#******M06
dup06 <- KE_mnh06_raw[duplicated(KE_mnh06_raw[c("MOMID","PREGID", "TYPE_VISIT"
                                                )]),] 

KE_mnh06 <- KE_mnh06_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  # relocate(TYPE_VISIT, .after = "DIAG_OTHR_SPFY_OBSLOC") %>% 
  rename_with( ~ paste0("M06_", .), -c(1:3)) %>% 
  distinct()

#******M07
dup07 <- KE_mnh07_raw[duplicated(KE_mnh07_raw[c("MOMID","PREGID", "MAT_SPEC_COLLECT_VISIT"
                                                )]),] 

KE_mnh07 <- KE_mnh07_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M07_", .), -c(1:3)) %>% 
  distinct()

#******M08
dup08 <- KE_mnh08_raw[duplicated(KE_mnh08_raw[c("MOMID","PREGID", "VISIT_LBSTDAT", "LBSTDAT"
                                                )]),] 

KE_mnh08 <- KE_mnh08_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M08_", .), -c(1:3)) %>% 
  distinct()

#******M09
dup09 <- KE_mnh09_raw[duplicated(KE_mnh09_raw[c("MOMID","PREGID", "MAT_LD_OHOSTDAT")]),] 

KE_mnh09 <- KE_mnh09_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M09_", .), -c(1:3)) %>% 
  distinct()

#******M10
dup10 <- KE_mnh10_raw[duplicated(KE_mnh10_raw[c("MOMID","PREGID", "FORMCOMPLDAT_MNH10")]),] 

KE_mnh10 <- KE_mnh10_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M10_", .), -c(1:3)) %>% 
  distinct()

#******M11
dup11 <- KE_mnh11_raw[duplicated(KE_mnh11_raw[c("MOMID","PREGID", "INFANTID")]),] 

KE_mnh11 <- KE_mnh11_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, INFANTID, SITE, everything()) %>% 
  rename_with( ~ paste0("M11_", .), -c(1:4)) %>% 
  distinct()

#******M12
dup12 <- KE_mnh12_raw[duplicated(KE_mnh12_raw[c("MOMID","PREGID","PNC_N_VISIT")]) | 
                        duplicated(KE_mnh12_raw[c("MOMID","PREGID","PNC_N_VISIT")],fromLast = TRUE),] 

KE_mnh12 <- KE_mnh12_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M12_", .), -c(1:3)) %>% 
  group_by(MOMID, PREGID, M12_PNC_N_VISIT) %>% 
  arrange(desc(M12_VISIT_OBSSTDAT)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct()

#******M13
#* INFANTID

#******M14
#* INFANTID

#******M15
#* INFANTID

#******M16

#******M17
dup17 <- KE_mnh17_raw[duplicated(KE_mnh17_raw[c("MOMID","PREGID")]),] 

KE_mnh17 <- KE_mnh17_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M17_", .), -c(1:3)) %>% 
  distinct()

#******M18


#******M19
dup19 <- KE_mnh19_raw[duplicated(KE_mnh19_raw[c("MOMID","PREGID"
                                                )]),] 

KE_mnh19 <- KE_mnh19_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M19_", .), -c(1:3)) %>% 
  distinct()

#******M20
#* INFANTID
   


#******M21
dup21 <- KE_mnh21_raw[duplicated(KE_mnh21_raw[c("MOMID","PREGID", "INFANTID")]),] 

KE_mnh21 <- KE_mnh21_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, INFANTID, SITE, everything()) %>% 
  rename_with( ~ paste0("M21_", .), -c(1:4)) %>% 
  distinct()

#******M22
#* INFANTID

#******M23

#******M24
#* INFANTID

#******M25
dup25 <- KE_mnh25_raw[duplicated(KE_mnh25_raw[c("MOMID","PREGID", "ANC_VISIT_N"
                                                )]),] 

KE_mnh25 <- KE_mnh25_raw %>% 
  # add SITE and M##_
  mutate(SITE="Kenya") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M25_", .), -c(1:3)) %>% 
  distinct()

#************************Zambia************************
#************new date
#******M00
#list duplicated ID
dup00 <- ZA_mnh00_raw[duplicated(ZA_mnh00_raw[c("MOMID", "PREGID")]),] 

ZA_mnh00 <- ZA_mnh00_raw %>% 
  mutate(SITE="Zambia") %>%
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>%
  rename_with( ~ paste0("M00_", .), -c(1:4)) %>%
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>%
  distinct() 

#******M01
dup01 <- ZA_mnh01_raw[duplicated(ZA_mnh01_raw[c("MOMID","PREGID", "US_OHOSTDAT"
                                                )]),] 

ZA_mnh01 <- ZA_mnh01_raw %>%
  mutate(SITE="Zambia") %>%
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>%
  rename_with( ~ paste0("M01_", .), -c(1:4)) %>%
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>%
  #remove previous case for duplicates
  group_by(SCRNID,MOMID, PREGID,SITE) %>% 
  arrange(desc(M01_US_OHOSTDAT)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct()

#******M02
dup02 <- ZA_mnh02_raw[duplicated(ZA_mnh02_raw[c("MOMID","PREGID")]),] 

ZA_mnh02 <- ZA_mnh02_raw %>%
  mutate(SITE="Zambia") %>%
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>%
  rename_with( ~ paste0("M02_", .), -c(1:4)) %>%
  distinct()

#******M03
dup03 <- ZA_mnh03_raw[duplicated(ZA_mnh03_raw[c("MOMID","PREGID")]),]

ZA_mnh03 <- ZA_mnh03_raw %>%
  rename_with (toupper) %>%
  mutate(SITE="Zambia") %>%
  select(MOMID, PREGID, SITE, everything()) %>%
  rename_with( ~ paste0("M03_", .), -c(1:3)) %>%
  distinct()

#******M05
dup05 <- ZA_mnh05_raw[duplicated(ZA_mnh05_raw[c("MOMID","PREGID","TYPE_VISIT")]) | 
                        duplicated(ZA_mnh05_raw[c("MOMID","PREGID","TYPE_VISIT")],fromLast = TRUE),] 

ZA_mnh05 <- ZA_mnh05_raw %>%  
  #add SITE var and M##_
  mutate(SITE="Zambia") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M05_", .), -c(1:3)) %>% 
  group_by(MOMID,PREGID,M05_TYPE_VISIT) %>% 
  arrange(desc(M05_ANT_PEDAT)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct()

#******M06


#******M07
dup07 <- ZA_mnh07_raw[duplicated(ZA_mnh07_raw[c("MOMID","PREGID", "MAT_SPEC_COLLECT_VISIT"
)]),] 

ZA_mnh07 <- ZA_mnh07_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Zambia") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M07_", .), -c(1:3)) %>% 
  distinct()

#******M08
dup08 <- ZA_mnh08_raw[duplicated(ZA_mnh08_raw[c("MOMID","PREGID", "VISIT_LBSTDAT", "LBSTDAT"
)]),] 

ZA_mnh08 <- ZA_mnh08_raw %>% 
  #add SITE var and M##_
  mutate(SITE="Zambia") %>% 
  select(MOMID, PREGID, SITE, everything()) %>% 
  rename_with( ~ paste0("M08_", .), -c(1:3)) %>% 
  distinct()


#************************Ghana************************
#************Jan 27 data
#******M00
#list duplicated ID
dup00 <- GH_mnh00_raw[duplicated(GH_mnh00_raw[c("MOMID", "PREGID")]),] #change to SCRNID once data updated!!!!!!

GH_mnh00 <- GH_mnh00_raw %>%
  #add site and "M##_"
  mutate(SITE="Ghana") %>%
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>%
  rename_with( ~ paste0("M00_", .), -c(1:4)) %>%
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>%
  distinct()

#******M01
dup01 <- GH_mnh01_raw[duplicated(GH_mnh01_raw[c("MOMID","PREGID", "US_OHOSTDAT")]),] #change to SCRNID once data updated!!!!!!

GH_mnh01 <- GH_mnh01_raw %>%
  #add site and M##_
  mutate(SITE="Ghana") %>%
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>%
  rename_with( ~ paste0("M01_", .), -c(1:4)) %>%
  replace_with_na(replace = list(MOMID = c("n/a"),
                                 PREGID = c("n/a"))) %>%
  distinct()

#******M02
dup02 <- GH_mnh02_raw[duplicated(GH_mnh02_raw[c("MOMID","PREGID")]),] #change to SCRNID once data updated!!!!!!

GH_mnh02 <- GH_mnh02_raw %>%
  #add site and M##_
  mutate(SITE="Ghana") %>%
  select(SCRNID, MOMID, PREGID, SITE, everything()) %>%
  rename_with( ~ paste0("M02_", .), -c(1:4)) %>%
  distinct()

#******M03
dup03 <- GH_mnh03_raw[duplicated(GH_mnh03_raw[c("MOMID","PREGID")]),]

GH_mnh03 <- GH_mnh03_raw %>%
  rename_with (toupper) %>%
  #add site and M##_
  mutate(SITE="Ghana") %>%
  select(MOMID, PREGID, SITE, everything()) %>%
  rename_with( ~ paste0("M03_", .), -c(1:3)) %>%
  distinct()


