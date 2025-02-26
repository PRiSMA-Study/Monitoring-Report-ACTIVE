#*****************************************************************************
#* Monitoring report GSED SF/LF/FCI
#* Drafted: 4 September 2024, Stacie Loisate (Translating Nazia's Stata Code to R)
#* Last updated: 25 February 2025 -- underway 

#*****************************************************************************
#*****************************************************************************
#* Data Setup 
#*****************************************************************************
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)
library(openxlsx)

# UPDATE EACH RUN # 
# set upload date 
UploadDate = "2025-02-21"

# path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")
path_to_data <- paste0("D:/Users/stacie.loisate/Documents/import/")
path_to_save <- "D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/"
path_to_tnt <- "Z:/SynapseCSVs/"
mat_enroll <- read_xlsx(paste0("Z:/Outcome Data/", UploadDate, "/", "MAT_ENROLL.xlsx"))
load(paste0("D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/MatData_Pnc_Visits.RData"))
load(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, "/InfData_Wide_", UploadDate, ".RData"))
mnh24 <- read_csv(paste0(path_to_data, UploadDate, "/", "mnh24_merged.csv"))

## pull out PNC windows 
MatData_Pnc_Visits_to_merge <- MatData_Pnc_Visits %>% 
  filter(PREGID %in% as.vector(mat_enroll$PREGID)) %>%
  # generate variable for livebirth from MNH09 
  select(SITE, MOMID, PREGID, ENDPREG_DAYS, contains("_LATE")) %>% 
  filter(!is.na(ENDPREG_DAYS))

InfData_Wide_to_merge <- InfData_Wide %>% 
  select(SITE, MOMID, PREGID, INFANTID, M09_BIRTH_DSTERM,M09_DELIV_DSSTDAT, contains("_TYPE_VISIT_12"), contains("VISIT_COMPLETE_12")) %>% 
  mutate(DOB = ymd(parse_date_time(M09_DELIV_DSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))))

mnh24 <- mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSDECOD, M24_CLOSE_DSSTDAT)

#*****************************************************************************
#*****************************************************************************
# PRISMA/ReMAPP Cohort - GSED SF: import data & data cleaning -----
#*****************************************************************************
#*****************************************************************************
# Ghana
mnh30_gha <- read.csv(paste0(path_to_tnt,"Ghana/", UploadDate, "/", "mnh30.csv"))

colnames(mnh30_gha) = toupper(colnames(mnh30_gha))

mnh30_gha <- mnh30_gha %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30,TYPE_VISIT, CONFIRM_DATADOB) %>% 
  rename(CHILD_DOB = CONFIRM_DATADOB) %>% 
  
  ## convert to date class
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         CHILD_DOB = ymd(parse_date_time(CHILD_DOB, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  ) %>%
  ## calculate age in days at visit 
  mutate(AGEDAYS = as.numeric(VISIT_OBSSTDAT-CHILD_DOB)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Ghana") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30, TYPE_VISIT, CHILD_DOB, AGEDAYS)

# Zambia (reporting d/m/yyyy)
mnh30_zam <- read.csv(paste0(path_to_tnt,"Zambia/", UploadDate, "/", "mnh30.csv"))
# mnh30_zam <- read.csv(paste0(path_to_tnt,"Zambia/", "2024-10-04", "/", "mnh30.csv"))

colnames(mnh30_zam) = toupper(colnames(mnh30_zam))

mnh30_zam <- mnh30_zam %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH30, TYPE_VISIT, STUDY_ID, GENDER, CHILD_DOB) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, orders = c("%d/%m/%Y"))),
         # DATE_ASSESSMENT = ymd(parse_date_time(DATE_ASSESSMENT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         CHILD_DOB = suppressWarnings(as.Date(parse_date_time(CHILD_DOB, order = c("%Y-%m-%d %H:%M:%S"))))) %>%
  ## calculate age in days at visit 
  mutate(AGEDAYS = as.numeric(VISIT_OBSSTDAT-CHILD_DOB)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Zambia") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30,  TYPE_VISIT, CHILD_DOB, AGEDAYS)

# Kenya -- all DATE_ASSESSMENT and CHILD_DOB are day month year
mnh30_ken <- read.csv(paste0(path_to_tnt,"Kenya/", UploadDate, "/", "mnh30.csv"))
# mnh30_ken <- read.csv(paste0(path_to_tnt,"Kenya/", "2024-12-12", "/", "mnh30.csv"))

colnames(mnh30_ken) = toupper(colnames(mnh30_ken))

mnh30_ken <- mnh30_ken %>% 
  select(MOMID, PREGID, INFANTID, INF_VISIT_MNH30, TYPE_VISIT, STUDY_ID, GENDER, DATE_ASSESSMENT, CHILD_DOB, AGEDAYS) %>% 
  ## convert to date class
  mutate(DATE_ASSESSMENT = as.Date(parse_date_time(DATE_ASSESSMENT, order = c("%d/%m/%Y %H:%M"))),
         CHILD_DOB = as.Date(parse_date_time(CHILD_DOB, order = c("%d/%m/%Y %H:%M")))
  ) %>% 
  rename(VISIT_OBSSTDAT = DATE_ASSESSMENT
  ) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Kenya") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30,  TYPE_VISIT, CHILD_DOB, AGEDAYS)



# Pakistan
mnh30_pak <- read.csv(paste0(path_to_tnt,"Pakistan/", UploadDate, "/", "mnh30.csv"))
# mnh30_pak <- read.csv(paste0("Z:/2025-01-04_pak", "/", "mnh30.csv"))

colnames(mnh30_pak) = toupper(colnames(mnh30_pak))

mnh30_pak <- mnh30_pak %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30 , TYPE_VISIT, STUDY_ID, GENDER, CHILD_DOB, AGEDAYS) %>% 
  
  ## convert to date class
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%b %d, %Y"))),
         CHILD_DOB = as.Date(parse_date_time(CHILD_DOB, orders = c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M:%S",
                                                                   "%d-%m-%Y %H:%M:%S", "%d-%b-%y %H:%M:%S")))) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Pakistan") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30, TYPE_VISIT, CHILD_DOB, AGEDAYS)

# India-SAS
mnh30_sas <- read_csv(paste0(path_to_data, UploadDate, "_SAS", "/", "mnh30.csv"))

colnames(mnh30_sas) = toupper(colnames(mnh30_sas))

mnh30_sas <- mnh30_sas %>% 
  rename(VISIT_OBSSTDAT = DATE_ASSESSMENT) %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30 , 
         TYPE_VISIT, STUDY_ID, GENDER, CHILD_DOB, AGEDAYS) %>% 
  
  
  ## convert to date class
  mutate(VISIT_OBSSTDAT = as.Date(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y-%m-%d %H:%M:%S", "%d-%b-%y", "%b %d, %Y"))),
         CHILD_DOB = as.Date(parse_date_time(CHILD_DOB, orders = c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M:%S",
                                                                   "%d-%m-%Y %H:%M:%S", "%d-%b-%y %H:%M:%S")))) %>% 
  ## generate site indicator variable 
  mutate(SITE = "India-SAS") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30, TYPE_VISIT, CHILD_DOB, AGEDAYS)

# India-CMC
mnh30_cmc <- read_csv(paste0(path_to_data, UploadDate, "_cmc", "/", "mnh30.csv"))

colnames(mnh30_cmc) = toupper(colnames(mnh30_cmc))

mnh30_cmc <- mnh30_cmc %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30 , 
         TYPE_VISIT, STUDY_ID, GENDER, CHILD_DOB, AGEDAYS) %>% 
  
  ## convert to date class
  mutate(VISIT_OBSSTDAT = as.Date(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y-%m-%d %H:%M:%S", "%d-%b-%y", "%b %d, %Y"))),
         CHILD_DOB = as.Date(parse_date_time(CHILD_DOB, orders = c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M:%S",
                                                                   "%d-%m-%Y %H:%M:%S", "%d-%b-%y %H:%M:%S")))) %>% 
  ## generate site indicator variable 
  mutate(SITE = "India-CMC") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30, TYPE_VISIT, CHILD_DOB, AGEDAYS)

#*****************************************************************************
# GSED SF: Merge data & remove duplicates -----
#*****************************************************************************
mnh30_merged <- rbind(mnh30_gha,mnh30_cmc, mnh30_zam, mnh30_ken, mnh30_pak )  # mnh30_sas

# # extract duplicate ids from main dataset
mnh30_merged <- mnh30_merged %>%
  mutate(SF_TYPE_VISIT = TYPE_VISIT) %>% 
  mutate(SF_VISIT_DATE = VISIT_OBSSTDAT)

table(mnh30_merged$TYPE_VISIT, mnh30_merged$SITE)
#*****************************************************************************
#*****************************************************************************
# FCI: import and data cleaning ----
#*****************************************************************************
#*****************************************************************************
# Ghana
mnh31_gha <- read.csv(paste0(path_to_tnt,"Ghana/", UploadDate, "/", "mnh31.csv"))
colnames(mnh31_gha) = toupper(colnames(mnh31_gha))

mnh31_gha <- mnh31_gha %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,TYPE_VISIT, CONFIRM_DATADOB) %>% 
  rename(CHILD_DOB = CONFIRM_DATADOB) %>% 
  
  ## convert to date class
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         CHILD_DOB = ymd(parse_date_time(CHILD_DOB, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  ) %>%
  ## calculate age in days at visit 
  mutate(AGEDAYS = as.numeric(VISIT_OBSSTDAT-CHILD_DOB)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Ghana") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31, TYPE_VISIT) # , CHILD_DOB, AGEDAYS

# Zambia
mnh31_zam <- read.csv(paste0(path_to_tnt,"Zambia/", UploadDate, "/", "mnh31.csv"))

colnames(mnh31_zam) = toupper(colnames(mnh31_zam))

mnh31_zam <- mnh31_zam %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH31, TYPE_VISIT) %>% # , STUDY_ID, GENDER, CHILD_DOB
  ## convert to date class
  # mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%Y-%m-%d")))) %>% 
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y")))) %>% 
  
  ## generate site indicator variable 
  mutate(SITE = "Zambia") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,  TYPE_VISIT) # , CHILD_DOB, AGEDAYS


# Kenya -- all DATE_ASSESSMENT are ub dd-mmm-yy
mnh31_ken <- read.csv(paste0(path_to_tnt,"Kenya/", UploadDate, "/", "mnh31.csv"))
# mnh31_ken <- read.csv(paste0(path_to_data,"2025-02-07_ke", "/", "mnh31.csv"))

colnames(mnh31_ken) = toupper(colnames(mnh31_ken))

mnh31_ken <- mnh31_ken %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH31, TYPE_VISIT) %>% # , STUDY_ID, GENDER, DATE_ASSESSMENT, CHILD_DOB, AGEDAYS
  ## convert to date class
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d-%b-%y")))
  ) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Kenya") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,  TYPE_VISIT) # , CHILD_DOB, AGEDAYS

# India-CMC
mnh31_cmc <- read.csv(paste0(path_to_tnt,"India_CMC/", UploadDate, "/", "mnh31.csv"))

colnames(mnh31_cmc) = toupper(colnames(mnh31_cmc))

mnh31_cmc <- mnh31_cmc %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH31, TYPE_VISIT) %>% # , STUDY_ID, GENDER, DATE_ASSESSMENT, CHILD_DOB, AGEDAYS
  ## convert to date class
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d-%m-%Y")))
  ) %>% 
  ## generate site indicator variable 
  mutate(SITE = "India-CMC") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,  TYPE_VISIT) # , CHILD_DOB, AGEDAYS


# India-SAS
mnh31_sas <- read.csv(paste0(path_to_tnt,"India_SAS/", UploadDate, "/", "mnh31.csv"))

colnames(mnh31_sas) = toupper(colnames(mnh31_sas))

mnh31_sas <- mnh31_sas %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH31, TYPE_VISIT) %>% # , STUDY_ID, GENDER, DATE_ASSESSMENT, CHILD_DOB, AGEDAYS
  ## convert to date class
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%Y-%m-%d")))
  ) %>% 
  ## generate site indicator variable 
  mutate(SITE = "India-SAS") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,  TYPE_VISIT) # , CHILD_DOB, AGEDAYS

#*****************************************************************************
# FCI: merge data & remove duplicates -----
#*****************************************************************************
mnh31_merged <- rbind(mnh31_gha, mnh31_zam, mnh31_ken,mnh31_cmc, mnh31_sas)

# extract duplicate ids from main dataset
mnh31_merged <- mnh31_merged %>% 
  mutate(FCI_TYPE_VISIT = TYPE_VISIT) %>% 
  mutate(FCI_VISIT_DATE = VISIT_OBSSTDAT)

#*****************************************************************************
# PRISMA/ReMAPP Cohort Data processing for monitoring report tables -----

## Date expansion launch for GSED SF @ 12 months
# Ghana: 22-May-2024
# Kenya: 29-May-2024
# Pakistan: 30-Jan-2024
# Zambia: 20-Mar-2024
# SAS: unclear 

#*****************************************************************************
inf_date_sf <- InfData_Wide_to_merge %>% 
  select(-M09_DELIV_DSSTDAT) %>% 
  # only include sitest that we have data for 
  filter(SITE %in% c("Pakistan", "Kenya", "Ghana", "Zambia", "India-CMC", "India-SAS")) %>% 
  filter(!is.na(DOB))

gsed_sf_wide <- inf_date_sf %>% 
  # filter for livebirths 
  filter(M09_BIRTH_DSTERM==1) %>% 
  ## merge in infant date (birth outcome)
  left_join(mnh30_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))  %>% 
  mutate(SF_TYPE_VISIT = as.numeric(SF_TYPE_VISIT)) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID, SF_TYPE_VISIT) %>%
  # address any duplicates (if exist, take the first instance)
  arrange(-desc(VISIT_OBSSTDAT)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup() %>% 
  select(SITE, MOMID, PREGID, INFANTID, DOB, SF_TYPE_VISIT, SF_VISIT_DATE, INF_VISIT_MNH30) %>% 
  pivot_wider(
    names_from = SF_TYPE_VISIT,
    values_from = c(SF_TYPE_VISIT, SF_VISIT_DATE, INF_VISIT_MNH30),
    names_glue = "{.value}_{SF_TYPE_VISIT}"
  ) %>% 
  ## calculate start windows for 3mo, 6mo, and 12mo visit
  mutate(START_3MO = DOB + 91) %>% # 13 weeks
  mutate(START_6MO = DOB + 182) %>% # 26 weeks
  mutate(START_12MO = DOB + 364) %>% # 52 weeks
  ## calculate late windows for 3mo, 6mo, and 12mo visit
  mutate(LATE_3MO = DOB + 181) %>% # 25 weeks 6 days
  mutate(LATE_6MO = DOB + 363) %>% # 51 weeks 6 days
  mutate(LATE_12MO = DOB + 454) %>% # 64 weeks
  ## calculate on time windows for 3mo, 6mo, and 12mo visit
  mutate(ONTIME_3MO = DOB + 125) %>% # 17 weeks 6 days
  mutate(ONTIME_6MO = DOB + 216) %>% # 30 weeks 6 days
  mutate(ONTIME_12MO = DOB + 398) %>% # 56 weeks 6 days
  
  # select(SITE, MOMID, PREGID, INFANTID, contains("START"), contains("LATE")) %>% 
  # distinct() %>% 
  mutate(PASS_3MO = case_when(ymd(UploadDate) > ONTIME_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > ONTIME_6MO ~ 1, TRUE ~ 0),
  ) %>% 
  ## merge in closeout form 
  left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD) %>%  filter(INFANTID != "Z3-025-1136-B"), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_GSED_3MO_DENOM_SF = case_when(PASS_3MO == 1  & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_GSED_6MO_DENOM_SF = case_when(PASS_6MO == 1 & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_6MO) ~ 1, TRUE ~ 0 ),
         
         VC_GSED_12MO_DENOM_SF = case_when((ymd(UploadDate) > ONTIME_12MO |
                                              (M24_CLOSE_DSDECOD==1)) ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_GSED_SF_3MO_NUM = case_when(INF_VISIT_MNH30_14 %in% c(1,2,3) & SF_TYPE_VISIT_14 == 14 & VC_GSED_3MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 ), 
         VC_GSED_SF_6MO_NUM = case_when(INF_VISIT_MNH30_11 %in% c(1,2,3) & SF_TYPE_VISIT_11 == 11 & VC_GSED_6MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 ),
         VC_GSED_SF_12MO_NUM = case_when(INF_VISIT_MNH30_12 %in% c(1,2,3) & SF_TYPE_VISIT_12 == 12 & VC_GSED_12MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 )
  ) 


gsed_fci_wide <- inf_date_sf %>% 
  # filter for live births 
  filter(M09_BIRTH_DSTERM==1) %>% 
  ## merge in infant date (birth outcome)
  left_join(mnh31_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))  %>% 
  mutate(FCI_TYPE_VISIT = as.numeric(FCI_TYPE_VISIT)) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID, FCI_TYPE_VISIT) %>%
  # address duplicates (if exists, take the first instance)
  arrange(-desc(VISIT_OBSSTDAT)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup() %>% 
  select(SITE, MOMID, PREGID, INFANTID, DOB, FCI_TYPE_VISIT, FCI_VISIT_DATE, INF_VISIT_MNH31) %>% 
  pivot_wider(
    names_from = FCI_TYPE_VISIT,
    values_from = c(FCI_TYPE_VISIT, FCI_VISIT_DATE, INF_VISIT_MNH31),
    names_glue = "{.value}_{FCI_TYPE_VISIT}"
  ) %>% 
  
  ## calculate start windows for 3mo, 6mo, and 12mo visit
  mutate(START_3MO = DOB + 91) %>% # 13 weeks
  mutate(START_6MO = DOB + 182) %>% # 26 weeks
  mutate(START_12MO = DOB + 364) %>% # 52 weeks
  ## calculate late windows for 3mo, 6mo, and 12mo visit
  # mutate(LATE_3MO = DOB + 181) %>% # 25 weeks 6 days
  # mutate(LATE_6MO = DOB + 363) %>% # 51 weeks 6 days
  # mutate(LATE_12MO = DOB + 454) %>% # 64 weeks
  # mutate(PASS_3MO = case_when(ymd(UploadDate) > LATE_3MO ~ 1, TRUE ~ 0),
  #        PASS_6MO = case_when(ymd(UploadDate) > LATE_6MO ~ 1, TRUE ~ 0),
  # ) %>% 
  ## calculate on time windows for 3mo, 6mo, and 12mo visit
  mutate(ONTIME_3MO = DOB + 125) %>% # 15 weeks 6 days
  mutate(ONTIME_6MO = DOB + 216) %>% # 28 weeks 6 days
  mutate(ONTIME_12MO = DOB + 398) %>% # 54 weeks 6 days
  
  # select(SITE, MOMID, PREGID, INFANTID, contains("START"), contains("LATE")) %>% 
  # distinct() %>% 
  mutate(PASS_3MO = case_when(ymd(UploadDate) > ONTIME_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > ONTIME_6MO ~ 1, TRUE ~ 0),
  ) %>% 
  
  ## merge in closeout form 
  left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD) %>%  filter(INFANTID != "Z3-025-1136-B"), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_GSED_3MO_DENOM_FCI = case_when(PASS_3MO == 1  & 
                                             (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_GSED_6MO_DENOM_FCI = case_when(PASS_6MO == 1 & 
                                             (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_6MO) ~ 1, TRUE ~ 0 ),
         
         VC_GSED_12MO_DENOM_FCI = case_when((ymd(UploadDate) > ONTIME_12MO |
                                               (M24_CLOSE_DSDECOD==1)) ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_GSED_FCI_3MO_NUM = case_when(INF_VISIT_MNH31_14 %in% c(1,2,3) & FCI_TYPE_VISIT_14 == 14 & VC_GSED_3MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 ), 
         VC_GSED_FCI_6MO_NUM = case_when(INF_VISIT_MNH31_11 %in% c(1,2,3) & FCI_TYPE_VISIT_11 == 11 & VC_GSED_6MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 ),
         VC_GSED_FCI_12MO_NUM = case_when(INF_VISIT_MNH31_12 %in% c(1,2,3) & FCI_TYPE_VISIT_12 == 12 & VC_GSED_12MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 )
         
  ) 



gsed_prisma_data_out <- gsed_sf_wide %>% 
  select(SITE, MOMID, PREGID, INFANTID, contains("DENOM"), contains("NUM"), 
         START_3MO, LATE_3MO, START_6MO, LATE_6MO,
         START_12MO, LATE_12MO,
         INF_VISIT_MNH30_14, INF_VISIT_MNH30_11, INF_VISIT_MNH30_12, contains("VISIT_DATE")
  ) %>% 
  full_join(gsed_fci_wide %>% select(SITE, MOMID, PREGID, INFANTID, contains("DENOM"), contains("NUM"), 
                                     INF_VISIT_MNH31_14, INF_VISIT_MNH31_11, INF_VISIT_MNH31_12, contains("VISIT_DATE")),
            
            by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  mutate(VC_GSED_3MO_DENOM = case_when(VC_GSED_3MO_DENOM_SF==1 | VC_GSED_3MO_DENOM_FCI==1 ~ 1, TRUE ~ 0),
         VC_GSED_6MO_DENOM = case_when(VC_GSED_6MO_DENOM_SF==1 | VC_GSED_6MO_DENOM_FCI==1 ~ 1, TRUE ~ 0), 
         VC_GSED_12MO_DENOM = case_when(VC_GSED_12MO_DENOM_SF==1 | VC_GSED_12MO_DENOM_FCI==1 ~ 1, TRUE ~ 0),
  ) %>% 
  mutate(VC_GSED_3MO_DENOM = case_when(SITE %in% c("India-CMC", "India-SAS", "Kenya","Pakistan", "Zambia") ~ NA,
                                       TRUE ~ VC_GSED_3MO_DENOM),
         VC_GSED_SF_3MO_NUM = case_when(SITE %in% c("India-CMC", "India-SAS", "Kenya","Pakistan", "Zambia") ~ NA, 
                                        TRUE ~ VC_GSED_SF_3MO_NUM),
         VC_GSED_FCI_3MO_NUM = case_when(SITE %in% c("India-CMC", "India-SAS", "Kenya","Pakistan", "Zambia") ~ NA,
                                         TRUE ~ VC_GSED_FCI_3MO_NUM),
         VC_GSED_6MO_DENOM = case_when(SITE %in% c("Pakistan") ~ NA,
                                       TRUE ~ VC_GSED_6MO_DENOM),
         VC_GSED_SF_6MO_NUM = case_when(SITE %in% c("Pakistan") ~ NA,
                                        TRUE ~ VC_GSED_SF_6MO_NUM),
         VC_GSED_FCI_6MO_NUM = case_when(SITE %in% c("India-CMC","Pakistan") ~ NA, 
                                         TRUE ~ VC_GSED_FCI_6MO_NUM),
         VC_GSED_FCI_12MO_NUM = case_when(SITE %in% c("India-CMC","Pakistan") ~ NA,
                                          TRUE ~ VC_GSED_FCI_12MO_NUM)
  )


save(gsed_prisma_data_out, file= paste0(path_to_save, "gsed_prisma_data_out",".RData",sep = ""))

#*****************************************************************************
#*****************************************************************************
#* GSED LONG FORM
#*****************************************************************************
#*****************************************************************************

# Ghana
mnh32_gha <- read_csv(paste0(path_to_data, UploadDate, "_gha", "/", "mnh32.csv"))

colnames(mnh32_gha) = toupper(colnames(mnh32_gha))

mnh32_gha <- mnh32_gha %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT, CONFIRM_DATADOB) %>% 
  rename(CHILD_DOB = CONFIRM_DATADOB) %>% 
  
  ## convert to date class
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         CHILD_DOB = ymd(parse_date_time(CHILD_DOB, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  ) %>%
  ## calculate age in days at visit 
  mutate(AGEDAYS = as.numeric(VISIT_OBSSTDAT-CHILD_DOB)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Ghana") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT, CHILD_DOB, AGEDAYS)

# Zambia (reporting m/d/yyyy)
mnh32_zam <- read_csv(paste0(path_to_data, UploadDate, "_zam", "/", "mnh32.csv"))
# mnh32_zam <- read_csv(paste0(path_to_data, "2024-10-04", "_zam", "/", "mnh32.csv"))
colnames(mnh32_zam) = toupper(colnames(mnh32_zam))

mnh32_zam <- mnh32_zam %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH32,TYPE_VISIT, GENDER, CHILD_DOB) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y"))),
         CHILD_DOB =   suppressWarnings(as.Date(parse_date_time(CHILD_DOB, order = c("%Y-%m-%d %H:%M:%S"))))) %>% 
  ## calculate age in days at visit 
  mutate(AGEDAYS = as.numeric(VISIT_OBSSTDAT-CHILD_DOB)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Zambia") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH32,TYPE_VISIT, CHILD_DOB, AGEDAYS)


# Kenya DATE_ASSESSMENT and CHILD_DOB are in d m yyyy format
mnh32_ken <- read_csv(paste0(path_to_data, UploadDate, "_ke", "/", "mnh32.csv"))

colnames(mnh32_ken) = toupper(colnames(mnh32_ken))

mnh32_ken <- mnh32_ken %>% 
  select(MOMID, PREGID, INFANTID, INF_VISIT_MNH32, TYPE_VISIT, GENDER, DATE_ASSESSMENT, CHILD_DOB, AGEDAYS) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = as.Date(parse_date_time(DATE_ASSESSMENT, order = c("%m/%d/%Y %H:%M"))), # c("%d/%m/%Y %H:%M"))
         CHILD_DOB = ymd(parse_date_time(CHILD_DOB, order = c("%d/%m/%Y %H:%M")))
  ) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Kenya") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT, CHILD_DOB, AGEDAYS)



# Pakistan
mnh32_pak <- read_csv(paste0(path_to_data, UploadDate, "_pak", "/", "mnh32.csv"))
# mnh32_pak <- read_csv(paste0("Z:/2025-01-04_pak", "/", "mnh32.csv"))

colnames(mnh32_pak) = toupper(colnames(mnh32_pak))

mnh32_pak <- mnh32_pak %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT, GENDER, CHILD_DOB, AGEDAYS) %>% 
  
  ## convert to date class
  mutate(VISIT_OBSSTDAT = as.Date(parse_date_time(VISIT_OBSSTDAT, order = c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         CHILD_DOB = as.Date(parse_date_time(CHILD_DOB, orders = c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M:%S",
                                                                   "%d-%m-%Y %H:%M:%S", "%d-%b-%y %H:%M:%S")))) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Pakistan") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT, CHILD_DOB, AGEDAYS)

# cmc
mnh32_cmc <- read_csv(paste0(path_to_data, UploadDate, "_cmc", "/", "mnh32.csv"))

colnames(mnh32_cmc) = toupper(colnames(mnh32_cmc))

mnh32_cmc <- mnh32_cmc %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT, GENDER, CHILD_DOB, AGEDAYS) %>%
  
  ## convert to date class
  mutate(VISIT_OBSSTDAT = as.Date(parse_date_time(VISIT_OBSSTDAT, order = c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         CHILD_DOB = as.Date(parse_date_time(CHILD_DOB, orders = c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M:%S",
                                                                   "%d-%m-%Y %H:%M:%S", "%d-%b-%y %H:%M:%S")))) %>% 
  ## generate site indicator variable 
  mutate(SITE = "India-CMC") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT, CHILD_DOB, AGEDAYS)

#*****************************************************************************
#* Merge data 
#*****************************************************************************
mnh32_merged <- rbind(mnh32_gha,mnh32_cmc, mnh32_zam, mnh32_ken,mnh32_pak)


# rename variables to make merging easier
mnh32_merged <- mnh32_merged %>% 
  mutate(LF_TYPE_VISIT = TYPE_VISIT) %>% 
  mutate(LF_VISIT_DATE = VISIT_OBSSTDAT) %>% 
  ## remove NA visit types 
  filter(!is.na(LF_TYPE_VISIT))

#*****************************************************************************
# ReMIND Cohort Data processing for monitoring report tables -----

## Date expansion launch for GSED SF @ 12 months
# Ghana: 22-May-2024
# Kenya: 29-May-2024
# Pakistan: 30-Jan-2024
# Zambia: 20-Mar-2024
#*****************************************************************************
remind_merged <- bind_rows(aim3_gha_remind, aim3_cmc_remind, aim3_ke_remind, aim3_pak_remind, aim3_zam_remind) 

inf_date <- InfData_Wide_to_merge %>%
  select(-M09_DELIV_DSSTDAT) %>%
  # only include sitest that we have data for
  filter(SITE %in% c("Pakistan", "Kenya", "Ghana", "Zambia", "India-CMC")) %>%
  filter(!is.na(DOB))

## extract aim 3 IDs
remind_merged_enrolled <- remind_merged %>% 
  left_join(mat_enroll %>% select(SITE, MOMID, PREGID, ENROLL), by = c("SITE", "PREGID")) %>% 
  ## only include those who we have valid enrollment information for 
  filter(ENROLL == 1) %>% 
  mutate(REMIND_ENROLL = 1) %>% 
  ## remove infantid from here -- will merge in infantid data from mnh09 (in inf_date)
  ungroup() %>% 
  select(-INFANTID) %>% 
  ## some duplicates exists -- only want unique PREGID, MOMID pairs 
  distinct(SITE, MOMID, PREGID)
# KEARC00678_P1

gsed_sf_wide_remind <- inf_date %>% 
  # filter for livebirths 
  filter(M09_BIRTH_DSTERM==1) %>% 
  ## merge in infant date (birth outcome)
  left_join(mnh30_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))  %>% 
  right_join(remind_merged_enrolled, by = c("SITE", "MOMID", "PREGID"))  %>% 
  # group_by(SITE, MOMID, PREGID,INFANTID,  TYPE_VISIT) 
  mutate(SF_TYPE_VISIT = as.numeric(SF_TYPE_VISIT)) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID, SF_TYPE_VISIT) %>%
  # address duplicates (if exists, take the first instance)
  arrange(-desc(VISIT_OBSSTDAT)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup() %>% 
  select(SITE, MOMID, PREGID, INFANTID, DOB, SF_TYPE_VISIT, SF_VISIT_DATE, INF_VISIT_MNH30) %>% 
  pivot_wider(
    names_from = SF_TYPE_VISIT,
    values_from = c(SF_TYPE_VISIT, SF_VISIT_DATE, INF_VISIT_MNH30),
    names_glue = "{.value}_{SF_TYPE_VISIT}"
  ) %>% 
  ## calculate start windows for 3mo, 6mo, and 12mo visit
  mutate(START_3MO = DOB + 91) %>% # 13 weeks
  mutate(START_6MO = DOB + 182) %>% # 26 weeks
  mutate(START_12MO = DOB + 364) %>% # 52 weeks
  ## calculate late windows for 3mo, 6mo, and 12mo visit
  mutate(LATE_3MO = DOB + 181) %>% # 25 weeks 6 days
  mutate(LATE_6MO = DOB + 363) %>% # 51 weeks 6 days
  mutate(LATE_12MO = DOB + 454) %>% # 64 weeks
  ## calculate on time windows for 3mo, 6mo, and 12mo visit
  mutate(ONTIME_3MO = DOB + 125) %>% # 17 weeks 6 days
  mutate(ONTIME_6MO = DOB + 216) %>% # 30 weeks 6 days
  mutate(ONTIME_12MO = DOB + 398) %>% # 56 weeks 6 days
  # select(SITE, MOMID, PREGID, INFANTID, contains("START"), contains("LATE")) %>% 
  # distinct() %>% 
  mutate(PASS_3MO = case_when(ymd(UploadDate) > ONTIME_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > ONTIME_6MO ~ 1, TRUE ~ 0),
  ) %>% 
  ## merge in closeout form 
  left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD) %>%  filter(INFANTID != "Z3-025-1136-B"), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_AIM3_3MO_DENOM_SF = case_when((PASS_3MO == 1  & 
                                             (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_3MO) )
                                          ##   | (INF_VISIT_MNH30_14 %in% c(1,2,3) & SF_TYPE_VISIT_14 == 14)  ## or completed the form
                                          ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_6MO_DENOM_SF = case_when((PASS_6MO == 1 & 
                                             (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_6MO) )
                                          ##   | (INF_VISIT_MNH30_11 %in% c(1,2,3) & SF_TYPE_VISIT_11 == 11)  ## or completed the form
                                          
                                          ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_12MO_DENOM_SF = case_when(((ymd(UploadDate) > ONTIME_12MO |
                                               (M24_CLOSE_DSDECOD==1)) )
                                           ##   | (INF_VISIT_MNH30_12 %in% c(1,2,3) & SF_TYPE_VISIT_12 == 12)  ## or completed the form
                                           
                                           ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_AIM3_SF_3MO_NUM = case_when(INF_VISIT_MNH30_14 %in% c(1,2,3) & SF_TYPE_VISIT_14 == 14 & VC_AIM3_3MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 ), 
         VC_AIM3_SF_6MO_NUM = case_when(INF_VISIT_MNH30_11 %in% c(1,2,3) & SF_TYPE_VISIT_11 == 11 & VC_AIM3_6MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 ),
         VC_AIM3_SF_12MO_NUM = case_when(INF_VISIT_MNH30_12 %in% c(1,2,3) & SF_TYPE_VISIT_12 == 12 & VC_AIM3_12MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 )
  ) 

table(gsed_sf_wide_remind$VC_AIM3_SF_3MO_NUM, gsed_sf_wide_remind$SITE)
table(gsed_sf_wide_remind$VC_AIM3_SF_6MO_NUM, gsed_sf_wide_remind$SITE)
table(gsed_sf_wide_remind$VC_AIM3_SF_12MO_NUM, gsed_sf_wide_remind$SITE)

gsed_lf_wide_remind <- inf_date %>% 
  # filter for livebirths 
  filter(M09_BIRTH_DSTERM==1)  %>% 
  ## merge in infant date (birth outcome)
  left_join(mnh32_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))  %>% 
  right_join(remind_merged_enrolled, by = c("SITE", "MOMID", "PREGID"))  %>% 
  # mutate(LF_TYPE_VISIT = as.numeric(LF_TYPE_VISIT)) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID, LF_TYPE_VISIT) %>%
  # address duplicates (if exists, take the first instance)
  arrange(-desc(VISIT_OBSSTDAT)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup() %>% 
  select(SITE, MOMID, PREGID, INFANTID, DOB, LF_TYPE_VISIT, LF_VISIT_DATE, INF_VISIT_MNH32) %>% 
  pivot_wider(
    names_from = LF_TYPE_VISIT,
    values_from = c(LF_TYPE_VISIT, LF_VISIT_DATE, INF_VISIT_MNH32),
    names_glue = "{.value}_{LF_TYPE_VISIT}"
  ) %>% 
  ## calculate start windows for 3mo, 6mo, and 12mo visit
  mutate(START_3MO = DOB + 91) %>% # 13 weeks
  mutate(START_6MO = DOB + 182) %>% # 26 weeks
  mutate(START_12MO = DOB + 364) %>% # 52 weeks
  ## calculate late windows for 3mo, 6mo, and 12mo visit
  mutate(LATE_3MO = DOB + 181) %>% # 25 weeks 6 days
  mutate(LATE_6MO = DOB + 363) %>% # 51 weeks 6 days
  mutate(LATE_12MO = DOB + 454) %>% # 64 weeks
  ## calculate on time windows for 3mo, 6mo, and 12mo visit
  mutate(ONTIME_3MO = DOB + 125) %>% # 17 weeks 6 days
  mutate(ONTIME_6MO = DOB + 216) %>% # 30 weeks 6 days
  mutate(ONTIME_12MO = DOB + 398) %>% # 56 weeks 6 days
  # select(SITE, MOMID, PREGID, INFANTID, contains("START"), contains("LATE")) %>% 
  # distinct() %>% 
  mutate(PASS_3MO = case_when(ymd(UploadDate) > ONTIME_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > ONTIME_6MO ~ 1, TRUE ~ 0),
  ) %>% 
  
  ## merge in closeout form 
  left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD) %>%  filter(INFANTID != "Z3-025-1136-B"), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_AIM3_3MO_DENOM_LF = case_when((PASS_3MO == 1  & 
                                             (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_3MO) )
                                          ##      | (INF_VISIT_MNH32_14 %in% c(1,2,3) & LF_TYPE_VISIT_14 == 14)  ## or completed the form
                                          
                                          ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_6MO_DENOM_LF = case_when(PASS_6MO == 1 & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_6MO) ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_12MO_DENOM_LF = case_when((ymd(UploadDate) > ONTIME_12MO |
                                              (M24_CLOSE_DSDECOD==1)) ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_AIM3_LF_3MO_NUM = case_when(INF_VISIT_MNH32_14 %in% c(1,2,3) & LF_TYPE_VISIT_14 == 14 & VC_AIM3_3MO_DENOM_LF ==1 ~ 1, TRUE ~ 0 ), 
         # VC_AIM3_LF_6MO_NUM = case_when(INF_VISIT_MNH32_11 %in% c(1,2,3) & LF_TYPE_VISIT_11 == 11 & VC_AIM3_6MO_DENOM_LF ==1 ~ 1, TRUE ~ 0 ),
         # VC_AIM3_LF_12MO_NUM = case_when(INF_VISIT_MNH32_12 %in% c(1,2,3) & LF_TYPE_VISIT_12 == 12 & VC_AIM3_12MO_DENOM_LF ==1 ~ 1, TRUE ~ 0 )
  ) %>% 
  # since there are no six or twelve month visits, code as 0
  mutate(VC_AIM3_LF_6MO_NUM = 0,
         VC_AIM3_LF_12MO_NUM = 0)


gsed_fci_wide_remind <- inf_date %>% 
  # filter for live births 
  filter(M09_BIRTH_DSTERM==1) %>% 
  ## merge in infant date (birth outcome)
  left_join(mnh31_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))  %>% 
  right_join(remind_merged_enrolled, by = c("SITE", "MOMID", "PREGID"))  %>% 
  mutate(FCI_TYPE_VISIT = as.numeric(FCI_TYPE_VISIT)) %>% 
  # address duplicates (if exists, take the first instance)
  group_by(SITE, MOMID, PREGID, INFANTID, FCI_TYPE_VISIT) %>%
  arrange(-desc(VISIT_OBSSTDAT)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup() %>% 
  select(SITE, MOMID, PREGID, INFANTID, DOB, FCI_TYPE_VISIT, FCI_VISIT_DATE, INF_VISIT_MNH31) %>% 
  pivot_wider(
    names_from = FCI_TYPE_VISIT,
    values_from = c(FCI_TYPE_VISIT, FCI_VISIT_DATE, INF_VISIT_MNH31),
    names_glue = "{.value}_{FCI_TYPE_VISIT}"
  ) %>% 
  
  ## calculate start windows for 3mo, 6mo, and 12mo visit
  mutate(START_3MO = DOB + 91) %>% # 13 weeks
  mutate(START_6MO = DOB + 182) %>% # 26 weeks
  mutate(START_12MO = DOB + 364) %>% # 52 weeks
  ## calculate late windows for 3mo, 6mo, and 12mo visit
  mutate(LATE_3MO = DOB + 181) %>% # 25 weeks 6 days
  mutate(LATE_6MO = DOB + 363) %>% # 51 weeks 6 days
  mutate(LATE_12MO = DOB + 454) %>% # 64 weeks
  ## calculate on time windows for 3mo, 6mo, and 12mo visit
  mutate(ONTIME_3MO = DOB + 125) %>% # 17 weeks 6 days
  mutate(ONTIME_6MO = DOB + 216) %>% # 30 weeks 6 days
  mutate(ONTIME_12MO = DOB + 398) %>% # 56 weeks 6 days
  # select(SITE, MOMID, PREGID, INFANTID, contains("START"), contains("LATE")) %>% 
  # distinct() %>% 
  mutate(PASS_3MO = case_when(ymd(UploadDate) > ONTIME_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > ONTIME_6MO ~ 1, TRUE ~ 0),
  ) %>% 
  ## merge in closeout form 
  left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD) %>%  filter(INFANTID != "Z3-025-1136-B"), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_AIM3_3MO_DENOM_FCI = case_when((PASS_3MO == 1  & 
                                              (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_3MO))
                                           ##    | (INF_VISIT_MNH31_14 %in% c(1,2,3) & FCI_TYPE_VISIT_14 == 14)  ## or completed the form
                                           
                                           ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_6MO_DENOM_FCI = case_when((PASS_6MO == 1 & 
                                              (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_6MO))
                                           ##     | (INF_VISIT_MNH31_11 %in% c(1,2,3) & FCI_TYPE_VISIT_11 == 11)  ## or completed the form
                                           
                                           ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_12MO_DENOM_FCI = case_when(((ymd(UploadDate) > ONTIME_12MO |
                                                (M24_CLOSE_DSDECOD==1)))
                                            ##     | (INF_VISIT_MNH31_12 %in% c(1,2,3) & FCI_TYPE_VISIT_12 == 12)  ## or completed the form
                                            
                                            ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_AIM3_FCI_3MO_NUM = case_when(INF_VISIT_MNH31_14 %in% c(1,2,3) & FCI_TYPE_VISIT_14 == 14 & VC_AIM3_3MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 ), 
         VC_AIM3_FCI_6MO_NUM = case_when(INF_VISIT_MNH31_11 %in% c(1,2,3) & FCI_TYPE_VISIT_11 == 11 & VC_AIM3_6MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 ),
         VC_AIM3_FCI_12MO_NUM = case_when(INF_VISIT_MNH31_12 %in% c(1,2,3) & FCI_TYPE_VISIT_12 == 12 & VC_AIM3_12MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 )
         
  ) 


gsed_aim3_data <- gsed_sf_wide_remind %>% 
  select(SITE, MOMID, PREGID, INFANTID, contains("DENOM"), contains("NUM"), 
         START_3MO, ONTIME_3MO, START_6MO, ONTIME_6MO,
         START_12MO, ONTIME_12MO,
         INF_VISIT_MNH30_14, INF_VISIT_MNH30_11, INF_VISIT_MNH30_12, contains("VISIT_DATE")
  ) %>% 
  full_join(gsed_fci_wide_remind %>% select(SITE, MOMID, PREGID, INFANTID, contains("DENOM"), contains("NUM"), 
                                            INF_VISIT_MNH31_14, INF_VISIT_MNH31_11, INF_VISIT_MNH31_12, contains("VISIT_DATE")),
            
            by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  
  full_join(gsed_lf_wide_remind %>% select(SITE, MOMID, PREGID, INFANTID, contains("DENOM"), contains("NUM"), 
                                           contains("INF_VISIT_MNH32"), contains("VISIT_DATE")),
            
            by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  
  # generate indicator variable is expansion launched by the time baby was entering PNC52 period 
  mutate(EXPANSION_3MO  = case_when((SITE =="Ghana" &  START_3MO >= ymd("2024-05-22")) | 
                                      (SITE =="India-CMC" &  START_3MO >= ymd("2024-03-28")) | 
                                      (SITE =="Kenya" &  START_3MO >= ymd("2024-05-29")) |   
                                      (SITE =="Pakistan" &  START_3MO >= ymd("2024-01-30")) |     
                                      (SITE =="Zambia" &  START_3MO >= ymd("2024-03-30")) ~ 1, 
                                    TRUE ~ 0
  )) %>% 
  mutate(EXPANSION_6MO  = case_when((SITE =="Ghana" &  START_6MO >= ymd("2024-05-22")) | 
                                      (SITE =="India-CMC" &  START_6MO >= ymd("2024-03-28")) | 
                                      (SITE =="Kenya" &  START_6MO >= ymd("2024-05-29")) |    
                                      (SITE =="Pakistan" &  START_6MO >= ymd("2024-01-30")) |     
                                      (SITE =="Zambia" &  START_6MO >= ymd("2024-03-30")) ~ 1, 
                                    TRUE ~ 0
  )) %>% 
  mutate(EXPANSION_12MO  = case_when((SITE =="Ghana" &  START_12MO >= ymd("2024-05-22")) | 
                                       (SITE =="India-CMC" &  START_12MO >= ymd("2024-03-28")) | 
                                       (SITE =="Kenya" &  START_12MO >= ymd("2024-05-29")) |   
                                       (SITE =="Pakistan" &  START_12MO >= ymd("2024-01-30")) |      
                                       (SITE =="Zambia" &  START_12MO >= ymd("2024-03-30")) ~ 1, 
                                     TRUE ~ 0
  )) %>% 
  
  mutate(VC_AIM3_3MO_DENOM = case_when((VC_AIM3_3MO_DENOM_SF==1 | VC_AIM3_3MO_DENOM_FCI==1 | VC_AIM3_3MO_DENOM_LF==1) & EXPANSION_3MO==1 ~ 1, TRUE ~ 0),
         VC_AIM3_6MO_DENOM = case_when((VC_AIM3_6MO_DENOM_SF==1 | VC_AIM3_6MO_DENOM_FCI==1 | VC_AIM3_6MO_DENOM_LF==1) & EXPANSION_6MO==1 ~ 1, TRUE ~ 0), 
         VC_AIM3_12MO_DENOM = case_when((VC_AIM3_12MO_DENOM_SF==1 | VC_AIM3_12MO_DENOM_FCI==1  | VC_AIM3_12MO_DENOM_LF==1) & EXPANSION_12MO==1~ 1, TRUE ~ 0),
  ) 


table(gsed_aim3_data$VC_AIM3_LF_3MO_NUM, gsed_aim3_data$SITE)
table(gsed_aim3_data$VC_AIM3_LF_6MO_NUM, gsed_aim3_data$SITE)
table(gsed_aim3_data$VC_AIM3_LF_12MO_NUM, gsed_aim3_data$SITE)



save(gsed_aim3_data, file= paste0(path_to_save, "gsed_aim3_data",".RData",sep = ""))


## REMIND TABLE 
gsed_aim3_tab <- gsed_aim3_data %>%
  ## If India-SAS doesn't have data, add empty row here
  mutate(existing_sas = ifelse(SITE == "India-SAS", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_sas == 0, "India-SAS", SITE), fill = list(SITE = NA)) %>%
  ## If India-CMC doesn't have data, add empty row here
  mutate(existing_cmc = ifelse(SITE == "India-CMC", 1, 0)) %>%
  ## Add empty rows for missing SITE values if the specific site doesn't exist
  complete(SITE = ifelse(existing_cmc == 0, "India-CMC", SITE), fill = list(SITE = NA)) %>%
  rowwise() %>%
  group_by(SITE) %>%
  summarise(
    
    ## 3 MONTHS
    "Denominator (3MO)" = paste0(
      format(sum(VC_AIM3_3MO_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)
    ),
    
    "GSED SF (3MO)" = paste0(
      format(sum(VC_AIM3_SF_3MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(VC_AIM3_SF_3MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_3MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "GSED LF (3MO)" = paste0(
      format(sum(VC_AIM3_LF_3MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(VC_AIM3_LF_3MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_3MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "FCI (3MO)" = paste0(
      format(sum(VC_AIM3_FCI_3MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(VC_AIM3_FCI_3MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_3MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    
    ## 6 MONTHS
    "Denominator (6MO)" = paste0(
      format(sum(VC_AIM3_6MO_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)
    ),
    
    "GSED SF (6MO)" = paste0(
      format(sum(VC_AIM3_SF_6MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(VC_AIM3_SF_6MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_6MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "GSED LF (6MO)" = paste0(
      format(sum(VC_AIM3_LF_6MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(VC_AIM3_LF_6MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_6MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "FCI (6MO)" = paste0(
      format(sum(VC_AIM3_FCI_6MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(VC_AIM3_FCI_6MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_6MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    ## 12 MONTHS
    "Denominator (12MO)" = paste0(
      format(sum(VC_AIM3_12MO_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)
    ),
    
    "GSED SF (12MO)" = paste0(
      format(sum(VC_AIM3_SF_12MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(VC_AIM3_SF_12MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_12MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "GSED LF (12MO)" = paste0(
      format(sum(VC_AIM3_LF_12MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(VC_AIM3_LF_12MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_12MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")"),
    
    "FCI (12MO)" = paste0(
      format(sum(VC_AIM3_FCI_12MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
      " (",
      format(round(sum(VC_AIM3_FCI_12MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_12MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
      ")")
    
    
  ) %>%
  
  t() %>% as.data.frame() %>%
  `colnames<-`(c(.[1,])) %>%
  slice(-1) %>%
  add_column(
    .before = 1,
    "Total" = gsed_aim3_data %>%
      plyr::summarise(
        
        ## 3 MONTHS
        "Denominator (3MO)" = paste0(
          format(sum(VC_AIM3_3MO_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)
        ),
        
        "GSED SF (3MO)" = paste0(
          format(sum(VC_AIM3_SF_3MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(VC_AIM3_SF_3MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_3MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        
        "GSED LF (3MO)" = paste0(
          format(sum(VC_AIM3_LF_3MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(VC_AIM3_LF_3MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_3MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        
        "FCI (3MO)" = paste0(
          format(sum(VC_AIM3_FCI_6MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(VC_AIM3_FCI_6MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_3MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        
        
        ## 6 MONTHS
        "Denominator (6MO)" = paste0(
          format(sum(VC_AIM3_6MO_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)
        ),
        
        "GSED SF (6MO)" = paste0(
          format(sum(VC_AIM3_SF_6MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(VC_AIM3_SF_6MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_6MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        
        "GSED LF (6MO)" = paste0(
          format(sum(VC_AIM3_LF_6MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(VC_AIM3_LF_6MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_6MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        
        "FCI (6MO)" = paste0(
          format(sum(VC_AIM3_FCI_6MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(VC_AIM3_FCI_6MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_6MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        
        ## 12 MONTHS
        "Denominator (12MO)" = paste0(
          format(sum(VC_AIM3_12MO_DENOM == 1, na.rm = TRUE), nsmall = 0, digits = 2)
        ),
        
        "GSED SF (12MO)" = paste0(
          format(sum(VC_AIM3_SF_12MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(VC_AIM3_SF_12MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_12MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        
        "GSED LF (12MO)" = paste0(
          format(sum(VC_AIM3_LF_12MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(VC_AIM3_LF_12MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_12MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")"),
        
        "FCI (12MO)" = paste0(
          format(sum(VC_AIM3_FCI_12MO_NUM==1, na.rm = TRUE), nsmall = 0, digits = 2),
          " (",
          format(round(sum(VC_AIM3_FCI_12MO_NUM ==1, na.rm = TRUE)/sum(VC_AIM3_12MO_DENOM==1, na.rm = TRUE)*100, 2), nsmall = 0, digits = 2),
          ")")
        
      ) %>%
      t() %>%
      as.data.frame() %>%
      `colnames<-`(c(.[1,])) %>% unlist()
  )



## output 
gsed_aim3_tab_data <- gsed_aim3_tab%>%
  `rownames<-` (c(
    "Denominator, n",
    "GSED SF",
    "GSED LF",
    "FCI",
    "Denominator, n ",
    "GSED SF ",
    "GSED LF ",
    "FCI ",
    "Denominator, n  ",
    "GSED SF  ",
    "GSED LF  ",
    "FCI  "
  )
  ) %>%
  mutate_at(vars(everything()), ~ifelse(. == "0 (NaN)", "0 (0)", .))

gsed_aim3_tab_out <- tb_theme1(gsed_aim3_tab_data) %>%
  tab_header(
    title = md("Table 17. ReMIND Assessment Completion")
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>3 month visit (Late window: 13 to <26 weeks) (GSED required; FCI optional) <sup>a</sup></span>"),
    rows = 1:4
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>6 month visit (Late window: 26 to <52 weeks) (GSED & FCI optional) <sup>b</sup></span>"),
    rows = 5:8
  ) %>%
  tab_row_group(
    label = html("<span style='font-size: 18px'>12 month visit (Late window: 52 to <61 weeks) (GSED required; FCI optional) <sup>c</sup></span>"),
    rows = 9:12
  ) %>%
  row_group_order(groups = c("<span style='font-size: 18px'>3 month visit (Late window: 13 to <26 weeks) (GSED required; FCI optional) <sup>a</sup></span>",
                             "<span style='font-size: 18px'>6 month visit (Late window: 26 to <52 weeks) (GSED & FCI optional) <sup>b</sup></span>",
                             "<span style='font-size: 18px'>12 month visit (Late window: 52 to <61 weeks) (GSED required; FCI optional) <sup>c</sup></span>"
  ))  %>%
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>a</sup> denominator is n passed late window with >=26 weeks gestation AND (has not closed out OR closed out with closeout date >=26wks).</span>")
  ) %>%
  tab_footnote(
    footnote = html("<span style='font-size: 18px'><sup>b</sup> denominator is n passed late window with >=52 weeks gestation AND (has not closed out OR closed out with closeout date >=52wks).</span>")
  ) %>%
  tab_footnote(
    footnote = md("<span style='font-size: 18px'><sup>c</sup> denominator is n passed late window with >=61 weeks gestation OR closed out with closeout
  reason reported as: *1-year postpartum follow-up period has ended*.</span>")
  )
