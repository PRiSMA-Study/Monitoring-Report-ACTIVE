#*****************************************************************************
#* Monitoring report GSED SF/LF/FCI
#* Drafted: 4 September 2024, Stacie Loisate (Translating Nazia's Stata Code to R)
#* Last updated: 25 October 2024

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
UploadDate = "2024-10-18"

# path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")
path_to_data <- paste0("D:/Users/stacie.loisate/Documents/import/")
path_to_save <- "D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/"
path_to_tnt <- "Z:/SynapseCSVs/"
mat_enroll <- read.csv(paste0("Z:/Outcome Data/", UploadDate, "/", "MAT_ENROLL.csv"))
load(paste0("D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/MatData_Pnc_Visits.RData"))
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

## read in aim 3 IDs 
aim3_gha <- read_excel("Z:/ReMAPP_Aim3_IDs/GH_Aim3_Participants_02July2024.xlsx") %>% select(momid, pregid) %>% 
  mutate(SITE = "Ghana")
names(aim3_gha) <- toupper(names(aim3_gha))

aim3_cmc <- read_excel("Z:/ReMAPP_Aim3_IDs/IN-CMC_Aim3_Participants_13June2024.xlsx") %>% select(MOMID, PREGID) %>% 
  mutate(SITE = "India-CMC")
names(aim3_cmc) <- toupper(names(aim3_cmc))

aim3_ke <- read_excel("Z:/ReMAPP_Aim3_IDs/KY_Aim3_Participants_06JUNE2024.xlsx") %>% select(MOMID, PREGID) %>% 
  mutate(SITE = "Kenya")
names(aim3_ke) <- toupper(names(aim3_ke))

aim3_pak <- read_excel("Z:/ReMAPP_Aim3_IDs/PK_Aim3_Participants_06JUNE2024.xlsx") %>% 
  rename(MOMID = "MOM ID") %>% 
  rename(PREGID = "PReg ID") %>% 
  rename(INFANTID = "INFANT ID") %>% 
  select(MOMID, PREGID) %>% 
  mutate(SITE = "Pakistan")
names(aim3_pak) <- toupper(names(aim3_pak))

aim3_zam <- read_excel("Z:/ReMAPP_Aim3_IDs/ZM_Aim3_Participants_2024-09-05.xlsx") %>% 
  rename(MOMID = "Mom ID") %>% 
  rename(PREGID = "Preg ID") %>% 
  rename(INFANTID = "Infant ID") %>% 
  select(MOMID, PREGID) %>% 
  mutate(SITE = "Zambia")

names(aim3_zam) <- toupper(names(aim3_zam))

aim3_merged <- bind_rows(aim3_gha, aim3_cmc, aim3_ke, aim3_pak, aim3_zam) %>% 
  relocate(SITE, .before = MOMID) %>% 
  distinct(SITE, MOMID, PREGID)

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
# mnh30_zam <- read.csv(paste0(path_to_tnt,"Zambia/", UploadDate, "/", "mnh30.csv"))

colnames(mnh30_zam) = toupper(colnames(mnh30_zam))

mnh30_zam <- mnh30_zam %>% 
  select(-CHILD_DOB) %>% 
  rename(MOMID = `ASSESSMENT.MOMID`,
         PREGID = `ASSESSMENT.PREGID`,
         INFANTID = `ASSESSMENT.INFANTID`,
         VISIT_OBSSTDAT = `ASSESSMENT.VISIT_OBSSTDAT`,
         TYPE_VISIT = `ASSESSMENT.TYPE_VISIT`,
         CHILD_DOB = `ID_INFO.DATE_OF_BIRTH_CHILD`,
         INF_VISIT_MNH30 = `ASSESSMENT.INF_VISIT_MNH30`
          ) %>%
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
mnh30_merged <- rbind(mnh30_gha,mnh30_cmc, mnh30_zam, mnh30_ken, mnh30_pak)

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
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%Y-%m-%d")))) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Zambia") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,  TYPE_VISIT) # , CHILD_DOB, AGEDAYS


# Kenya -- all DATE_ASSESSMENT are ub dd-mmm-yy
mnh31_ken <- read.csv(paste0(path_to_tnt,"Kenya/", UploadDate, "/", "mnh31.csv"))
colnames(mnh31_ken) = toupper(colnames(mnh31_ken))

mnh31_ken <- mnh31_ken %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH31, TYPE_VISIT) %>% # , STUDY_ID, GENDER, DATE_ASSESSMENT, CHILD_DOB, AGEDAYS
  ## convert to date class
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d-%b-%y")))
  ) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Kenya") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,  TYPE_VISIT) # , CHILD_DOB, AGEDAYS

#*****************************************************************************
# FCI: merge data & remove duplicates -----
#*****************************************************************************
mnh31_merged <- rbind(mnh31_gha, mnh31_zam, mnh31_ken)

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
#*****************************************************************************
inf_date_sf <- InfData_Wide_to_merge %>% 
  select(-M09_DELIV_DSSTDAT) %>% 
  # only include sitest that we have data for 
  filter(SITE %in% c("Pakistan", "Kenya", "Ghana", "Zambia", "India-CMC")) %>% 
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
  # select(SITE, MOMID, PREGID, INFANTID, contains("START"), contains("LATE")) %>% 
  # distinct() %>% 
  mutate(PASS_3MO = case_when(ymd(UploadDate) > LATE_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > LATE_6MO ~ 1, TRUE ~ 0),
  ) %>% 
## merge in closeout form 
left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD) %>%  filter(INFANTID != "Z3-025-1136-B"), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_GSED_3MO_DENOM_SF = case_when(PASS_3MO == 1  & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > LATE_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_GSED_6MO_DENOM_SF = case_when(PASS_6MO == 1 & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > LATE_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_GSED_12MO_DENOM_SF = case_when((ymd(UploadDate) > LATE_12MO |
                                              (M24_CLOSE_DSDECOD==1)) ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_GSED_SF_3MO_NUM = case_when(INF_VISIT_MNH30_14 %in% c(1,2) & SF_TYPE_VISIT_14 == 14 & VC_GSED_3MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 ), 
         VC_GSED_SF_6MO_NUM = case_when(INF_VISIT_MNH30_11 %in% c(1,2) & SF_TYPE_VISIT_11 == 11 & VC_GSED_6MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 ),
         VC_GSED_SF_12MO_NUM = case_when(INF_VISIT_MNH30_12 %in% c(1,2) & SF_TYPE_VISIT_12 == 12 & VC_GSED_12MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 )
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
  mutate(LATE_3MO = DOB + 181) %>% # 25 weeks 6 days
  mutate(LATE_6MO = DOB + 363) %>% # 51 weeks 6 days
  mutate(LATE_12MO = DOB + 454) %>% # 64 weeks
  mutate(PASS_3MO = case_when(ymd(UploadDate) > LATE_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > LATE_6MO ~ 1, TRUE ~ 0),
  ) %>% 
## merge in closeout form 
left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD) %>%  filter(INFANTID != "Z3-025-1136-B"), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_GSED_3MO_DENOM_FCI = case_when(PASS_3MO == 1  & 
                                             (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > LATE_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_GSED_6MO_DENOM_FCI = case_when(PASS_6MO == 1 & 
                                             (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > LATE_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_GSED_12MO_DENOM_FCI = case_when((ymd(UploadDate) > LATE_12MO |
                                               (M24_CLOSE_DSDECOD==1)) ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_GSED_FCI_3MO_NUM = case_when(INF_VISIT_MNH31_14 %in% c(1,2) & FCI_TYPE_VISIT_14 == 14 & VC_GSED_3MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 ), 
         VC_GSED_FCI_6MO_NUM = case_when(INF_VISIT_MNH31_11 %in% c(1,2) & FCI_TYPE_VISIT_11 == 11 & VC_GSED_6MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 ),
         VC_GSED_FCI_12MO_NUM = case_when(INF_VISIT_MNH31_12 %in% c(1,2) & FCI_TYPE_VISIT_12 == 12 & VC_GSED_12MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 )
         
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


######
# save(gsed_prisma_data_out, file= paste0(path_to_save, "gsed_prisma_data_out",".RData",sep = ""))

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
mnh32_zam <- read_csv(paste0(path_to_data, "2024-10-04", "_zam", "/", "mnh32.csv"))
colnames(mnh32_zam) = toupper(colnames(mnh32_zam))

mnh32_zam <- mnh32_zam %>% 
  select(-CHILD_DOB) %>% 
  rename(MOMID = `ASSESSMENT:MOMID`,
         PREGID = `ASSESSMENT:PREGID`,
         INFANTID = `ASSESSMENT:INFANTID`,
         VISIT_OBSSTDAT = `ASSESSMENT:VISIT_OBSSTDAT`,
         TYPE_VISIT = `ASSESSMENT:TYPE_VISIT`,
         CHILD_DOB = `ID_INFO:DATE_OF_BIRTH_CHILD`,
         INF_VISIT_MNH32 = `ASSESSMENT:INF_VISIT_MNH32`
  ) %>%
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
  mutate(VISIT_OBSSTDAT = as.Date(parse_date_time(DATE_ASSESSMENT, order = c("%d/%m/%Y %H:%M"))),
         CHILD_DOB = ymd(parse_date_time(CHILD_DOB, order = c("%d/%m/%Y %H:%M")))
  ) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Kenya") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT, CHILD_DOB, AGEDAYS)



# Pakistan
mnh32_pak <- read_csv(paste0(path_to_data, UploadDate, "_pak", "/", "mnh32.csv"))

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
mnh32_merged <- rbind(mnh32_gha,mnh32_cmc, mnh32_zam, mnh32_ken, mnh32_pak)


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


inf_date <- InfData_Wide_to_merge %>%
  select(-M09_DELIV_DSSTDAT) %>%
  # only include sitest that we have data for
  filter(SITE %in% c("Pakistan", "Kenya", "Ghana", "Zambia", "India-CMC")) %>%
  filter(!is.na(DOB))

## extract aim 3 IDs
aim3_all <- aim3_merged %>% 
  mutate(AIM3_ENROLL = 1)


gsed_sf_wide_remind <- inf_date %>% 
  # filter for livebirths 
  filter(M09_BIRTH_DSTERM==1) %>% 
  ## merge in infant date (birth outcome)
  left_join(mnh30_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))  %>% 
  right_join(aim3_all, by = c("SITE", "MOMID", "PREGID"))  %>% 
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
  # select(SITE, MOMID, PREGID, INFANTID, contains("START"), contains("LATE")) %>% 
  # distinct() %>% 
  mutate(PASS_3MO = case_when(ymd(UploadDate) > LATE_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > LATE_6MO ~ 1, TRUE ~ 0),
  ) %>% 
## merge in closeout form 
left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD) %>%  filter(INFANTID != "Z3-025-1136-B"), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_AIM3_3MO_DENOM_SF = case_when(PASS_3MO == 1  & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > LATE_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_6MO_DENOM_SF = case_when(PASS_6MO == 1 & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > LATE_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_12MO_DENOM_SF = case_when((ymd(UploadDate) > LATE_12MO |
                                              (M24_CLOSE_DSDECOD==1)) ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_AIM3_SF_3MO_NUM = case_when(INF_VISIT_MNH30_14 %in% c(1,2) & SF_TYPE_VISIT_14 == 14 & VC_AIM3_3MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 ), 
         VC_AIM3_SF_6MO_NUM = case_when(INF_VISIT_MNH30_11 %in% c(1,2) & SF_TYPE_VISIT_11 == 11 & VC_AIM3_6MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 ),
         VC_AIM3_SF_12MO_NUM = case_when(INF_VISIT_MNH30_12 %in% c(1,2) & SF_TYPE_VISIT_12 == 12 & VC_AIM3_12MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 )
  ) 


gsed_lf_wide_remind <- inf_date %>% 
  # filter for livebirths 
  filter(M09_BIRTH_DSTERM==1)  %>% 
  ## merge in infant date (birth outcome)
  left_join(mnh32_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))  %>% 
  right_join(aim3_all, by = c("SITE", "MOMID", "PREGID"))  %>% 
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
  # select(SITE, MOMID, PREGID, INFANTID, contains("START"), contains("LATE")) %>% 
  # distinct() %>% 
  mutate(PASS_3MO = case_when(ymd(UploadDate) > LATE_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > LATE_6MO ~ 1, TRUE ~ 0),
  ) %>% 

## merge in closeout form 
left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD) %>%  filter(INFANTID != "Z3-025-1136-B"), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_AIM3_3MO_DENOM_LF = case_when(PASS_3MO == 1  & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > LATE_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_6MO_DENOM_LF = case_when(PASS_6MO == 1 & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > LATE_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_12MO_DENOM_LF = case_when((ymd(UploadDate) > LATE_12MO |
                                              (M24_CLOSE_DSDECOD==1)) ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_AIM3_LF_3MO_NUM = case_when(INF_VISIT_MNH32_14 %in% c(1,2) & LF_TYPE_VISIT_14 == 14 & VC_AIM3_3MO_DENOM_LF ==1 ~ 1, TRUE ~ 0 ), 
         # VC_AIM3_LF_6MO_NUM = case_when(INF_VISIT_MNH32_11 %in% c(1,2) & LF_TYPE_VISIT_11 == 11 & VC_AIM3_6MO_DENOM_LF ==1 ~ 1, TRUE ~ 0 ),
         # VC_AIM3_LF_12MO_NUM = case_when(INF_VISIT_MNH32_12 %in% c(1,2) & LF_TYPE_VISIT_12 == 12 & VC_AIM3_12MO_DENOM_LF ==1 ~ 1, TRUE ~ 0 )
  ) 


gsed_fci_wide_remind <- inf_date %>% 
  # filter for live births 
  filter(M09_BIRTH_DSTERM==1) %>% 
  ## merge in infant date (birth outcome)
  left_join(mnh31_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))  %>% 
  right_join(aim3_all, by = c("SITE", "MOMID", "PREGID"))  %>% 
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
  # select(SITE, MOMID, PREGID, INFANTID, contains("START"), contains("LATE")) %>% 
  # distinct() %>% 
  mutate(PASS_3MO = case_when(ymd(UploadDate) > LATE_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > LATE_6MO ~ 1, TRUE ~ 0),
  ) %>% 
  ## merge in closeout form 
  left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD) %>%  filter(INFANTID != "Z3-025-1136-B"), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_AIM3_3MO_DENOM_FCI = case_when(PASS_3MO == 1  & 
                                             (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > LATE_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_6MO_DENOM_FCI = case_when(PASS_6MO == 1 & 
                                             (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > LATE_3MO) ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_12MO_DENOM_FCI = case_when((ymd(UploadDate) > LATE_12MO |
                                               (M24_CLOSE_DSDECOD==1)) ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_AIM3_FCI_3MO_NUM = case_when(INF_VISIT_MNH31_14 %in% c(1,2) & FCI_TYPE_VISIT_14 == 14 & VC_AIM3_3MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 ), 
         VC_AIM3_FCI_6MO_NUM = case_when(INF_VISIT_MNH31_11 %in% c(1,2) & FCI_TYPE_VISIT_11 == 11 & VC_AIM3_6MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 ),
         VC_AIM3_FCI_12MO_NUM = case_when(INF_VISIT_MNH31_12 %in% c(1,2) & FCI_TYPE_VISIT_12 == 12 & VC_AIM3_12MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 )
         
  ) 


gsed_aim3_data <- gsed_sf_wide_remind %>% 
  select(SITE, MOMID, PREGID, INFANTID, contains("DENOM"), contains("NUM"), 
         START_3MO, LATE_3MO, START_6MO, LATE_6MO,
         START_12MO, LATE_12MO,
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

######
# save(gsed_prisma_data_out, file= paste0(path_to_save, "gsed_prisma_data_out",".RData",sep = "")) 
