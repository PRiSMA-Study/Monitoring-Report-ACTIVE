#*****************************************************************************
#* Monitoring report GSED SF/LF/FCI
#* Drafted: 4 September 2024, Stacie Loisate (Translating Nazia's Stata Code to R)
#* Last updated: 30 March 2026 (cleaning up code)

#*****************************************************************************
#*****************************************************************************
#* Data Setup 
#*****************************************************************************
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)
library(openxlsx)

# Path set-up ----
# UPDATE EACH RUN # 
# set upload date 
UploadDate = "2026-03-20"

# path_to_tnt <- paste0("Z:/Outcome Data/", UploadDate, "/")
path_to_data <- paste0("D:/Users/stacie.loisate/Documents/import/")
path_to_save <- "D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/"
path_to_tnt <- "Z:/"

mat_enroll <- read.csv(paste0("Z:/Outcome Data/", UploadDate, "/", "MAT_ENROLL.csv"))
load(paste0("D:/Users/stacie.loisate/Box/Monitoring-Report-Active/data/MatData_Pnc_Visits.RData"))
load(paste0("D:/Users/stacie.loisate/Documents/Monitoring Report/data/cleaned/", UploadDate, "/InfData_Wide_", UploadDate, ".RData"))
mnh24 <- read_csv(paste0(path_to_data, UploadDate, "/", "mnh24_merged.csv"))
mnh24 <- read_csv(paste0(path_to_tnt,"Stacked Data/", UploadDate, "/", "mnh24_merged.csv"))

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

## read in aim 3 IDs -- this will be for the PRISMA & ReMAPP cohort tables 
aim3_list <- list.files(path = "D:/Users/stacie.loisate/Documents/Output/Outcomes-Queries/ReMIND",
                        pattern = '.xlsx',
                        full.names = TRUE)

aim3_output <- sapply(aim3_list, read_excel)
aim3ids <- bind_rows(aim3_output)
aim3ids <- aim3ids %>% filter(!is.na(SITE))

# import final list for all sites 
aim3_final_moms <- read.xlsx("Z:/ReMAPP_Aim3_IDs/Finalized Lists/Allsites_Jan_2026.xlsx") %>% mutate(AIM3 = 1)
# aim3_final_infants <- read.xlsx("D:/Users/stacie.loisate/Documents/Output/Outcomes-Queries/ReMIND/remind-infantids_2026-03-04.xlsx") %>% mutate(AIM3 = 1)
aim3_final_infants <- read.xlsx("Z:/ReMAPP_Aim3_IDs/remind-infantids_2026-03-04.xlsx") %>% mutate(AIM3 = 1)

aim3ids <- aim3_final_infants
table(aim3_final_infants$REMIND_ENROLLMENT_STATUS_TEXT, aim3_final_infants$SITE)

# Generate function to convert any date class to YYYY-MM-DD ----
smart_parse <- function(x) {
  # Normalize separators: convert "-" and "." to "/"
  x <- gsub("[-.]", "/", x)
  
  # Detect if the format uses a 3-letter month (e.g., "14/AUG/24")
  has_text_month <- grepl("[A-Za-z]{3}", x)
  
  # Extract first and second numeric chunks (for numeric formats)
  first  <- as.numeric(stringr::str_extract(x, "^[0-9]{1,4}"))
  second <- as.numeric(stringr::str_extract(x, "(?<=^[0-9]{1,4}[^0-9])[0-9]{1,2}"))
  
  dplyr::case_when(
    # dd-mon-yy or dd-mon-yyyy
    has_text_month ~ suppressWarnings(lubridate::dmy(x)),
    
    # yyyy/mm/dd (first chunk has 4 digits)
    nchar(first) == 4 ~ suppressWarnings(lubridate::ymd(x)),
    
    # dd/mm/yyyy → first > 12
    first > 12 ~ suppressWarnings(lubridate::dmy(x)),
    
    # mm/dd/yyyy → second > 12
    second > 12 ~ suppressWarnings(lubridate::mdy(x)),
    
    # ambiguous → try all formats
    TRUE ~ suppressWarnings(dplyr::coalesce(
      lubridate::ymd(x),
      lubridate::dmy(x),
      lubridate::mdy(x)
    ))
  )
}


#*****************************************************************************
#*****************************************************************************
# PRISMA/ReMAPP Cohort:
# GSED SF: import data & data cleaning -----
#*****************************************************************************
#*****************************************************************************
library(lubridate)
library(dplyr)
library(stringr)
## Ghana
mnh30_gha <- read.csv(paste0(path_to_data, UploadDate, "_gha/", "mnh30.csv"))

colnames(mnh30_gha) = toupper(colnames(mnh30_gha))

mnh30_gha <- mnh30_gha %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH30, TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Ghana") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30, TYPE_VISIT)

## Zambia
mnh30_zam <- read.csv(paste0(path_to_data, UploadDate, "_zam/", "mnh30.csv"))

colnames(mnh30_zam) = toupper(colnames(mnh30_zam))

mnh30_zam <- mnh30_zam %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH30, TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Zambia") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30, TYPE_VISIT)

## Kenya
mnh30_ken <- read.csv(paste0(path_to_data, UploadDate, "_ke/", "MNH30.csv"))

colnames(mnh30_ken) = toupper(colnames(mnh30_ken))

mnh30_ken <- mnh30_ken %>% 
  select(MOMID, PREGID, INFANTID,VISIT_OBSSTDAT, INF_VISIT_MNH30, TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Kenya") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30,  TYPE_VISIT)


## Pakistan
mnh30_pak <- read.csv(paste0(path_to_data, UploadDate, "_pak/", "mnh30.csv"))

colnames(mnh30_pak) = toupper(colnames(mnh30_pak))

mnh30_pak <- mnh30_pak %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30 , TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Pakistan") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30, TYPE_VISIT)

# India-SAS
mnh30_sas <- read.csv(paste0(path_to_data, UploadDate, "_sas/", "mnh30.csv"))

colnames(mnh30_sas) = toupper(colnames(mnh30_sas))

## rename momid
mnh30_sas <- mnh30_sas %>% 
  filter(!is.na(INF_VISIT_MNH30)) %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30, TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "India-SAS") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30, TYPE_VISIT)

# India-CMC
mnh30_cmc <- read.csv(paste0(path_to_data, UploadDate, "_cmc/", "mnh30.csv"))

colnames(mnh30_cmc) = toupper(colnames(mnh30_cmc))

mnh30_cmc <- mnh30_cmc %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30 , 
         TYPE_VISIT) %>% 
  
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "India-CMC") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH30, TYPE_VISIT)

#*****************************************************************************
# GSED SF: Merge data & remove duplicates -----
#*****************************************************************************
mnh30_merged <- rbind(mnh30_gha,mnh30_zam, mnh30_ken, mnh30_pak, mnh30_cmc, mnh30_sas)  #

# # extract duplicate ids from main dataset
mnh30_merged <- mnh30_merged %>%
  mutate(SF_TYPE_VISIT = TYPE_VISIT) %>% 
  mutate(SF_VISIT_DATE = VISIT_OBSSTDAT) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID, SF_TYPE_VISIT) %>% 
  arrange(-desc(SF_VISIT_DATE)) %>%
  slice(1)

## test if there are any unparsed dates 
if (dim(mnh30_merged %>% filter(is.na(VISIT_OBSSTDAT)))[1] > 0) {
  test <- mnh30_merged %>% filter(is.na(VISIT_OBSSTDAT))
  cat("there are n =", dim(test)[1], "missing dates in the merged data")
  stop("review missing dates in mnh30_merged before moving on")
} else {
  print("all dates parsed in mnh30_merged correctly")
}

## test if there are any duplicates
if (dim(mnh30_merged %>% group_by(SITE,MOMID,PREGID, INFANTID, SF_TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1))[1] > 0) {
  test <- mnh30_merged %>% group_by(SITE,MOMID,PREGID, INFANTID, SF_TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
  cat("there are n =", dim(test)[1], "duplicates in mnh30_merged")
  stop("review duplicates in mnh30_merged before moving on")
} else {
  print("no duplicates in mnh30_merged")
}

table(mnh30_merged$TYPE_VISIT, mnh30_merged$SITE)
#*****************************************************************************
#*****************************************************************************
# FCI: import and data cleaning ----
#*****************************************************************************
#*****************************************************************************
# Ghana
mnh31_gha <- read.csv(paste0(path_to_data, UploadDate, "_gha/", "mnh31.csv"))

colnames(mnh31_gha) = toupper(colnames(mnh31_gha))

mnh31_gha <- mnh31_gha %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Ghana") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31, TYPE_VISIT) 

# Zambia
mnh31_zam <- read.csv(paste0(path_to_data, UploadDate, "_zam/", "mnh31.csv"))

colnames(mnh31_zam) = toupper(colnames(mnh31_zam))

mnh31_zam <- mnh31_zam %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH31, TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Zambia") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,  TYPE_VISIT) 

# Kenya 

mnh31_ken <- read.csv(paste0(path_to_data, UploadDate, "_ke/", "mnh31.csv"))

colnames(mnh31_ken) = toupper(colnames(mnh31_ken))

mnh31_ken <- mnh31_ken %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH31, TYPE_VISIT) %>%
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Kenya") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,  TYPE_VISIT) 

# India-CMC
mnh31_cmc <- read.csv(paste0(path_to_data, UploadDate, "_cmc/", "mnh31.csv"))

colnames(mnh31_cmc) = toupper(colnames(mnh31_cmc))

mnh31_cmc <- mnh31_cmc %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH31, TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "India-CMC") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,  TYPE_VISIT) 


# India-SAS
mnh31_sas <- read.csv(paste0(path_to_data, UploadDate, "_sas/", "mnh31.csv"))

colnames(mnh31_sas) = toupper(colnames(mnh31_sas))

mnh31_sas <- mnh31_sas %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH31, TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "India-SAS") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH31,  TYPE_VISIT) 

#*****************************************************************************
# FCI: merge data & remove duplicates -----
#*****************************************************************************
mnh31_merged <- rbind(mnh31_gha, mnh31_zam, mnh31_ken ,mnh31_cmc, mnh31_sas) # mnh31_sas

# extract duplicate ids from main dataset
mnh31_merged <- mnh31_merged %>% 
  mutate(FCI_TYPE_VISIT = TYPE_VISIT) %>% 
  mutate(FCI_VISIT_DATE = VISIT_OBSSTDAT) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID, FCI_TYPE_VISIT) %>% 
  arrange(-desc(FCI_VISIT_DATE)) %>%
  slice(1)

## test if there are any unparsed dates 
if (dim(mnh31_merged %>% filter(is.na(VISIT_OBSSTDAT)))[1] > 0) {
  test <- mnh31_merged %>% filter(is.na(VISIT_OBSSTDAT))
  cat("there are n =", dim(test)[1], "missing dates in the merged data")
  stop("review missing dates in mnh31_merged before moving on")
} else {
  print("all dates parsed correctly in mnh31_merged")
}

## test if there are any duplicates
if (dim(mnh31_merged %>% group_by(SITE,MOMID,PREGID, INFANTID, FCI_TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1))[1] > 0) {
  test <- mnh31_merged %>% group_by(SITE,MOMID,PREGID, INFANTID, FCI_TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
  cat("there are n =", dim(test)[1], "duplicates in mnh31_merged")
  stop("review duplicates in mnh31_merged before moving on")
} else {
  print("no duplicates in mnh31_merged")
}

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
  filter(!is.na(DOB)) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID) %>% 
  arrange(-desc(DOB)) %>%
  slice(1)


## remove duplicates in closeout form 
mnh24 <- mnh24 %>% 
  group_by(SITE, INFANTID) %>% 
  arrange(-desc(M24_CLOSE_DSSTDAT)) %>%
  slice(1)
 
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
  select(SITE, MOMID, PREGID, INFANTID, contains("DENOM"), contains("NUM"), contains("PASS"),contains("ONTIME_"),M24_CLOSE_DSDECOD,
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
# GSED LONG FORM (ReMIND substudy only)
# GSED LF: import data & data cleaning ----
# Note: SAS does not participait in this substudy
#*****************************************************************************
#*****************************************************************************
# Ghana
mnh32_gha <- read.csv(paste0(path_to_data, UploadDate, "_gha/", "mnh32.csv"))


colnames(mnh32_gha) = toupper(colnames(mnh32_gha))

mnh32_gha <- mnh32_gha %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Ghana") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT)

# Zambia 
mnh32_zam <- read.csv(paste0(path_to_data, UploadDate, "_zam/", "mnh32.csv"))

colnames(mnh32_zam) = toupper(colnames(mnh32_zam))

mnh32_zam <- mnh32_zam %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH32,TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Zambia") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT, INF_VISIT_MNH32,TYPE_VISIT)


# Kenya 
mnh32_ken <- read.csv(paste0(path_to_data, UploadDate, "_ke/", "mnh32.csv"))

colnames(mnh32_ken) = toupper(colnames(mnh32_ken))

mnh32_ken <- mnh32_ken %>% 
  select(MOMID, PREGID, INFANTID, INF_VISIT_MNH32, TYPE_VISIT, GENDER, VISIT_OBSSTDAT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Kenya") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT)



# Pakistan
mnh32_pak <- read.csv(paste0(path_to_data, UploadDate, "_pak/", "mnh32.csv"))

colnames(mnh32_pak) = toupper(colnames(mnh32_pak))

mnh32_pak <- mnh32_pak %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT) %>% 
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "Pakistan") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT)

# cmc
mnh32_cmc <- read.csv(paste0(path_to_data, UploadDate, "_cmc/", "mnh32.csv"))

colnames(mnh32_cmc) = toupper(colnames(mnh32_cmc))

mnh32_cmc <- mnh32_cmc %>% 
  select(MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT) %>%
  ## convert to date class
  mutate(VISIT_OBSSTDAT = smart_parse(VISIT_OBSSTDAT)) %>% 
  ## generate site indicator variable 
  mutate(SITE = "India-CMC") %>% 
  select(SITE, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT,INF_VISIT_MNH32, TYPE_VISIT)

#*****************************************************************************
# GSED LF: Merge data & remove duplicates ----
#*****************************************************************************
mnh32_merged <- rbind(mnh32_gha,mnh32_cmc, mnh32_zam, mnh32_ken,mnh32_pak)

# rename variables to make merging easier
mnh32_merged <- mnh32_merged %>% 
  mutate(LF_TYPE_VISIT = TYPE_VISIT) %>% 
  mutate(LF_VISIT_DATE = VISIT_OBSSTDAT) %>% 
  ## remove NA visit types 
  filter(!is.na(LF_TYPE_VISIT)) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID, LF_TYPE_VISIT) %>% 
  arrange(-desc(LF_VISIT_DATE)) %>%
  slice(1)

## test if there are any unparsed dates 
if (dim(mnh32_merged %>% filter(is.na(VISIT_OBSSTDAT)))[1] > 0) {
  test <- mnh32_merged %>% filter(is.na(VISIT_OBSSTDAT))
  cat("there are n =", dim(test)[1], "missing dates in the merged data")
  stop("review missing dates in mnh32_merged before moving on")
} else {
  print("all dates parsed in mnh32_merged correctly")
}

## if there are missing dates, remove them from the dataset 

## test if there are any duplicates
if (dim(mnh32_merged %>% group_by(SITE,MOMID,PREGID, INFANTID, LF_TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1))[1] > 0) {
  test <- mnh32_merged %>% group_by(SITE,MOMID,PREGID, INFANTID, LF_TYPE_VISIT) %>% mutate(n=n()) %>% filter(n>1)
  cat("there are n =", dim(test)[1], "duplicates in mnh32_merged")
  stop("review duplicates in mnh32_merged before moving on")
} else {
  print("no duplicates in mnh32_merged")
}


table(mnh30_merged$INF_VISIT_MNH30,mnh30_merged$TYPE_VISIT,  mnh30_merged$SITE)
table(mnh31_merged$INF_VISIT_MNH31,mnh31_merged$TYPE_VISIT, mnh31_merged$SITE)
table(mnh32_merged$INF_VISIT_MNH32, mnh32_merged$TYPE_VISIT, mnh32_merged$SITE)
#*****************************************************************************
# ReMIND Cohort Data processing for monitoring report tables -----

## Date expansion launch for GSED SF @ 12 months
# Ghana: 22-May-2024
# Kenya: 29-May-2024
# Pakistan: 30-Jan-2024
# Zambia: 20-Mar-2024
#*****************************************************************************

inf_date <- InfData_Wide_to_merge %>%
  # right_join(aim3ids, by= c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  right_join(aim3_final_infants, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  select(-M09_DELIV_DSSTDAT) %>%
  # only include sitest that we have data for
  filter(SITE %in% c("Pakistan", "Kenya", "Ghana", "Zambia", "India-CMC")) %>%
  filter(!is.na(DOB)) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID) %>% 
  arrange(-desc(DOB)) %>%
  slice(1)

## extract aim 3 IDs
remind_merged_enrolled <- aim3_final_infants %>% 
  ## only include those who we have valid enrollment information for mom
  left_join(mat_enroll %>% select(SITE, MOMID, PREGID, ENROLL), by = c("SITE","MOMID", "PREGID")) %>% 
  ungroup() %>% 
  left_join(InfData_Wide_to_merge %>% select(SITE, MOMID, PREGID, INFANTID, DOB), by = c("SITE", "MOMID", "PREGID", "INFANTID"))

gsed_sf_wide_remind <- remind_merged_enrolled %>% 
  ## merge in infant date (birth outcome)
  left_join(mnh30_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))  %>% 
  mutate(SF_TYPE_VISIT = as.numeric(SF_TYPE_VISIT)) %>% 
  group_by(SITE, MOMID, PREGID, INFANTID, SF_TYPE_VISIT) %>%
  # address duplicates (if exists, take the first instance)
  arrange(-desc(VISIT_OBSSTDAT)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup() %>% 
  select(SITE, MOMID, PREGID, INFANTID, DOB, PREG_OUTCOME,REMIND_ENROLLMENT_STATUS,  SF_TYPE_VISIT, SF_VISIT_DATE, INF_VISIT_MNH30) %>% 
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
  # on time windows without the +2 weeks 
  mutate(ONTIME_3MO = DOB + 111) %>% # 15 weeks 6 days
  mutate(ONTIME_6MO = DOB + 202) %>% # 28 weeks 6 days
  mutate(ONTIME_12MO = DOB + 330) %>% # 54 weeks 6 days
  mutate(PASS_3MO = case_when(ymd(UploadDate) > ONTIME_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > ONTIME_6MO ~ 1, TRUE ~ 0),
  ) %>% 
## merge in closeout form 
left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_AIM3_3MO_DENOM_SF = case_when((PASS_3MO == 1  & REMIND_ENROLLMENT_STATUS == 1 & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_3MO))  ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_6MO_DENOM_SF = case_when((PASS_6MO == 1 & REMIND_ENROLLMENT_STATUS == 1 &  
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_6MO)) ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_12MO_DENOM_SF = case_when( REMIND_ENROLLMENT_STATUS == 1 & ((ymd(UploadDate) > ONTIME_12MO |
                                              (M24_CLOSE_DSDECOD==1))) ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_AIM3_SF_3MO_NUM = case_when(REMIND_ENROLLMENT_STATUS == 1 & INF_VISIT_MNH30_14 %in% c(1,2,3) & SF_TYPE_VISIT_14 == 14 & VC_AIM3_3MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 ), 
         VC_AIM3_SF_6MO_NUM = case_when(REMIND_ENROLLMENT_STATUS == 1 & INF_VISIT_MNH30_11 %in% c(1,2,3) & SF_TYPE_VISIT_11 == 11 & VC_AIM3_6MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 ),
         VC_AIM3_SF_12MO_NUM = case_when(REMIND_ENROLLMENT_STATUS == 1 & INF_VISIT_MNH30_12 %in% c(1,2,3) & SF_TYPE_VISIT_12 == 12 & VC_AIM3_12MO_DENOM_SF ==1 ~ 1, TRUE ~ 0 )
  ) 


gsed_lf_wide_remind <- remind_merged_enrolled %>% 
  ## merge in infant date (birth outcome)
  left_join(mnh32_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))  %>% 
  group_by(SITE, MOMID, PREGID, INFANTID, LF_TYPE_VISIT) %>%
  # address duplicates (if exists, take the first instance)
  arrange(-desc(VISIT_OBSSTDAT)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup() %>% 
  select(SITE, MOMID, PREGID, INFANTID, DOB,PREG_OUTCOME,REMIND_ENROLLMENT_STATUS, LF_TYPE_VISIT, LF_VISIT_DATE, INF_VISIT_MNH32) %>% 
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
  # on time windows without the +2 weeks 
  mutate(ONTIME_3MO = DOB + 111) %>% # 15 weeks 6 days
  mutate(ONTIME_6MO = DOB + 202) %>% # 28 weeks 6 days
  mutate(ONTIME_12MO = DOB + 330) %>% # 54 weeks 6 days
  mutate(PASS_3MO = case_when(ymd(UploadDate) > ONTIME_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > ONTIME_6MO ~ 1, TRUE ~ 0),
  ) %>% 

## merge in closeout form 
left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD) %>%  filter(INFANTID != "Z3-025-1136-B"), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_AIM3_3MO_DENOM_LF = case_when((PASS_3MO == 1  & REMIND_ENROLLMENT_STATUS == 1 & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_3MO) )
                                      ##      | (INF_VISIT_MNH32_14 %in% c(1,2,3) & LF_TYPE_VISIT_14 == 14)  ## or completed the form
                                            
                                            ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_6MO_DENOM_LF = case_when(PASS_6MO == 1 & REMIND_ENROLLMENT_STATUS == 1 & 
                                            (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_6MO) ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_12MO_DENOM_LF = case_when(REMIND_ENROLLMENT_STATUS == 1 & (ymd(UploadDate) > ONTIME_12MO |
                                              (M24_CLOSE_DSDECOD==1)) ~ 1, TRUE ~ 0 ),
         
         
  )  %>% 
  
  mutate(VC_AIM3_LF_3MO_NUM = case_when(REMIND_ENROLLMENT_STATUS == 1 & INF_VISIT_MNH32_14 %in% c(1,2,3) & LF_TYPE_VISIT_14 == 14 & VC_AIM3_3MO_DENOM_LF ==1 ~ 1, TRUE ~ 0), 
         VC_AIM3_LF_6MO_NUM = case_when(REMIND_ENROLLMENT_STATUS == 1 & INF_VISIT_MNH32_11 %in% c(1,2,3) & LF_TYPE_VISIT_11 == 11 & VC_AIM3_6MO_DENOM_LF ==1 ~ 1, TRUE ~ 0),
         VC_AIM3_LF_12MO_NUM = case_when(REMIND_ENROLLMENT_STATUS == 1 & INF_VISIT_MNH32_12 %in% c(1,2,3) & LF_TYPE_VISIT_12 == 12 & VC_AIM3_12MO_DENOM_LF ==1 ~ 1, TRUE ~ 0)
  )


gsed_fci_wide_remind <- remind_merged_enrolled %>% 
  ## merge in infant date (birth outcome)
  left_join(mnh31_merged, by = c("SITE", "MOMID", "PREGID", "INFANTID"))  %>% 
  # right_join(remind_merged_enrolled, by = c("SITE", "MOMID", "PREGID"))  %>% 
  mutate(FCI_TYPE_VISIT = as.numeric(FCI_TYPE_VISIT)) %>% 
  # address duplicates (if exists, take the first instance)
  group_by(SITE, MOMID, PREGID, INFANTID, FCI_TYPE_VISIT) %>%
  arrange(-desc(VISIT_OBSSTDAT)) %>% 
  slice(1) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(-n) %>% 
  ungroup() %>% 
  select(SITE, MOMID, PREGID, INFANTID, DOB,PREG_OUTCOME,REMIND_ENROLLMENT_STATUS,  FCI_TYPE_VISIT, FCI_VISIT_DATE, INF_VISIT_MNH31) %>% 
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
  # on time windows without the +2 weeks 
  mutate(ONTIME_3MO = DOB + 111) %>% # 15 weeks 6 days
  mutate(ONTIME_6MO = DOB + 202) %>% # 28 weeks 6 days
  mutate(ONTIME_12MO = DOB + 330) %>% # 54 weeks 6 days
  # select(SITE, MOMID, PREGID, INFANTID, contains("START"), contains("LATE")) %>% 
  # distinct() %>% 
  mutate(PASS_3MO = case_when(ymd(UploadDate) > ONTIME_3MO ~ 1, TRUE ~ 0),
         PASS_6MO = case_when(ymd(UploadDate) > ONTIME_6MO ~ 1, TRUE ~ 0),
  ) %>% 
  ## merge in closeout form 
  left_join(mnh24 %>% select(SITE, MOMID, PREGID, INFANTID, M24_CLOSE_DSSTDAT, M24_CLOSE_DSDECOD), by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% ## duplicate in infant closeout (remove id)
  # generate numerator and denominators for 3mo window (both short form and long form have the same denominator)
  mutate(VC_AIM3_3MO_DENOM_FCI = case_when((PASS_3MO == 1  & REMIND_ENROLLMENT_STATUS == 1 & 
                                             (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_3MO)) ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_6MO_DENOM_FCI = case_when((PASS_6MO == 1 & REMIND_ENROLLMENT_STATUS == 1 & 
                                             (is.na(M24_CLOSE_DSSTDAT) | M24_CLOSE_DSSTDAT > ONTIME_6MO)) ~ 1, TRUE ~ 0 ),
         
         VC_AIM3_12MO_DENOM_FCI = case_when(REMIND_ENROLLMENT_STATUS == 1 & ((ymd(UploadDate) > ONTIME_12MO |
                                               (M24_CLOSE_DSDECOD==1))) ~ 1, TRUE ~ 0 ),
  )  %>% 
  
  mutate(VC_AIM3_FCI_3MO_NUM = case_when(REMIND_ENROLLMENT_STATUS == 1 & INF_VISIT_MNH31_14 %in% c(1,2,3) & FCI_TYPE_VISIT_14 == 14 & VC_AIM3_3MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 ), 
         VC_AIM3_FCI_6MO_NUM = case_when(REMIND_ENROLLMENT_STATUS == 1 & INF_VISIT_MNH31_11 %in% c(1,2,3) & FCI_TYPE_VISIT_11 == 11 & VC_AIM3_6MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 ),
         VC_AIM3_FCI_12MO_NUM = case_when(REMIND_ENROLLMENT_STATUS == 1 & INF_VISIT_MNH31_12 %in% c(1,2,3) & FCI_TYPE_VISIT_12 == 12 & VC_AIM3_12MO_DENOM_FCI ==1 ~ 1, TRUE ~ 0 )
         
  ) 


gsed_aim3_data <- gsed_sf_wide_remind %>% 
  select(SITE, MOMID, PREGID, INFANTID, contains("DENOM"), contains("NUM"),PREG_OUTCOME, REMIND_ENROLLMENT_STATUS,
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

  mutate(VC_AIM3_3MO_DENOM = case_when(REMIND_ENROLLMENT_STATUS == 1  & (VC_AIM3_3MO_DENOM_SF==1 | VC_AIM3_3MO_DENOM_FCI==1 | VC_AIM3_3MO_DENOM_LF==1) & EXPANSION_3MO==1 ~ 1, TRUE ~ 0),
         VC_AIM3_6MO_DENOM = case_when(REMIND_ENROLLMENT_STATUS == 1  & (VC_AIM3_6MO_DENOM_SF==1 | VC_AIM3_6MO_DENOM_FCI==1 | VC_AIM3_6MO_DENOM_LF==1) & EXPANSION_6MO==1 ~ 1, TRUE ~ 0), 
         VC_AIM3_12MO_DENOM = case_when(REMIND_ENROLLMENT_STATUS == 1  & (VC_AIM3_12MO_DENOM_SF==1 | VC_AIM3_12MO_DENOM_FCI==1  | VC_AIM3_12MO_DENOM_LF==1) & EXPANSION_12MO==1~ 1, TRUE ~ 0),
  ) %>% 
  mutate(REMAPP_AIM3_ENROLL=1) %>% 
  group_by(SITE, MOMID, PREGID) %>% 
  mutate(MOM_REMAPP_AIM3_ENROLL= row_number()) %>% 
  ungroup()

save(gsed_aim3_data, file= paste0(
  path_to_save, "gsed_aim3_data",".RData",sep = ""))
