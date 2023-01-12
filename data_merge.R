#*****************************************************************************
#*Read cleaned data
#*Revise column type for site, merge and save data (mnh##.csv)
#*Extract variables for monitoring report and change the smaller data from long to wide
#*Merge all data into two datasets: one for mom, one for infant
#*Save data (matData.csv, neoData.csv)
#*****************************************************************************
rm(list = ls())
library(tidyverse)
library(lubridate)

#*****************************************************************************
#*Read cleaned data
#*****************************************************************************
PA_mnh00 <- read_csv("cleaned_data/PA_mnh00.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh01 <- read_csv("cleaned_data/PA_mnh01.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh02 <- read_csv("cleaned_data/PA_mnh02.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh03 <- read_csv("cleaned_data/PA_mnh03.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh04 <- read_csv("cleaned_data/PA_mnh04.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh05 <- read_csv("cleaned_data/PA_mnh05.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh06 <- read_csv("cleaned_data/PA_mnh06.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh07 <- read_csv("cleaned_data/PA_mnh07.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh08 <- read_csv("cleaned_data/PA_mnh08.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh19 <- read_csv("cleaned_data/PA_mnh19.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh25 <- read_csv("cleaned_data/PA_mnh25.csv", col_names=TRUE, show_col_types = FALSE)
PA_mnh26 <- read_csv("cleaned_data/PA_mnh26.csv", col_names=TRUE, show_col_types = FALSE)

KE_mnh00 <- read_csv("cleaned_data/KE_mnh00.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh01 <- read_csv("cleaned_data/KE_mnh01.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh02 <- read_csv("cleaned_data/KE_mnh02.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh03 <- read_csv("cleaned_data/KE_mnh03.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh04 <- read_csv("cleaned_data/KE_mnh04.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh05 <- read_csv("cleaned_data/KE_mnh05.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh06 <- read_csv("cleaned_data/KE_mnh06.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh07 <- read_csv("cleaned_data/KE_mnh07.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh08 <- read_csv("cleaned_data/KE_mnh08.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh19 <- read_csv("cleaned_data/KE_mnh19.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh25 <- read_csv("cleaned_data/KE_mnh25.csv", col_names=TRUE, show_col_types = FALSE)
KE_mnh26 <- read_csv("cleaned_data/KE_mnh26.csv", col_names=TRUE, show_col_types = FALSE)

#*****************************************************************************
#*Revise column type for site, merge and save data (mnh##.csv)
#*****************************************************************************

#format date
sf<-stamp_date("16-06-2022")

#******M00
#prepare site data
PA_mnh00 <- PA_mnh00 %>% 
  mutate(
    M00_SCRN_OBSSTDAT = as.character(sf(M00_SCRN_OBSSTDAT)),
    M00_PRV_MOMID = as.character(M00_PRV_MOMID),
    M00_FORMCOMPLDAT_MNH00 = as.character(sf(M00_FORMCOMPLDAT_MNH00))
    )

KE_mnh00 <- KE_mnh00 %>% 
  mutate(
    SCRNID = as.character(SCRNID),
    M00_SCRN_OBSSTDAT = as.character(sf(mdy_hm(M00_SCRN_OBSSTDAT))),
    M00_BRTHDAT = as.character(sf(mdy_hm(M00_BRTHDAT))),
    M00_OTHR_REASON_IEORRES = as.character(M00_OTHR_REASON_IEORRES)
) 

#merge site data
mnh00 <- PA_mnh00 %>% 
  bind_rows(KE_mnh00)

#******M01
KE_mnh01 <- KE_mnh01 %>% 
  mutate(
    SCRNID = as.character(SCRNID),
    M01_US_OHOSTDAT = as.character(sf(mdy_hm(M01_US_OHOSTDAT)))
  ) 

#merge site data
mnh01 <- PA_mnh01 %>% 
  bind_rows(KE_mnh01)

#******M02
KE_mnh02 <- KE_mnh02 %>% 
  mutate(
    SCRNID = as.character(SCRNID),
    M02_SCRN_OBSSTDAT = as.character(sf(mdy_hm(M02_SCRN_OBSSTDAT)))
  ) 

#merge site data
mnh02 <- PA_mnh02 %>% 
  bind_rows(KE_mnh02)

#******M03
PA_mnh03 <- PA_mnh03 %>% 
  mutate(
    M03_OWN_RENT_SPFY_SCORRES = as.character(M03_OWN_RENT_SPFY_SCORRES),
    M03_LAND_USE_SPFY_FCORRES_88 = as.character(M03_LAND_USE_SPFY_FCORRES_88)
  ) 

KE_mnh03 <- KE_mnh03 %>% 
  mutate(
    M03_SD_OBSSTDAT = as.character(sf(mdy_hm(M03_SD_OBSSTDAT))),
    M03_HEAD_HH_FCORRES = as.character(M03_HEAD_HH_FCORRES), #double is the correct format, should ask Pakistan to change format
    M03_CHEW_OECOCCUR = as.character(M03_CHEW_OECOCCUR) #double is the correct format, should ask Pakistan to change format
  ) 

#merge site data
mnh03 <- PA_mnh03 %>% 
  bind_rows(KE_mnh03)

#******M04
PA_mnh04 <- PA_mnh04 %>% 
  mutate(
    M04_STI_OTHR_CMTRT = as.character(M04_STI_OTHR_CMTRT),
    M04_TB_SPFY_CMTRT = as.character(M04_TB_SPFY_CMTRT), 
    M04_STILLBIRTH_CT_RPORRES = as.character(M04_STILLBIRTH_CT_RPORRES) #Should be numeric. Delete this line once Kenya data fixed
  ) 

KE_mnh04 <- KE_mnh04 %>% 
  mutate(
    M04_ANC_OBSSTDAT = as.character(sf(mdy_hm(M04_ANC_OBSSTDAT)))
  ) 

#merge site data
mnh04 <- PA_mnh04 %>% 
  bind_rows(KE_mnh04)

#******M05
KE_mnh05 <- KE_mnh05 %>% 
  mutate(
    M05_ANT_PEDAT = as.character(sf(mdy_hm(M05_ANT_PEDAT)))
  ) 

mnh05 <- PA_mnh05 %>% 
  bind_rows(KE_mnh05)

#******M06
KE_mnh06 <- KE_mnh06 %>% 
  mutate(
    M06_DIAG_VSDAT = as.character(sf(mdy_hm(M06_DIAG_VSDAT)))
  ) 

mnh06 <- PA_mnh06 %>% 
  bind_rows(KE_mnh06)

#******M07
KE_mnh07 <- KE_mnh07 %>% 
  mutate(
    M07_MAT_SPEC_COLLECT_DAT = as.character(sf(mdy_hm(M07_MAT_SPEC_COLLECT_DAT)))
  ) 

mnh07 <- PA_mnh07 %>% 
  bind_rows(KE_mnh07)

#******M08
KE_mnh08 <- KE_mnh08 %>% 
  mutate(
    M08_LBSTDAT = as.character(sf(mdy_hm(M08_LBSTDAT)))
  ) 

mnh08 <- PA_mnh08 %>% 
  bind_rows(KE_mnh08)

#******M09

#******M10

#******M11

#******M12

#******M13

#******M14

#******M15

#******M16

#******M17

#******M18

#******M19
KE_mnh19 <- KE_mnh19 %>% 
mutate(
  M19_OBSSTDAT = as.character(sf(mdy_hm(M19_OBSSTDAT)))
) 
mnh19 <- PA_mnh19 %>% 
  bind_rows(KE_mnh19)

#******M20

#******M21

#******M22

#******M23

#******M24

#******M25
KE_mnh25 <- KE_mnh25 %>% 
  mutate(
    M25_OBSSTDAT = as.character(sf(mdy_hm(M25_OBSSTDAT)))
  ) 

mnh25 <- PA_mnh25 %>% 
  bind_rows(KE_mnh25)

#******M26
mnh26 <- PA_mnh26 %>% 
  bind_rows(KE_mnh26)

#************save data (mnh##.csv)
write_csv(mnh00, file = "cleaned_data/mnh00.csv", col_names = TRUE, "")
write_csv(mnh01, file = "cleaned_data/mnh01.csv", col_names = TRUE, "")
write_csv(mnh02, file = "cleaned_data/mnh02.csv", col_names = TRUE, "")
write_csv(mnh03, file = "cleaned_data/mnh03.csv", col_names = TRUE, "")
write_csv(mnh04, file = "cleaned_data/mnh04.csv", col_names = TRUE, "")
write_csv(mnh05, file = "cleaned_data/mnh05.csv", col_names = TRUE, "")
write_csv(mnh06, file = "cleaned_data/mnh06.csv", col_names = TRUE, "")
write_csv(mnh07, file = "cleaned_data/mnh07.csv", col_names = TRUE, "")
write_csv(mnh08, file = "cleaned_data/mnh08.csv", col_names = TRUE, "")
write_csv(mnh19, file = "cleaned_data/mnh19.csv", col_names = TRUE, "")
write_csv(mnh25, file = "cleaned_data/mnh25.csv", col_names = TRUE, "")
write_csv(mnh26, file = "cleaned_data/mnh26.csv", col_names = TRUE, "")


#**************************************************************************************
#*Extract variables for monitoring report 
#**************************************************************************************

# update/add/delete variable names in the varNames_sheet.xlsx (check if the var is multiple or singe when add)
varNames_sheet <- readxl::read_excel("varNames_sheet.xlsx")

# function to extract variables from the MNH forms
dataExtract_func <- function(FormId, varNames_sheet){
  
  # forms are saved in path data/ as mnhxx.csv
  formPath <- paste0("cleaned_data/mnh", FormId, ".csv")
  formData <- read_csv(formPath, col_names = TRUE, show_col_types = FALSE)
  
  # requested var. names
  requested_varNames <- str_subset(varNames_sheet$name, paste0("^M", FormId))
  # all variable names in the form
  form_varNames <- names(formData)
  
  # common var. names
  common_varNames <- c("SCRNID", "MOMID", "PREGID", "INFANTID", "SITE")
                       
  # index of the extracted var. in the form variables
  varExtract_idx <- toupper(form_varNames) %in% toupper(c(requested_varNames, common_varNames))
  # extracted variables
  varExtract <- form_varNames[varExtract_idx]
  
  # remove the prefix of SCRNID, MOMID, PREGID, INFANTID
  varPrefix <- paste0("M",FormId, "_", c("SCRNID", "MOMID", "PREGID", "INFANTID", "SITE"))
  varPrefix_removed <- c("SCRNID", "MOMID", "PREGID", "INFANTID", "SITE")
  varPrefix_idx <- varPrefix %in% varExtract

  # subset data
  dataExtract <- formData[, varExtract] %>% 
    rename_at(vars(varPrefix[varPrefix_idx]), ~varPrefix_removed[varPrefix_idx]) %>% 
    rename_with(~toupper(.x), .cols = everything())
  
  # return
  dataExtract
}

# apply the function on each form
# requested formId
requested_formId <- unique(str_match(varNames_sheet$name, "^M(\\d+)_")[,2])
dataExtract_list <- lapply(requested_formId, 
                           function(id) dataExtract_func(FormId = id, varNames_sheet))
names(dataExtract_list) <- paste0("M", requested_formId)
save(dataExtract_list, file = "derived_data/dataExtract_list.rda")

#**************************************************************************************
#* Tidy each form
#* 1. Outlier are not cleaned at this point
#* 2. Non-routine visit are removed temporarily
#**************************************************************************************
# load data
load("derived_data/dataExtract_list.rda")
# create empty list to save the processed data
dataWide_list <- list()

#******M00
mnh00_long <- dataExtract_list[["M00"]]

mnh00_wide <- mnh00_long %>% 
  subset(!is.na(SCRNID) & #MOMID and PREGID 
         !is.na(M00_FORMCOMPLDAT_MNH00) & !is.na(M00_SCRN_OBSSTDAT)) %>% 
  distinct()

dataWide_list$M00 <- mnh00_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M01 
mnh01_long <- dataExtract_list[["M01"]]

#function: get min positive value
minpositive = function(x) {
  y <- x[x>0]
  if (length(y) >0) {
    return(min(y))}
  else {return(x)}
}

#prepare data: add VISIT_TOT and VISIT_TYPE
mnh01_wide0 <- mnh01_long %>% 
  subset(!is.na(SCRNID) & 
           !is.na(M01_FORMCOMPLDAT_MNH01) & !is.na(M01_US_OHOSTDAT)) %>% 
  rowwise() %>% 
  mutate(GAUSGSCRNDAYS=max(
    (M01_US_GA_WKS_AGE_FTS1 * 7 + M01_US_GA_DAYS_AGE_FTS1),
    (M01_US_GA_WKS_AGE_FTS2 * 7 + M01_US_GA_DAYS_AGE_FTS2),
    (M01_US_GA_WKS_AGE_FTS3 * 7 + M01_US_GA_DAYS_AGE_FTS3),
    (M01_US_GA_WKS_AGE_FTS4 * 7 + M01_US_GA_DAYS_AGE_FTS4))
  ) %>% 
group_by(SCRNID, MOMID, PREGID, SITE) %>% 
  mutate(
    baselineGA = case_when(
      M01_US_VISIT == 1 ~ GAUSGSCRNDAYS,
      (M01_US_VISIT == 2 | M01_US_VISIT == 77) & max(GAUSGSCRNDAYS) > 0 ~ minpositive(GAUSGSCRNDAYS),
      (M01_US_VISIT == 2 | M01_US_VISIT == 77) & max(GAUSGSCRNDAYS) < 0 ~ -9
    ),
    baselinedate = case_when(
      M01_US_VISIT == 1 ~ dmy(M01_US_OHOSTDAT),
      M01_US_VISIT == 2 | M01_US_VISIT == 77 ~ min(dmy(M01_US_OHOSTDAT))
    ),
    M01_VISIT_TYPE = case_when(
      M01_US_VISIT == 1 ~ 1,
      (M01_US_VISIT == 2 | M01_US_VISIT == 77) & 
        ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) >= 18 & 
        ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) < 23 ~ 2, 
      (M01_US_VISIT == 2 | M01_US_VISIT == 77) & 
        ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) >= 26 & 
        ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) < 31 ~ 3,   
      (M01_US_VISIT == 2 | M01_US_VISIT == 77) & 
        ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) >= 31 & 
        ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) < 34 ~ 4,   
      (M01_US_VISIT == 2 | M01_US_VISIT == 77) & 
        ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) >= 34 & 
        ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) < 39 ~ 5, 
      TRUE ~ 13)
    ) %>% 
  subset(M01_VISIT_TYPE != 13) %>% #remove not routine visit
  mutate(M01_VISIT_TOT = length(M01_VISIT_TYPE)) %>% 
select(-baselineGA, -baselinedate) %>% ungroup()

#select screen only variables (+IDs and M01_US_VISIT, M01_VISIT_TOT, M01_VISIT_TYPE) 
mnh01_wide1 <- mnh01_wide0 %>% 
  subset(M01_US_VISIT == 1) %>% 
  select(-c(M01_US_OHOSTDAT, M01_US_VISIT, M01_VISIT_TYPE, M01_MAT_VITAL_MNH01, M01_MAT_VISIT_MNH01, 
            M01_MAT_VISIT_OTHR_MNH01, M01_FORMCOMPLDAT_MNH01)) 

#select all us visit vars 
mnh01_wide2 <- mnh01_wide0 %>% 
  select(SCRNID, MOMID, PREGID, SITE, 
           M01_US_OHOSTDAT, M01_US_VISIT, M01_VISIT_TYPE, M01_MAT_VITAL_MNH01, 
           M01_MAT_VISIT_MNH01, M01_MAT_VISIT_OTHR_MNH01, M01_FORMCOMPLDAT_MNH01) %>% 
  subset(
       !(SCRNID == "AE-3229" & M01_VISIT_TYPE == 2) &
       !(SCRNID == "EF-2945" & M01_VISIT_TYPE == 2) &
       !(SCRNID == "FI-1451" & M01_VISIT_TYPE == 2) &
       !(SCRNID == "JQ-1434" & M01_VISIT_TYPE == 2)
    ) %>% ####this part will be removed later, these cases are duplicated at same visit window, wait for decision on this
  pivot_wider(names_from = M01_VISIT_TYPE,
              values_from = names(mnh01_wide0)[!names(mnh01_wide0) %in% c(names(mnh01_wide1))],
              names_glue = "{.value}_V{M01_VISIT_TYPE}") 

#combine screen and all us visit
mnh01_wide3 <- mnh01_wide1 %>% 
  full_join(mnh01_wide2, by = c("SCRNID", "MOMID", "PREGID", "SITE")) %>% 
  distinct()

dataWide_list$M01 <- mnh01_wide3

save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M02
mnh02_long <- dataExtract_list[["M02"]]

mnh02_wide <- mnh02_long %>% 
  subset(!is.na(SCRNID) &  
           !is.na(M02_FORMCOMPLDAT_MNH02) & !is.na(M02_SCRN_OBSSTDAT)) %>% 
  distinct()

dataWide_list$M02 <- mnh02_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M03 
mnh03_long <- dataExtract_list[["M03"]]

mnh03_wide <- mnh03_long %>% 
  subset(!is.na(MOMID)  & !is.na(PREGID) &
           !is.na(M03_FORMCOMPLDAT_MNH03) & !is.na(M03_SD_OBSSTDAT)) %>% 
  distinct()

dataWide_list$M03 <- mnh03_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M04
mnh04_long <- dataExtract_list[["M04"]]

mnh04_wide0 <- mnh04_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M04_FORMCOMPLDAT_MNH04) & !is.na(M04_ANC_OBSSTDAT)) %>% 
  distinct() %>% 
  subset(M04_TYPE_VISIT != 13) %>% #remove not routine visit
  group_by(MOMID, PREGID) %>% 
  mutate(
    M04_VISIT_TOT = length(M04_TYPE_VISIT),
    M04_VISIT_TYPE = M04_TYPE_VISIT) %>%
  ungroup() %>% 
  select(-c(M04_APH_COMP_RPTEST_1, M04_APH_COMP_RPTEST_2, M04_APH_COMP_RPTEST_3, M04_APH_COMP_RPTEST_88, M04_APH_COMP_RPTEST_99, 
            M04_PPH_COMP_RPORRES_1, M04_PPH_COMP_RPORRES_2, M04_PPH_COMP_RPORRES_3, M04_PPH_COMP_RPORRES_4, M04_PPH_COMP_RPORRES_88, M04_PPH_COMP_RPORRES_99)) 

# convert long-format to wide-format
mnh04_wide1 <- mnh04_wide0 %>% 
  pivot_wider(names_from = M04_TYPE_VISIT,
              values_from = names(mnh04_wide0)[!names(mnh04_wide0) %in% c("MOMID", "PREGID", "SITE", "M04_VISIT_TOT", "M04_TYPE_VISIT")],
              names_glue = "{.value}_V{M04_TYPE_VISIT}") 

dataWide_list$M04 <- mnh04_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M05
mnh05_long <- dataExtract_list[['M05']]

mnh05_wide0 <- mnh05_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M05_FORMCOMPLDAT_MNH05) & !is.na(M05_ANT_PEDAT)) %>% 
  distinct() %>% 
  subset(M05_TYPE_VISIT != 13) %>% #remove not routine visit
  group_by(MOMID, PREGID) %>% 
  mutate(
    M05_VISIT_TOT = length(M05_TYPE_VISIT),
    M05_VISIT_TYPE = M05_TYPE_VISIT) %>% 
  ungroup() 

# long to wide 
mnh05_wide1 <- mnh05_wide0 %>% 
  pivot_wider(names_from = M05_TYPE_VISIT,
              values_from = names(mnh05_wide0)[!names(mnh05_wide0) %in% c("MOMID", "PREGID", "SITE", "M05_VISIT_TOT", "M05_TYPE_VISIT")],
              names_glue = "{.value}_V{M05_TYPE_VISIT}")

dataWide_list$M05 <- mnh05_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M06
mnh06_long <- dataExtract_list[['M06']]

## remove duplicates or missing key variables
mnh06_wide0 <- mnh06_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M06_FORMCOMPLDAT_MNH06) & !is.na(M06_DIAG_VSDAT)) %>% 
  distinct() %>% 
  subset(M06_TYPE_VISIT != 13) %>% #remove not routine visit
  group_by(MOMID, PREGID) %>% 
  mutate(
 M06_VISIT_TOT = length(M06_TYPE_VISIT),
 M06_VISIT_TYPE = M06_TYPE_VISIT) %>% 
  ungroup()

# long to wide
mnh06_wide1 <- mnh06_wide0 %>% 
  pivot_wider(names_from = M06_TYPE_VISIT,
              values_from = names(mnh06_wide0)[!names(mnh06_wide0) %in% c("MOMID", "PREGID", "SITE", "M06_VISIT_TOT", "M06_TYPE_VISIT")],
              names_glue = "{.value}_V{M06_TYPE_VISIT}")

dataWide_list$M06 <- mnh06_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M07
mnh07_long <- dataExtract_list[['M07']]

## remove duplicates or missing key variables
mnh07_wide0 <- mnh07_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M07_FORMCOMPLDAT_MNH07) & !is.na(M07_MAT_SPEC_COLLECT_DAT)) %>% 
  distinct() %>% 
  subset(M07_MAT_SPEC_COLLECT_VISIT != 13) %>% #remove not routine visit
  group_by(MOMID, PREGID) %>% 
  mutate(
    M07_VISIT_TOT = length(M07_MAT_SPEC_COLLECT_VISIT),
    M07_VISIT_TYPE = M07_MAT_SPEC_COLLECT_VISIT) %>% 
  ungroup()

# long to wide
mnh07_wide1 <- mnh07_wide0 %>% 
  pivot_wider(names_from = M07_MAT_SPEC_COLLECT_VISIT,
              values_from = names(mnh07_wide0)[!names(mnh07_wide0) %in% c("MOMID", "PREGID", "SITE", "M07_VISIT_TOT", "M07_MAT_SPEC_COLLECT_VISIT")],
              names_glue = "{.value}_V{M07_MAT_SPEC_COLLECT_VISIT}")

dataWide_list$M07 <- mnh07_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M08
mnh08_long <- dataExtract_list[['M08']]

## remove duplicates or missing key variables
mnh08_wide0 <- mnh08_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M08_FORMCOMPLDAT_MNH08) & !is.na(M08_LBSTDAT)) %>% 
  distinct() %>% 
  subset(M08_VISIT_LBSTDAT != 13) %>% #remove not routine visit
  group_by(MOMID, PREGID) %>% 
  mutate(
    M08_VISIT_TOT = length(M08_VISIT_LBSTDAT),
    M08_VISIT_TYPE = M08_VISIT_LBSTDAT) %>% 
  ungroup()

# long to wide
mnh08_wide1 <- mnh08_wide0 %>% 
  pivot_wider(names_from = M08_VISIT_LBSTDAT,
              values_from = names(mnh08_wide0)[!names(mnh08_wide0) %in% c("MOMID", "PREGID", "SITE", "M08_VISIT_TOT", "M08_VISIT_LBSTDAT")],
              names_glue = "{.value}_V{M08_VISIT_LBSTDAT}")

dataWide_list$M08 <- mnh08_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M19
mnh19_long <- dataExtract_list[["M19"]]

mnh19_wide <- mnh19_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M19_FORMCOMPLDAT_MNH19) & !is.na(M19_OBSSTDAT)) %>% 
  distinct()

dataWide_list$M19 <- mnh19_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M25
mnh25_long <- dataExtract_list[['M25']]
table(mnh25_long$M25_ANC_VISIT_N, useNA = "ifany")

## remove duplicates or missing key variables
mnh25_wide0 <- mnh25_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M25_FORMCOMPLDAT_MNH25) & !is.na(M25_OBSSTDAT)) %>% 
  distinct() %>% 
  subset(M25_ANC_VISIT_N != 5 & M25_ANC_VISIT_N != 11 ) %>% #remove not routine visit
  group_by(MOMID, PREGID) %>% 
  mutate(
    M25_VISIT_TOT = length(M25_ANC_VISIT_N),
    M25_VISIT_TYPE = case_when(
      M25_ANC_VISIT_N == 1 ~ 2,
      M25_ANC_VISIT_N == 2 ~ 3,
      M25_ANC_VISIT_N == 3 ~ 4,
      M25_ANC_VISIT_N == 4 ~ 5,
      M25_ANC_VISIT_N == 6 ~ 8,
      M25_ANC_VISIT_N == 7 ~ 9,
      M25_ANC_VISIT_N == 8 ~ 10,
      M25_ANC_VISIT_N == 9 ~ 11,
      M25_ANC_VISIT_N == 10 ~ 12
      )) %>% #this will be removed once the corrected the categories
  ungroup()

# long to wide
mnh25_wide1 <- mnh25_wide0 %>% 
  pivot_wider(names_from = M25_ANC_VISIT_N,
              values_from = names(mnh25_wide0)[!names(mnh25_wide0) %in% c("MOMID", "PREGID", "SITE", "M25_VISIT_TOT")],
              names_glue = "{.value}_V{M25_ANC_VISIT_N}")

dataWide_list$M25 <- mnh25_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M26
mnh26_long <- dataExtract_list[['M26']]

## remove duplicates or missing key variables
mnh26_wide0 <- mnh26_long %>% 
  left_join(
    mnh25 %>% select(MOMID, PREGID, SITE, M25_OBSSTDAT, M25_ANC_VISIT_N), 
    by = c("MOMID", "PREGID", "SITE", "M26_FTGE_OBSTDAT" = "M25_OBSSTDAT")
  ) %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M26_FORMCOMPLDAT_MNH26) & !is.na(M26_FTGE_OBSTDAT)) %>% 
  subset(M25_ANC_VISIT_N != 5 & M25_ANC_VISIT_N != 11 ) %>% #remove not routine visit
  distinct() %>% 
  group_by(MOMID, PREGID) %>% 
  mutate(
    M26_VISIT_TOT = length(M25_ANC_VISIT_N),
    M26_VISIT_TYPE = case_when(
      M25_ANC_VISIT_N == 1 ~ 2,
      M25_ANC_VISIT_N == 2 ~ 3,
      M25_ANC_VISIT_N == 3 ~ 4,
      M25_ANC_VISIT_N == 4 ~ 5,
      M25_ANC_VISIT_N == 6 ~ 8,
      M25_ANC_VISIT_N == 7 ~ 9,
      M25_ANC_VISIT_N == 8 ~ 10,
      M25_ANC_VISIT_N == 9 ~ 11,
      M25_ANC_VISIT_N == 10 ~ 12
    )) %>% 
  ungroup()
table(mnh26_wide0$M25_ANC_VISIT_N, useNA = "ifany")

# long to wide
mnh26_wide1 <- mnh26_wide0 %>% 
  pivot_wider(names_from = M25_ANC_VISIT_N,
              values_from = names(mnh26_wide0)[!names(mnh26_wide0) %in% c("MOMID", "PREGID", "SITE", "M26_VISIT_TOT")],
              names_glue = "{.value}_V{M25_ANC_VISIT_N}")

dataWide_list$M26 <- mnh26_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")


#**************************************************************************************
#* Combine data
#**************************************************************************************

load("derived_data/dataWide_list.rda")

#enroll MHH forms
enrollForms <- c('M00','M01','M02')
# combine enrollment data
enrollData <- dataWide_list[["M00"]]
for (Form in enrollForms[-c(1)]){
  
  data <- dataWide_list[[Form]]
  
  enrollData <- enrollData %>% 
    full_join(data, by = c("SCRNID", "MOMID", "PREGID", "SITE")) %>% 
    group_by(SCRNID) %>% 
    fill(names(.), .direction = "downup" ) %>% 
    ungroup() %>% 
    distinct()
}

# maternal MNH forms
matForms <- c('M03','M04','M05','M06','M07','M08','M25','M26', 'M19')

# combine maternal data
matData <- dataWide_list[["M03"]]
for (Form in matForms[-c(1)]){
  
  data <- dataWide_list[[Form]]
  
  matData <- matData %>% 
    full_join(data, by = c("MOMID", "PREGID", "SITE")) %>% 
    group_by(MOMID, PREGID) %>% 
    ungroup() %>% 
    distinct()
} 

matData <- matData %>%
  full_join(enrollData, by = c("MOMID", "PREGID", "SITE")) %>% 
  select(SCRNID, MOMID, PREGID, SITE, starts_with("M00_"), starts_with("M01_"), 
         starts_with("M02_"), everything()) %>% 
distinct()

#Adding a step to fill in estimated age by using MOMID since we have different SCRNID between 00 and 01, 02. 
#this step should be removed once SCRNID can matched
matData <- matData %>% 
  group_by(MOMID) %>% 
  fill(M00_ESTIMATED_AGE, .direction = "updown") %>% 
  ungroup()

save(matData, file = "derived_data/matData.rda") # will update once we have neo info
