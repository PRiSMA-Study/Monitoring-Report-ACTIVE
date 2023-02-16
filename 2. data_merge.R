#*****************************************************************************
#*Revise column type for site, merge and save data (mnh##.csv)
#*Extract variables for monitoring report and change the smaller data from long to wide
#*Merge all data into two datasets: one for mom, one for infant
#*Save data (matData.csv, neoData.csv)
#*****************************************************************************

library(tidyverse)
library(lubridate)

#*****************************************************************************
#*Revise column type for site, merge and save data (mnh##.csv)
#*****************************************************************************

#format date and time
dateformat <- stamp_date("16-06-2016") 

#******M00
#prepare site data
PA_mnh00_m <- PA_mnh00 %>% 
  mutate(
    M00_SCRN_OBSSTDAT = as.character(dateformat(M00_SCRN_OBSSTDAT)),
    M00_PRV_MOMID = as.character(M00_PRV_MOMID),
    M00_FORMCOMPLDAT_MNH00 = as.character(dateformat(M00_FORMCOMPLDAT_MNH00)), 
    M00_OTHR_REASON_IEORRES = as.numeric(M00_OTHR_REASON_IEORRES), 
    )

KE_mnh00_m <- KE_mnh00 %>% 
  mutate(
    SCRNID = as.character(SCRNID),
    M00_SCRN_OBSSTDAT = as.character(dateformat(mdy_hm(M00_SCRN_OBSSTDAT))),
    M00_BRTHDAT = as.character(dateformat(mdy_hm(M00_BRTHDAT))),
) 

ZA_mnh00_m <- ZA_mnh00 %>% 
  mutate(
    M00_ESTIMATED_AGE = as.numeric(M00_ESTIMATED_AGE),
    M00_SCHOOL_YRS_SCORRES = as.numeric(M00_SCHOOL_YRS_SCORRES),
    M00_AGE_IEORRES = as.numeric(M00_AGE_IEORRES),
    M00_OTHR_IEORRES = as.numeric(M00_OTHR_IEORRES),
    M00_OTHR_REASON_IEORRES = as.numeric(M00_OTHR_REASON_IEORRES), 
    M00_CON_LAR_YN_DSDECOD = as.numeric(M00_CON_LAR_YN_DSDECOD),
    M00_ASSNT_YN_DSDECOD = as.numeric(M00_ASSNT_YN_DSDECOD),
    M00_CON_WITNESS_SIGNATURE_PRYN = as.numeric(M00_CON_WITNESS_SIGNATURE_PRYN),
    M00_CON_FUTURE_RES_DSDECOD = as.numeric(M00_CON_FUTURE_RES_DSDECOD),
    M00_CON_SAMPLES_DSDECOD = as.numeric(M00_CON_SAMPLES_DSDECOD),
    M00_FUT_CONTACT_DSDECOD = as.numeric(M00_FUT_CONTACT_DSDECOD),
    M00_PRV_ENROLL_YN = as.numeric(M00_PRV_ENROLL_YN),
    M00_COYN_MNH00 = as.numeric(M00_COYN_MNH00),
  ) 

GH_mnh00_m <- GH_mnh00 %>% 
  mutate(
    M00_OTHR_REASON_IEORRES = as.numeric(M00_OTHR_REASON_IEORRES)
  ) 

#merge site data
mnh00 <- PA_mnh00_m %>% 
  bind_rows(KE_mnh00_m) %>% 
  bind_rows(ZA_mnh00_m) %>% 
  bind_rows(GH_mnh00_m)

#******M01
PA_mnh01_m <- PA_mnh01

KE_mnh01_m <- KE_mnh01 %>% 
  mutate(
    SCRNID = as.character(SCRNID),
    M01_US_OHOSTDAT = as.character(dateformat(mdy_hm(M01_US_OHOSTDAT)))
  ) 

ZA_mnh01_m <- ZA_mnh01 %>% 
  mutate(
    M01_KNOWN_LMP_SCORRES = as.numeric(M01_KNOWN_LMP_SCORRES),
    M01_GA_LMP_WEEKS_SCORRES = as.numeric(M01_GA_LMP_WEEKS_SCORRES),
    M01_GA_LMP_DAYS_SCORRES = as.numeric(M01_GA_LMP_DAYS_SCORRES),
    M01_US_PELOC = as.numeric(M01_US_PELOC),
    M01_FHR_VSORRES_SCREEN = as.numeric(M01_FHR_VSORRES_SCREEN),
    M01_PREG_LOSS_PERES = as.numeric(M01_PREG_LOSS_PERES),
    M01_PREG_LOSS_PEDESC = as.numeric(M01_PREG_LOSS_PEDESC),
    M01_FETUS_CT_PERES_US = as.numeric(M01_FETUS_CT_PERES_US),
    M01_FHR_VSORRES_FTS1 = as.numeric(M01_FHR_VSORRES_FTS1),
    M01_FHR_VSORRES_FTS2 = as.numeric(M01_FHR_VSORRES_FTS2),
    M01_FHR_VSORRES_FTS3 = as.numeric(M01_FHR_VSORRES_FTS3),
    M01_FHR_VSORRES_FTS4 = as.numeric(M01_FHR_VSORRES_FTS4),
    M01_AFI_PERES_FTS1_Q1 = as.numeric(M01_AFI_PERES_FTS1_Q1),
    M01_AFI_PERES_FTS1_Q2 = as.numeric(M01_AFI_PERES_FTS1_Q2),
    M01_AFI_PERES_FTS1_Q3 = as.numeric(M01_AFI_PERES_FTS1_Q3),
    M01_AFI_PERES_FTS1_Q4 = as.numeric(M01_AFI_PERES_FTS1_Q4),
    M01_AFI_PERES_FTS2_Q1 = as.numeric(M01_AFI_PERES_FTS2_Q1),
    M01_AFI_PERES_FTS2_Q2 = as.numeric(M01_AFI_PERES_FTS2_Q2),
    M01_AFI_PERES_FTS2_Q3 = as.numeric(M01_AFI_PERES_FTS2_Q3),
    M01_AFI_PERES_FTS2_Q4 = as.numeric(M01_AFI_PERES_FTS2_Q4),
    M01_AFI_PERES_FTS3_Q1 = as.numeric(M01_AFI_PERES_FTS3_Q1),
    M01_AFI_PERES_FTS3_Q2 = as.numeric(M01_AFI_PERES_FTS3_Q2),
    M01_AFI_PERES_FTS3_Q3 = as.numeric(M01_AFI_PERES_FTS3_Q3),
    M01_AFI_PERES_FTS3_Q4 = as.numeric(M01_AFI_PERES_FTS3_Q4),
    M01_AFI_PERES_FTS4_Q1 = as.numeric(M01_AFI_PERES_FTS4_Q1),
    M01_AFI_PERES_FTS4_Q2 = as.numeric(M01_AFI_PERES_FTS4_Q2),
    M01_AFI_PERES_FTS4_Q3 = as.numeric(M01_AFI_PERES_FTS4_Q3),
    M01_AFI_PERES_FTS4_Q4 = as.numeric(M01_AFI_PERES_FTS4_Q4),
    M01_PREVIA_PERES_FTS1 = as.numeric(M01_PREVIA_PERES_FTS1),
    M01_PREVIA_PERES_FTS2 = as.numeric(M01_PREVIA_PERES_FTS2),
    M01_PREVIA_PERES_FTS3 = as.numeric(M01_PREVIA_PERES_FTS3),
    M01_PREVIA_PERES_FTS4 = as.numeric(M01_PREVIA_PERES_FTS4),
    M01_CRL_PERES_01_FTS1 = as.numeric(M01_CRL_PERES_01_FTS1),
    M01_CRL_PERES_01_FTS2 = as.numeric(M01_CRL_PERES_01_FTS2),
    M01_CRL_PERES_01_FTS3 = as.numeric(M01_CRL_PERES_01_FTS3),
    M01_CRL_PERES_01_FTS4 = as.numeric(M01_CRL_PERES_01_FTS4),
    M01_CRL_PERES_02_FTS1 = as.numeric(M01_CRL_PERES_02_FTS1),
    M01_CRL_PERES_02_FTS2 = as.numeric(M01_CRL_PERES_02_FTS2),
    M01_CRL_PERES_02_FTS3 = as.numeric(M01_CRL_PERES_02_FTS3),
    M01_CRL_PERES_02_FTS4 = as.numeric(M01_CRL_PERES_02_FTS4),
    M01_CRL_PERES_MEAN_FTS1 = as.numeric(M01_CRL_PERES_MEAN_FTS1),
    M01_CRL_PERES_MEAN_FTS2 = as.numeric(M01_CRL_PERES_MEAN_FTS2),
    M01_CRL_PERES_MEAN_FTS3 = as.numeric(M01_CRL_PERES_MEAN_FTS3),
    M01_CRL_PERES_MEAN_FTS4 = as.numeric(M01_CRL_PERES_MEAN_FTS4),
    M01_BPD_PERES_01_FTS1 = as.numeric(M01_BPD_PERES_01_FTS1),
    M01_BPD_PERES_02_FTS1 = as.numeric(M01_BPD_PERES_02_FTS1),
    M01_BPD_PERES_MEAN_FTS1 = as.numeric(M01_BPD_PERES_MEAN_FTS1),
    M01_BPD_PERED_01_FTS2 = as.numeric(M01_BPD_PERED_01_FTS2),
    M01_BPD_PERED_02_FTS2 = as.numeric(M01_BPD_PERED_02_FTS2),
    M01_BPD_PERES_MEAN_FTS2 = as.numeric(M01_BPD_PERES_MEAN_FTS2),
    M01_BPD_PERED_01_FTS3 = as.numeric(M01_BPD_PERED_01_FTS3),
    M01_BPD_PERED_02_FTS3 = as.numeric(M01_BPD_PERED_02_FTS3),
    M01_BPD_PERES_MEAN_FTS3 = as.numeric(M01_BPD_PERES_MEAN_FTS3),
    M01_BPD_PERED_01_FTS4 = as.numeric(M01_BPD_PERED_01_FTS4),
    M01_BPD_PERED_02_FTS4 = as.numeric(M01_BPD_PERED_02_FTS4),
    M01_BPD_PERES_MEAN_FTS4 = as.numeric(M01_BPD_PERES_MEAN_FTS4),
    M01_HC_PERES_01_FTS1 = as.numeric(M01_HC_PERES_01_FTS1),
    M01_HC_PERES_02_FTS1 = as.numeric(M01_HC_PERES_02_FTS1),
    M01_HC_PERES_MEAN_FTS1 = as.numeric(M01_HC_PERES_MEAN_FTS1),
    M01_HC_PERED_01_FTS2 = as.numeric(M01_HC_PERED_01_FTS2),
    M01_HC_PERED_02_FTS2 = as.numeric(M01_HC_PERED_02_FTS2),
    M01_HC_PERES_MEAN_FTS2 = as.numeric(M01_HC_PERES_MEAN_FTS2),
    M01_HC_PERED_01_FTS3 = as.numeric(M01_HC_PERED_01_FTS3),
    M01_HC_PERED_02_FTS3 = as.numeric(M01_HC_PERED_02_FTS3),
    M01_HC_PERES_MEAN_FTS3 = as.numeric(M01_HC_PERES_MEAN_FTS3),
    M01_HC_PERED_01_FTS4 = as.numeric(M01_HC_PERED_01_FTS4),
    M01_HC_PERED_02_FTS4 = as.numeric(M01_HC_PERED_02_FTS4),
    M01_HC_PERES_MEAN_FTS4 = as.numeric(M01_HC_PERES_MEAN_FTS4),
    M01_AC_PERES_01_FTS1 = as.numeric(M01_AC_PERES_01_FTS1),
    M01_AC_PERES_02_FTS1 = as.numeric(M01_AC_PERES_02_FTS1),
    M01_AC_PERES_MEAN_FTS1 = as.numeric(M01_AC_PERES_MEAN_FTS1),
    M01_AC_PERED_01_FTS2 = as.numeric(M01_AC_PERED_01_FTS2),
    M01_AC_PERED_02_FTS2 = as.numeric(M01_AC_PERED_02_FTS2),
    M01_AC_PERES_MEAN_FTS2 = as.numeric(M01_AC_PERES_MEAN_FTS2),
    M01_AC_PERED_01_FTS3 = as.numeric(M01_AC_PERED_01_FTS3),
    M01_AC_PERED_02_FTS3 = as.numeric(M01_AC_PERED_02_FTS3),
    M01_AC_PERES_MEAN_FTS3 = as.numeric(M01_AC_PERES_MEAN_FTS3),
    M01_AC_PERED_01_FTS4 = as.numeric(M01_AC_PERED_01_FTS4),
    M01_AC_PERED_02_FTS4 = as.numeric(M01_AC_PERED_02_FTS4),
    M01_AC_PERES_MEAN_FTS4 = as.numeric(M01_AC_PERES_MEAN_FTS4),
    M01_FL_PERES_01_FTS1 = as.numeric(M01_FL_PERES_01_FTS1),
    M01_FL_PERES_02_FTS1 = as.numeric(M01_FL_PERES_02_FTS1),
    M01_FL_PERES_MEAN_FTS1 = as.numeric(M01_FL_PERES_MEAN_FTS1),
    M01_FL_PERED_01_FTS2 = as.numeric(M01_FL_PERED_01_FTS2),
    M01_FL_PERED_02_FTS2 = as.numeric(M01_FL_PERED_02_FTS2),
    M01_FL_PERES_MEAN_FTS2 = as.numeric(M01_FL_PERES_MEAN_FTS2),
    M01_FL_PERED_01_FTS3 = as.numeric(M01_FL_PERED_01_FTS3),
    M01_FL_PERED_02_FTS3 = as.numeric(M01_FL_PERED_02_FTS3),
    M01_FL_PERES_MEAN_FTS3 = as.numeric(M01_FL_PERES_MEAN_FTS3),
    M01_FL_PERED_01_FTS4 = as.numeric(M01_FL_PERED_01_FTS4),
    M01_FL_PERED_02_FTS4 = as.numeric(M01_FL_PERED_02_FTS4),
    M01_FL_PERES_MEAN_FTS4 = as.numeric(M01_FL_PERES_MEAN_FTS4),
    M01_TCD_PERES_01_FTS1 = as.numeric(M01_TCD_PERES_01_FTS1),
    M01_TCD_PERES_02_FTS1 = as.numeric(M01_TCD_PERES_02_FTS1),
    M01_TCD_PERES_MEAN_FTS1 = as.numeric(M01_TCD_PERES_MEAN_FTS1),
    M01_TCD_PERES_01_FTS2 = as.numeric(M01_TCD_PERES_01_FTS2),
    M01_TCD_PERES_02_FTS2 = as.numeric(M01_TCD_PERES_02_FTS2),
    M01_TCD_PERES_MEAN_FTS2 = as.numeric(M01_TCD_PERES_MEAN_FTS2),
    M01_TCD_PERES_01_FTS3 = as.numeric(M01_TCD_PERES_01_FTS3),
    M01_TCD_PERES_02_FTS3 = as.numeric(M01_TCD_PERES_02_FTS3),
    M01_TCD_PERES_MEAN_FTS3 = as.numeric(M01_TCD_PERES_MEAN_FTS3),
    M01_TCD_PERES_01_FTS4 = as.numeric(M01_TCD_PERES_01_FTS4),
    M01_TCD_PERES_02_FTS4 = as.numeric(M01_TCD_PERES_02_FTS4),
    M01_TCD_PERES_MEAN_FTS4 = as.numeric(M01_TCD_PERES_MEAN_FTS4),
    M01_EFW_PERES_01_FTS1 = as.numeric(M01_EFW_PERES_01_FTS1),
    M01_EFW_PERES_01_FTS2 = as.numeric(M01_EFW_PERES_01_FTS2),
    M01_EFW_PERES_01_FTS3 = as.numeric(M01_EFW_PERES_01_FTS3),
    M01_EFW_PERES_01_FTS4 = as.numeric(M01_EFW_PERES_01_FTS4),
    M01_US_GA_WKS_AGE_FTS1 = as.numeric(M01_US_GA_WKS_AGE_FTS1),
    M01_US_GA_DAYS_AGE_FTS1 = as.numeric(M01_US_GA_DAYS_AGE_FTS1),
    M01_US_GA_WKS_AGE_FTS2 = as.numeric(M01_US_GA_WKS_AGE_FTS2),
    M01_US_GA_DAYS_AGE_FTS2 = as.numeric(M01_US_GA_DAYS_AGE_FTS2),
    M01_US_GA_WKS_AGE_FTS3 = as.numeric(M01_US_GA_WKS_AGE_FTS3),
    M01_US_GA_DAYS_AGE_FTS3 = as.numeric(M01_US_GA_DAYS_AGE_FTS3),
    M01_US_GA_WKS_AGE_FTS4 = as.numeric(M01_US_GA_WKS_AGE_FTS4),
    M01_US_GA_DAYS_AGE_FTS4 = as.numeric(M01_US_GA_DAYS_AGE_FTS4),
  ) 

GH_mnh01_m <- GH_mnh01 %>% 
  mutate(
    M01_MAT_VISIT_OTHR_MNH01 = as.character(M01_MAT_VISIT_OTHR_MNH01),
  ) 

#merge site data
mnh01 <- PA_mnh01_m %>% 
  bind_rows(KE_mnh01_m) %>% 
  bind_rows(ZA_mnh01_m) %>% 
  bind_rows(GH_mnh01_m)

table(mnh01$M01_US_OHOSTDAT, mnh01$SITE, useNA = "ifany")
table(mnh01$M01_US_GA_WKS_AGE_FTS1, mnh01$SITE, useNA = "ifany")
table(mnh01$M01_US_GA_DAYS_AGE_FTS1, mnh01$SITE, useNA = "ifany")

#******M02
PA_mnh02_m <- PA_mnh02

KE_mnh02_m <- KE_mnh02 %>% 
  mutate(
    SCRNID = as.character(SCRNID),
    M02_SCRN_OBSSTDAT = as.character(dateformat(mdy_hm(M02_SCRN_OBSSTDAT))),
    M02_FORMCOMPLDAT_MNH02 = as.character(dateformat(mdy_hm(M02_FORMCOMPLDAT_MNH02))),
  ) 

ZA_mnh02_m <- ZA_mnh02 %>% 
  mutate(
    M02_SCRN_FAC_SPFY_OBSLOC = as.character(M02_SCRN_FAC_SPFY_OBSLOC),
    M02_CONSENT_IEORRES = as.numeric(M02_CONSENT_IEORRES),
    M02_ANC_PREENROLL_YN = as.numeric(M02_ANC_PREENROLL_YN),
    M02_ANC_PREENROLL_3 = as.numeric(M02_ANC_PREENROLL_3),
    M02_ANC_PREENROLL_4 = as.numeric(M02_ANC_PREENROLL_4),
    M02_COYN_MNH02 = as.numeric(M02_COYN_MNH02),
  ) 

GH_mnh02_m <- GH_mnh02

#merge site data
mnh02 <- PA_mnh02_m %>% 
  bind_rows(KE_mnh02_m) %>% 
  bind_rows(ZA_mnh02_m) %>% 
  bind_rows(GH_mnh02_m)

#******M03
PA_mnh03_m <- PA_mnh03 %>% 
  mutate(
    M03_OWN_RENT_SPFY_SCORRES = as.character(M03_OWN_RENT_SPFY_SCORRES),
    M03_LAND_USE_SPFY_FCORRES_88 = as.character(M03_LAND_USE_SPFY_FCORRES_88), 
    M03_HEAD_HH_FCORRES = as.numeric(M03_HEAD_HH_FCORRES), 
    M03_CHEW_OECOCCUR = as.numeric(M03_CHEW_OECOCCUR), 
    M03_HOUSE_ROOMS_FCORRES = as.numeric(M03_HOUSE_ROOMS_FCORRES), 
  ) 

KE_mnh03_m <- KE_mnh03 %>% 
  mutate(
    M03_SD_OBSSTDAT = as.character(dateformat(mdy_hm(M03_SD_OBSSTDAT))),
  ) 

#update code later by reading numeric in dd and revise all
ZA_mnh03_m <- ZA_mnh03 %>% 
  mutate(
    M03_MARITAL_SCORRES = as.numeric(M03_MARITAL_SCORRES),
    M03_MARITAL_AGE = as.numeric(M03_MARITAL_AGE),
    M03_RELIGION_SCORRES = as.numeric(M03_RELIGION_SCORRES),
    M03_CETHNIC = as.numeric(M03_CETHNIC),
    M03_HEAD_HH_FCORRES = as.numeric(M03_HEAD_HH_FCORRES),
    M03_HOUSE_OCC_TOT_FCORRES = as.numeric(M03_HOUSE_OCC_TOT_FCORRES),
    M03_HOUSE_OCC_LT5_FCORRES = as.numeric(M03_HOUSE_OCC_LT5_FCORRES),
    M03_HOUSE_OCC_GE5_FCORRES = as.numeric(M03_HOUSE_OCC_GE5_FCORRES),
    M03_H2O_FCORRES = as.numeric(M03_H2O_FCORRES),
    M03_H2O_DIST_FCORRES = as.numeric(M03_H2O_DIST_FCORRES),
    M03_H2O_HOURS_FCORRES = as.numeric(M03_H2O_HOURS_FCORRES),
    M03_H2O_MINS_FCORRES = as.numeric(M03_H2O_MINS_FCORRES),
    M03_H2O_PREP_FCORRES = as.numeric(M03_H2O_PREP_FCORRES),
    M03_TOILET_LOC_FCORRES = as.numeric(M03_TOILET_LOC_FCORRES),
    M03_TOILET_SHARE_FCORRES = as.numeric(M03_TOILET_SHARE_FCORRES),
    M03_TOILET_SHARE_NUM_FCORRES = as.numeric(M03_TOILET_SHARE_NUM_FCORRES),
    M03_EXT_WALL_FCORRES = as.numeric(M03_EXT_WALL_FCORRES),
    M03_FLOOR_FCORRES = as.numeric(M03_FLOOR_FCORRES),
    M03_ELECTRICITY_FCORRES = as.numeric(M03_ELECTRICITY_FCORRES),
    M03_SOLAR_FCORRES = as.numeric(M03_SOLAR_FCORRES),
    M03_INTERNET_FCORRES = as.numeric(M03_INTERNET_FCORRES),
    M03_LANDLINE_FCORRES = as.numeric(M03_LANDLINE_FCORRES),
    M03_MOBILE_FCORRES = as.numeric(M03_MOBILE_FCORRES),
    M03_MOBILE_NUM_FCORRES = as.numeric(M03_MOBILE_NUM_FCORRES),
    M03_MOBILE_ACCESS_FCORRES = as.numeric(M03_MOBILE_ACCESS_FCORRES),
    M03_RADIO_FCORRES = as.numeric(M03_RADIO_FCORRES),
    M03_RADIO_NUM_FCORRES = as.numeric(M03_RADIO_NUM_FCORRES),
    M03_TV_FCORRES = as.numeric(M03_TV_FCORRES),
    M03_TV_NUM_FCORRES = as.numeric(M03_TV_NUM_FCORRES),
    M03_FRIDGE_FCORRES = as.numeric(M03_FRIDGE_FCORRES),
    M03_FRIDGE_NUM_FCORRES = as.numeric(M03_FRIDGE_NUM_FCORRES),
    M03_COMPUTER_FCORRES = as.numeric(M03_COMPUTER_FCORRES),
    M03_COMPUTER_NUM_FCORRES = as.numeric(M03_COMPUTER_NUM_FCORRES),
    M03_WATCH_FCORRES = as.numeric(M03_WATCH_FCORRES),
    M03_WATCH_NUM_FCORRES = as.numeric(M03_WATCH_NUM_FCORRES),
    M03_BIKE_FCORRES = as.numeric(M03_BIKE_FCORRES),
    M03_BIKE_NUM_FCORRES = as.numeric(M03_BIKE_NUM_FCORRES),
    M03_MOTORCYCLE_FCORRES = as.numeric(M03_MOTORCYCLE_FCORRES),
    M03_MOTORCYCLE_NUM_FCORRES = as.numeric(M03_MOTORCYCLE_NUM_FCORRES),
    M03_CAR_FCORRES = as.numeric(M03_CAR_FCORRES),
    M03_CAR_NUM_FCORRES = as.numeric(M03_CAR_NUM_FCORRES),
    M03_BOAT_FCORRES = as.numeric(M03_BOAT_FCORRES),
    M03_BOAT_NUM_FCORRES = as.numeric(M03_BOAT_NUM_FCORRES),
    M03_CART_FCORRES = as.numeric(M03_CART_FCORRES),
    M03_CART_NUM_FCORRES = as.numeric(M03_CART_NUM_FCORRES),
    M03_PLOUGH_FCORRES = as.numeric(M03_PLOUGH_FCORRES),
    M03_PLOUGH_NUM_FCORRES = as.numeric(M03_PLOUGH_NUM_FCORRES),
    M03_FOAM_MATT_FCORRES = as.numeric(M03_FOAM_MATT_FCORRES),
    M03_FOAM_MATT_NUM_FCORRES = as.numeric(M03_FOAM_MATT_NUM_FCORRES),
    M03_STRAW_MATT_FCORRES = as.numeric(M03_STRAW_MATT_FCORRES),
    M03_STRAW_MATT_NUM_FCORRES = as.numeric(M03_STRAW_MATT_NUM_FCORRES),
    M03_SPRING_MATT_FCORRES = as.numeric(M03_SPRING_MATT_FCORRES),
    M03_SPRING_MATT_NUM_FCORRES = as.numeric(M03_SPRING_MATT_NUM_FCORRES),
    M03_SOFA_FCORRES = as.numeric(M03_SOFA_FCORRES),
    M03_SOFA_NUM_FCORRES = as.numeric(M03_SOFA_NUM_FCORRES),
    M03_LANTERN_FCORRES = as.numeric(M03_LANTERN_FCORRES),
    M03_LANTERN_NUM_FCORRES = as.numeric(M03_LANTERN_NUM_FCORRES),
    M03_SEW_FCORRES = as.numeric(M03_SEW_FCORRES),
    M03_SEW_NUM_FCORRES = as.numeric(M03_SEW_NUM_FCORRES),
    M03_WASH_FCORRES = as.numeric(M03_WASH_FCORRES),
    M03_WASH_NUM_FCORRES = as.numeric(M03_WASH_NUM_FCORRES),
    M03_BLENDER_FCORRES = as.numeric(M03_BLENDER_FCORRES),
    M03_BLENDER_NUM_FCORRES = as.numeric(M03_BLENDER_NUM_FCORRES),
    M03_MOSQUITO_NET_FCORRES = as.numeric(M03_MOSQUITO_NET_FCORRES),
    M03_MOSQUITO_NET_NUM_FCORRES = as.numeric(M03_MOSQUITO_NET_NUM_FCORRES),
    M03_TRICYCLES_FCORRES = as.numeric(M03_TRICYCLES_FCORRES),
    M03_TRICYCLES_NUM_FCORRES = as.numeric(M03_TRICYCLES_NUM_FCORRES),
    M03_TABLES_FCORRES = as.numeric(M03_TABLES_FCORRES),
    M03_TABLES_NUM_FCORRES = as.numeric(M03_TABLES_NUM_FCORRES),
    M03_CABINETS_FCORRES = as.numeric(M03_CABINETS_FCORRES),
    M03_CABINETS_NUM_FCORRES = as.numeric(M03_CABINETS_NUM_FCORRES),
    M03_SAT_DISH_FCORRES = as.numeric(M03_SAT_DISH_FCORRES),
    M03_SAT_DISH_NUM_FCORRES = as.numeric(M03_SAT_DISH_NUM_FCORRES),
    M03_DVD_CD_FCORRES = as.numeric(M03_DVD_CD_FCORRES),
    M03_DVD_CD_NUM_FCORRES = as.numeric(M03_DVD_CD_NUM_FCORRES),
    M03_AIRCON_FCORRES = as.numeric(M03_AIRCON_FCORRES),
    M03_AIRCON_NUM_FCORRES = as.numeric(M03_AIRCON_NUM_FCORRES),
    M03_TRACTOR_FCORRES = as.numeric(M03_TRACTOR_FCORRES),
    M03_TRACTOR_NUM_FCORRES = as.numeric(M03_TRACTOR_NUM_FCORRES),
    M03_OWN_RENT_SCORRES = as.numeric(M03_OWN_RENT_SCORRES),
    M03_HOUSE_ROOMS_FCORRES = as.numeric(M03_HOUSE_ROOMS_FCORRES),
    M03_HOUSE_ROOM_CHILD_FCORRES = as.numeric(M03_HOUSE_ROOM_CHILD_FCORRES),
    M03_LAND_FCORRES = as.numeric(M03_LAND_FCORRES),
    M03_LIVESTOCK_FCORRES = as.numeric(M03_LIVESTOCK_FCORRES),
    M03_CATTLE_FCORRES = as.numeric(M03_CATTLE_FCORRES),
    M03_CATTLE_NUM_FCORRES = as.numeric(M03_CATTLE_NUM_FCORRES),
    M03_GOAT_FCORRES = as.numeric(M03_GOAT_FCORRES),
    M03_GOAT_NUM_FCORRES = as.numeric(M03_GOAT_NUM_FCORRES),
    M03_SHEEP_FCORRES = as.numeric(M03_SHEEP_FCORRES),
    M03_SHEEP_NUM_FCORRES = as.numeric(M03_SHEEP_NUM_FCORRES),
    M03_POULTRY_FCORRES = as.numeric(M03_POULTRY_FCORRES),
    M03_POULTRY_NUM_FCORRES = as.numeric(M03_POULTRY_NUM_FCORRES),
    M03_PIG_FCORRES = as.numeric(M03_PIG_FCORRES),
    M03_PIG_NUM_FCORRES = as.numeric(M03_PIG_NUM_FCORRES),
    M03_DONKEY_FCORRES = as.numeric(M03_DONKEY_FCORRES),
    M03_DONKEY_NUM_FCORRES = as.numeric(M03_DONKEY_NUM_FCORRES),
    M03_HORSE_FCORRES = as.numeric(M03_HORSE_FCORRES),
    M03_HORSE_NUM_FCORRES = as.numeric(M03_HORSE_NUM_FCORRES),
    M03_ANIMAL_OTHR_FCORRES = as.numeric(M03_ANIMAL_OTHR_FCORRES),
    M03_ANIMAL_OTHR_NUM_FCORRES = as.numeric(M03_ANIMAL_OTHR_NUM_FCORRES),
    M03_PTR_SCORRES = as.numeric(M03_PTR_SCORRES),
    M03_STOVE_FCORRES = as.numeric(M03_STOVE_FCORRES),
    M03_COOKING_INSIDE_FCORRES = as.numeric(M03_COOKING_INSIDE_FCORRES),
    M03_COOKING_ROOM_FCORRES = as.numeric(M03_COOKING_ROOM_FCORRES),
    M03_COOKING_LOC_FCORRES = as.numeric(M03_COOKING_LOC_FCORRES),
    M03_COOKING_VENT_FCORRES = as.numeric(M03_COOKING_VENT_FCORRES),
    M03_SMOKE_OECOCCUR = as.numeric(M03_SMOKE_OECOCCUR),
    M03_SMOKE_IN_OECDOSFRQ = as.numeric(M03_SMOKE_IN_OECDOSFRQ),
    M03_SMOKE_HHOLD_OECOCCUR = as.numeric(M03_SMOKE_HHOLD_OECOCCUR),
    M03_SMOKE_HHOLD_IN_OECDOSFRQ = as.numeric(M03_SMOKE_HHOLD_IN_OECDOSFRQ),
    M03_CHEW_OECOCCUR = as.numeric(M03_CHEW_OECOCCUR),
    M03_CHEW_OECOCCUR = as.numeric(M03_CHEW_OECOCCUR),
    M03_CHEW_BNUT_OECOCCUR = as.numeric(M03_CHEW_BNUT_OECOCCUR),
    M03_DRINK_OECOCCUR = as.numeric(M03_DRINK_OECOCCUR),
    M03_PD_DM_SCORRES = as.numeric(M03_PD_DM_SCORRES),
    M03_JOB_SCORRES = as.numeric(M03_JOB_SCORRES),
  ) %>% 
  mutate_at(vars(starts_with("M03_LAND_USE_FCORRES_")), funs(as.numeric)) %>%
  mutate_at(vars(starts_with("M03_H2O_PREP_SPFY_FCORRES_")), funs(as.numeric)) %>%
  mutate_at(vars(starts_with("M03_STOVE_FUEL_FCORRES_")), funs(as.numeric)) 

GH_mnh03_m <- GH_mnh03 %>% 
  mutate(
    M03_LAND_USE_SPFY_FCORRES_88 = as.character(M03_LAND_USE_SPFY_FCORRES_88),
  ) 

#merge site data
mnh03 <- PA_mnh03_m %>% 
  bind_rows(KE_mnh03_m) %>% 
  bind_rows(ZA_mnh03_m) %>% 
  bind_rows(GH_mnh03_m)

#******M04
PA_mnh04_m <- PA_mnh04 %>% 
  mutate(
    M04_STI_OTHR_CMTRT = as.character(M04_STI_OTHR_CMTRT),
    M04_TB_SPFY_CMTRT = as.character(M04_TB_SPFY_CMTRT), 
  ) 

KE_mnh04_m <- KE_mnh04 %>% 
  mutate(
    M04_ANC_OBSSTDAT = as.character(dateformat(mdy_hm(M04_ANC_OBSSTDAT)))
  ) 

#merge site data
mnh04 <- PA_mnh04_m %>% 
  bind_rows(KE_mnh04_m)

#******M05
PA_mnh05_m <- PA_mnh05 

KE_mnh05_m <- KE_mnh05 %>% 
  mutate(
    M05_ANT_PEDAT = as.character(dateformat(mdy_hm(M05_ANT_PEDAT)))
  ) 

ZA_mnh05_m <- ZA_mnh05 %>% 
  mutate(
    M05_HEIGHT_PERES = as.numeric(M05_HEIGHT_PERES),
    M05_WEIGHT_PERES = as.numeric(M05_WEIGHT_PERES),
    M05_MUAC_PERES = as.numeric(M05_MUAC_PERES),
  )

mnh05 <- PA_mnh05_m %>% 
  bind_rows(KE_mnh05_m) %>% 
  bind_rows(ZA_mnh05_m)

#******M06
PA_mnh06_m <- PA_mnh06 

KE_mnh06_m <- KE_mnh06 %>% 
  mutate(
    M06_DIAG_VSDAT = as.character(dateformat(mdy_hm(M06_DIAG_VSDAT)))
  ) 

mnh06 <- PA_mnh06_m %>% 
  bind_rows(KE_mnh06_m)

#******M07
PA_mnh07_m <- PA_mnh07

KE_mnh07_m <- KE_mnh07 %>% 
  mutate(
    M07_MAT_SPEC_COLLECT_DAT = as.character(dateformat(mdy_hm(M07_MAT_SPEC_COLLECT_DAT)))
  ) 

ZA_mnh07_m <- ZA_mnh07

mnh07 <- PA_mnh07_m %>% 
  bind_rows(KE_mnh07_m) %>% 
  bind_rows(ZA_mnh07_m)

#******M08
PA_mnh08_m <- PA_mnh08 %>% 
  mutate(
    M08_LBSTDAT = as.character(dateformat(M08_LBSTDAT)), 
    M08_BLD_GRP_LBTSTDAT = as.character(dateformat(M08_BLD_GRP_LBTSTDAT)),
    M08_FORMCOMPLDAT_MNH08 = as.character(dateformat(M08_FORMCOMPLDAT_MNH08))
  )

KE_mnh08_m <- KE_mnh08 %>% 
  mutate(
    M08_LBSTDAT = as.character(dateformat(mdy_hm(M08_LBSTDAT))),
    M08_BLD_GRP_LBTSTDAT = as.character(dateformat(mdy_hm(M08_BLD_GRP_LBTSTDAT))),
    M08_FORMCOMPLDAT_MNH08 = as.character(dateformat(mdy_hm(M08_FORMCOMPLDAT_MNH08)))
  ) 

ZA_mnh08_m <- ZA_mnh08 %>% 
  mutate(
    M08_VISIT_LBSTDAT = as.numeric(M08_VISIT_LBSTDAT),
    M08_HBA1C_PRCNT = as.numeric(M08_HBA1C_PRCNT),
  ) %>%
  mutate_at(vars(starts_with("M08_RBC_THALA_")), funs(as.numeric)) %>% 
  mutate_at(vars(starts_with("M08_RBC_LBPERF_")), funs(as.numeric)) %>% 
  mutate_at(vars(matches("M08_(.+)_LBORRES")), funs(as.numeric)) 

mnh08 <- PA_mnh08_m %>% 
  bind_rows(KE_mnh08_m) %>% 
  bind_rows(ZA_mnh08_m)

#******M09
KE_mnh09_m <- KE_mnh09 %>% 
  mutate(
    M09_MAT_LD_OHOSTDAT = as.character(dateformat(mdy_hm(M09_MAT_LD_OHOSTDAT))),
    M09_DELIV_DSSTDAT_INF1 = as.character(dateformat(mdy_hm(M09_DELIV_DSSTDAT_INF1))),
    M09_DELIV_DSSTDAT_INF2 = as.character(dateformat(mdy_hm(M09_DELIV_DSSTDAT_INF2))),
    M09_DELIV_DSSTDAT_INF3 = as.character(dateformat(mdy_hm(M09_DELIV_DSSTDAT_INF3))),
    M09_DELIV_DSSTDAT_INF4 = as.character(dateformat(mdy_hm(M09_DELIV_DSSTDAT_INF4)))
  ) 

mnh09 <- KE_mnh09_m 

#******M10
KE_mnh10_m <- KE_mnh10

mnh10 <- KE_mnh10_m 

#******M11
KE_mnh11_m <- KE_mnh11

mnh11 <- KE_mnh11_m 

#******M12
KE_mnh12_m <- KE_mnh12

mnh12 <- KE_mnh12_m 

#******M13

#******M14

#******M15

#******M16

#******M17
KE_mnh17_m <- KE_mnh17

mnh17 <- KE_mnh17_m 

#******M18

#******M19
KE_mnh19_m <- KE_mnh19 %>% 
mutate(
  M19_OBSSTDAT = as.character(dateformat(mdy_hm(M19_OBSSTDAT)))
) 

mnh19 <- KE_mnh19_m 

#******M20

#******M21
KE_mnh21_m <- KE_mnh21

mnh21 <- KE_mnh21_m 

#******M22

#******M23

#******M24

#******M25
PA_mnh25_m <- PA_mnh25

KE_mnh25_m <- KE_mnh25 %>% 
  mutate(
    M25_OBSSTDAT = as.character(dateformat(mdy_hm(M25_OBSSTDAT)))
  ) 

mnh25 <- PA_mnh25_m %>% 
  bind_rows(KE_mnh25_m)

#******M26
PA_mnh26_m <- PA_mnh26

mnh26 <- PA_mnh26_m 

#************save data (mnh##.csv) update later
lapply(c(paste0("mnh0",c(0:9)),
         paste0("mnh",c(10:12, 17, 19, 21, 25, 26))), #update forms
       function(Id){
         write_csv(eval(parse(text = Id)), file = paste0("cleaned_data/", Id, ".csv"), col_names = TRUE, "")
       })

#**************************************************************************************
#*Extract variables for monitoring report 
#**************************************************************************************

# update/add/delete variable names in the varNames_sheet.xlsx (check if the var is multiple or singe when add)
varNames_sheet <- readxl::read_excel("varNames_sheet.xlsx")

# function to extract variables from the MNH forms
dataExtract_func <- function(FormId, varNames_sheet){
  
  # forms are saved in path data/ as mnhxx.csv
  formPath <- paste0("cleaned_data/mnh", FormId, ".csv")
  # formData <- read_csv(formPath, guess_max = 50000, show_col_types = FALSE)
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
#* Non-routine visit are removed temporarily
#**************************************************************************************
# load data
load("derived_data/dataExtract_list.rda")
# create empty list to save the processed data
dataWide_list <- list()

#******M00
mnh00_long <- dataExtract_list[["M00"]]

#******temp solution for Zambia data no SCRNID, no visit date, remove later!!!!!!!
mnh00_long <- mnh00_long %>% 
  mutate(
    SCRNID = case_when(
      SITE != "Zambia" ~ SCRNID,
      SITE == "Zambia" ~ PREGID
    ),
    M00_SCRN_OBSSTDAT = case_when(
      SITE != "Zambia" ~ M00_SCRN_OBSSTDAT,
      SITE == "Zambia" ~ M00_FORMCOMPLDAT_MNH00,
    )
  )

mnh00_wide <- mnh00_long %>% 
  subset(!is.na(SCRNID) & 
         !is.na(M00_FORMCOMPLDAT_MNH00) & !is.na(M00_SCRN_OBSSTDAT)) %>% 
  distinct()

dataWide_list$M00 <- mnh00_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M01 
mnh01_long <- dataExtract_list[["M01"]]

#******temp solution for Zambia data no SCRNID, remove later!!!!!!
mnh01_long <- mnh01_long %>% 
  mutate(
    SCRNID = case_when(
      SITE != "Zambia" ~ SCRNID,
      SITE == "Zambia" ~ PREGID,
    ),
    M01_FORMCOMPLDAT_MNH01 = case_when(
      SITE != "Zambia" ~ M01_FORMCOMPLDAT_MNH01,
      SITE == "Zambia" ~ M01_US_OHOSTDAT,
    )
  )

#function: get min positive value
minpositive = function(x) {
  y <- x[x>0]
  if (length(y) >0) {
    return(min(y))}
  else {return(x)}
}

#prepare data: add VISIT_TOT and VISIT_TYPE
mnh01_wide0 <- mnh01_long %>% 
  subset(!is.na(SCRNID) & !is.na(M01_FORMCOMPLDAT_MNH01) &
            !is.na(M01_US_OHOSTDAT)) %>% #!!!!!!revise later , this is because Zambia data has no complete date and scrnid
  rowwise() %>% 
  mutate(GAUSGSCRNDAYS = case_when(
      if_all(matches("M01_US_GA_(.+)_AGE_FTS"), ~.x == -7) ~ NA_real_,
      if_all(matches("M01_US_GA_(.+)_AGE_FTS"), ~is.na(.)) ~ NA_real_, 
      if_all(matches("M01_US_GA_DAYS_AGE_FTS"), ~is.na(.)) ~  max(
        c(M01_US_GA_WKS_AGE_FTS1 * 7, M01_US_GA_WKS_AGE_FTS2 * 7,
          M01_US_GA_WKS_AGE_FTS3 * 7, M01_US_GA_WKS_AGE_FTS4 * 7), na.rm = TRUE),
      TRUE ~ max(
        c((M01_US_GA_WKS_AGE_FTS1 * 7 + M01_US_GA_DAYS_AGE_FTS1),
          (M01_US_GA_WKS_AGE_FTS2 * 7 + M01_US_GA_DAYS_AGE_FTS2),
          (M01_US_GA_WKS_AGE_FTS3 * 7 + M01_US_GA_DAYS_AGE_FTS3),
          (M01_US_GA_WKS_AGE_FTS4 * 7 + M01_US_GA_DAYS_AGE_FTS4)), na.rm = TRUE)
  )) %>% 
  ungroup() %>%
group_by(SCRNID, MOMID, PREGID, SITE) %>% 
  # arrange(M01_US_VISIT, .by_group=T) %>%
  arrange(dmy(M01_US_OHOSTDAT), .by_group=T) %>% 
  mutate(
    baselineGA = case_when(
      M01_US_VISIT == 1 ~ GAUSGSCRNDAYS,
      SITE == "Ghana" ~ first(GAUSGSCRNDAYS), # special code for ghana!!!!!!fix later
    ),
    baselinedate = case_when(
      M01_US_VISIT == 1 ~ dmy(M01_US_OHOSTDAT),
      SITE == "Ghana" ~ dmy(first(M01_US_OHOSTDAT)), # special code for ghana!!!!!!fix later
    )) %>% 
  fill(baselineGA, .direction = "down") %>% 
  fill(baselinedate, .direction = "down") %>% 
  mutate(M01_VISIT_TYPE = case_when(
    SITE == "Ghana" & dmy(M01_US_OHOSTDAT) == baselinedate ~ 1,
    SITE == "Ghana" & ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) >= 18 & 
      ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) < 23 ~ 2, 
    SITE == "Ghana" & ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) >= 26 & 
      ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) < 31 ~ 3,  
    SITE == "Ghana" & ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) >= 31 & 
      ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) < 34 ~ 4,   
    SITE == "Ghana" & ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) >= 34 & 
      ((dmy(M01_US_OHOSTDAT)-baselinedate+baselineGA)/7) < 39 ~ 5, 
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
  mutate(M01_VISIT_TOT = length(M01_VISIT_TYPE)) %>% 
  ungroup() %>% 
  subset(M01_VISIT_TYPE != 13) #remove not routine visit
 
mnh01_wide1 <- mnh01_wide0 %>% 
  subset(M01_VISIT_TYPE == 1) %>%
  select(-c(M01_US_OHOSTDAT, M01_US_VISIT, M01_VISIT_TYPE, M01_MAT_VITAL_MNH01, M01_MAT_VISIT_MNH01, 
            M01_MAT_VISIT_OTHR_MNH01, M01_FORMCOMPLDAT_MNH01)) 

#select all us visit vars except first visit
mnh01_wide2 <- mnh01_wide0 %>% 
  select(SCRNID, MOMID, PREGID, SITE, 
           M01_US_OHOSTDAT, M01_US_VISIT, M01_VISIT_TYPE, M01_MAT_VITAL_MNH01, 
           M01_MAT_VISIT_MNH01, M01_MAT_VISIT_OTHR_MNH01, M01_FORMCOMPLDAT_MNH01) %>% 
  #remove later case for same visit window (same to remove not routine visit)
  group_by(SCRNID, MOMID, PREGID, SITE) %>% 
  arrange(dmy(M01_US_OHOSTDAT),.by_group=T) %>% 
  slice(1) %>% 
  ungroup() %>% ###
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

#******temp solution for Zambia data no SCRNID, remove later
mnh02_long <- mnh02_long %>% 
  mutate(
    SCRNID = case_when(
      SITE != "Zambia" ~ SCRNID,
      SITE == "Zambia" ~ PREGID,
    ),
    M02_FORMCOMPLDAT_MNH02 = case_when(
      SITE != "Zambia" ~ M02_FORMCOMPLDAT_MNH02,
      SITE == "Zambia" ~ M02_SCRN_OBSSTDAT,
    )
  )

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
           !is.na(M03_FORMCOMPLDAT_MNH03) & !is.na(M03_SD_OBSSTDAT)) 

dataWide_list$M03 <- mnh03_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M04
mnh04_long <- dataExtract_list[["M04"]]
table(mnh04_long$M04_TYPE_VISIT, useNA = "ifany")

mnh04_wide0 <- mnh04_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M04_FORMCOMPLDAT_MNH04) & !is.na(M04_ANC_OBSSTDAT)) %>% 
  distinct() %>% 
  subset(M04_TYPE_VISIT != 13) %>% #remove not routine visit, should be 6 after data fixing
  group_by(MOMID, PREGID) %>% 
  mutate(
    M04_VISIT_TOT = length(M04_TYPE_VISIT),
    M04_VISIT_TYPE = M04_TYPE_VISIT) %>% #since non-rountie removed, the rest options matched
  ungroup() 


# convert long-format to wide-format
mnh04_wide1 <- mnh04_wide0 %>% 
  pivot_wider(names_from = M04_TYPE_VISIT,
              values_from = names(mnh04_wide0)[!names(mnh04_wide0) %in% c("MOMID", "PREGID", "SITE", "M04_VISIT_TOT", "M04_TYPE_VISIT")], #
              # values_fn = list,
              names_glue = "{.value}_V{M04_TYPE_VISIT}") 

dataWide_list$M04 <- mnh04_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M05
mnh05_long <- dataExtract_list[['M05']]
table(mnh05_long$M05_TYPE_VISIT, useNA = "ifany")

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
              values_from = names(mnh05_wide0)[!names(mnh05_wide0) %in% c("MOMID", "PREGID", "SITE", "M05_VISIT_TOT")], #
              # values_fn = list,
              names_glue = "{.value}_V{M05_TYPE_VISIT}")

dataWide_list$M05 <- mnh05_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M06
mnh06_long <- dataExtract_list[['M06']]
table(mnh06_long$M06_TYPE_VISIT, useNA = "ifany")

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
              # values_fn = list,
              names_glue = "{.value}_V{M06_TYPE_VISIT}")

dataWide_list$M06 <- mnh06_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M07
mnh07_long <- dataExtract_list[['M07']]
table(mnh07_long$M07_MAT_SPEC_COLLECT_VISIT, useNA = "ifany")

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
              # values_fn = list,
              names_glue = "{.value}_V{M07_MAT_SPEC_COLLECT_VISIT}")

dataWide_list$M07 <- mnh07_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M08
mnh08_long <- dataExtract_list[['M08']]
table(mnh08_long$M08_VISIT_LBSTDAT, useNA = "ifany")

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
              values_from = names(mnh08_wide0)[!names(mnh08_wide0) %in% c("MOMID", "PREGID", "SITE", "M08_VISIT_TOT")],
              # values_fn = list,
              names_glue = "{.value}_V{M08_VISIT_LBSTDAT}")

dataWide_list$M08 <- mnh08_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M09
mnh09_long <- dataExtract_list[['M09']]

## remove duplicates or missing key variables
mnh09_wide <- mnh09_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M09_FORMCOMPLDAT_MNH09) & !is.na(M09_MAT_LD_OHOSTDAT)) %>% 
  distinct() 

dataWide_list$M09 <- mnh09_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M10
mnh10_long <- dataExtract_list[['M10']]

## remove duplicates or missing key variables
mnh10_wide <- mnh10_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M10_FORMCOMPLDAT_MNH10)) %>% 
  distinct() 

dataWide_list$M10 <- mnh10_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M11
mnh11_long <- dataExtract_list[['M11']]

## remove duplicates or missing key variables
mnh11_wide <- mnh11_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) & !is.na(INFANTID) &
           !is.na(M11_FORMCOMPLDAT_MNH11) & !is.na(M11_VISIT_OBSSTDAT)) %>% 
  distinct() 

dataWide_list$M11 <- mnh11_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M12
mnh12_long <- dataExtract_list[['M12']]
table(mnh12_long$M12_PNC_N_VISIT, useNA = "ifany")

## remove duplicates or missing key variables
mnh12_wide0 <- mnh12_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M12_FORMCOMPLDAT_MNH12) & !is.na(M12_VISIT_OBSSTDAT)) %>% 
  distinct() %>% 
  subset(M12_PNC_N_VISIT != 6) %>% #remove not routine visit
  group_by(MOMID, PREGID) %>% 
  mutate(
    M12_VISIT_TOT = length(M12_PNC_N_VISIT),
    M12_VISIT_TYPE = M12_PNC_N_VISIT) %>% # 
  ungroup()

# long to wide
mnh12_wide1 <- mnh12_wide0 %>% 
  pivot_wider(names_from = M12_PNC_N_VISIT,
              values_from = names(mnh12_wide0)[!names(mnh12_wide0) %in% c("MOMID", "PREGID", "SITE", "M12_VISIT_TOT")],
              names_glue = "{.value}_V{M12_PNC_N_VISIT}")

dataWide_list$M12 <- mnh12_wide1
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M17
mnh17_long <- dataExtract_list[["M17"]]

mnh17_wide <- mnh17_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M17_FORMCOMPLDAT_MNH17) & !is.na(M17_VISDAT)) %>% 
  distinct()

dataWide_list$M17 <- mnh17_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M19
mnh19_long <- dataExtract_list[["M19"]]

mnh19_wide <- mnh19_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M19_FORMCOMPLDAT_MNH19) & !is.na(M19_OBSSTDAT)) %>% 
  distinct()

dataWide_list$M19 <- mnh19_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M21
mnh21_long <- dataExtract_list[["M21"]]

mnh21_wide <- mnh21_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) & !is.na(INFANTID) &
           !is.na(M21_FORMCOMPLDAT_MNH21) & !is.na(M21_AESTDAT)) %>% 
  distinct()

dataWide_list$M21 <- mnh21_wide
save(dataWide_list, file = "derived_data/dataWide_list.rda")

#******M25
mnh25_long <- dataExtract_list[['M25']]
table(mnh25_long$M25_ANC_VISIT_N, useNA = "ifany")

## remove duplicates or missing key variables
mnh25_wide0 <- mnh25_long %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) &
           !is.na(M25_FORMCOMPLDAT_MNH25) & !is.na(M25_OBSSTDAT)) %>% 
  distinct() %>% 
  subset(M25_ANC_VISIT_N != 5 & M25_ANC_VISIT_N != 11 & M25_ANC_VISIT_N != 12) %>% #remove not routine visit
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
      )) %>% #this will be removed once the corrected the categories!!!!!!!
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
mnh26_wide0 <- mnh26_long %>% select(-SCRNID) %>% 
  left_join(
    mnh25 %>% select(MOMID, PREGID, SITE, M25_OBSSTDAT, M25_ANC_VISIT_N), 
    by = c("MOMID", "PREGID", "SITE", "M26_FTGE_OBSTDAT" = "M25_OBSSTDAT")
  ) %>% 
  subset(!is.na(MOMID) & !is.na(PREGID) & 
           !is.na(M26_FORMCOMPLDAT_MNH26) & !is.na(M26_FTGE_OBSTDAT)) %>% 
  subset(M25_ANC_VISIT_N != 5 & M25_ANC_VISIT_N != 11 & M25_ANC_VISIT_N != 12) %>% #remove not routine visit
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
matForms <- c('M03','M04','M05','M06','M07','M08','M25','M26', 'M09', 'M10', 'M12', 'M17', 'M21')

# combine maternal data
matData <- dataWide_list[["M03"]]
for (Form in matForms[-c(1)]){
  
  data <- dataWide_list[[Form]]
  
  matData <- matData %>% 
    full_join(data, by = c("MOMID", "PREGID", "SITE")) %>% 
    group_by(MOMID, PREGID, SITE) %>% 
    fill(names(.), .direction = "downup" ) %>% 
    ungroup() %>% 
    distinct()
} 

matData <- matData %>%
  full_join(enrollData, by = c("MOMID", "PREGID", "SITE")) %>% 
  select(SCRNID, MOMID, PREGID, SITE, starts_with("M00_"), starts_with("M01_"), 
         starts_with("M02_"), everything()) %>% 
distinct()

#fill in estimated age by using MOMID since we have different SCRNID between 00 and 01, 02. 
#this step should be removed once SCRNID can matched !!!!!!
matData <- matData %>% 
  group_by(MOMID) %>% 
  fill(M00_ESTIMATED_AGE, .direction = "updown") %>% 
  ungroup() %>% 
  #remove Zambia case before relaunch
  filter(!(SITE == "Zambia" & dmy(M02_SCRN_OBSSTDAT) < dmy("15-12-2022")))

#remove Zambia old data


save(matData, file = "derived_data/matData.rda")


# neonatal MNH forms
neoForms <- c('M11') #,'M13','M24'

# combine neonatal data
neoData <- dataWide_list[["M11"]]
for (Form in neoForms[-1]){

  data <- dataWide_list[[Form]]

  neoData <- neoData %>%
    full_join(data, by = c("MOMID", "PREGID", "INFANTID", "SITE")) %>%
    distinct()
}

neoData <- neoData %>% 
  left_join((dataWide_list[["M09"]] %>% select("MOMID", "PREGID", "SITE", "M09_FORMCOMPLDAT_MNH09", "M09_INFANTS_FAORRES")),
            by = c("MOMID", "PREGID","SITE")) %>% 
  left_join((dataWide_list[["M09"]] %>% select("MOMID", "PREGID", "SITE", "M09_INFANTID_INF1":"M09_SEX_INF1")),
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF1", "SITE")) %>% 
  left_join((dataWide_list[["M09"]] %>% select("MOMID", "PREGID", "SITE", "M09_INFANTID_INF2":"M09_SEX_INF2")),
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF2", "SITE")) %>% 
  left_join((dataWide_list[["M09"]] %>% select("MOMID", "PREGID", "SITE", "M09_INFANTID_INF3":"M09_SEX_INF3")),
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF3", "SITE")) %>% 
  left_join((dataWide_list[["M09"]] %>% select("MOMID", "PREGID", "SITE", "M09_INFANTID_INF4":"M09_SEX_INF4")),
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF4", "SITE")) 
  
  
save(neoData, file = "derived_data/neoData.rda")
save(matData, neoData, file = "derived_data/dataMerged.rda")
