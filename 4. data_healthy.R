#**************************************************************************************
#*ReMAPP 
#*Output: healthyOutcome.rda
#*includes: CRIT_s, HEALTHY_ELIGIBLE
#**************************************************************************************
library(tidyverse)
load("derived_data/matData.rda")
load("derived_data/matOutcome.rda")

#**************************************************************************************
#*Create Variables for Aim 1 Criteria (21 vars)
#**************************************************************************************
vars_criteria <- matData %>%
  select(
    SCRNID, MOMID, PREGID, SITE, 
    M00_BRTHDAT, M00_ESTIMATED_AGE, M02_SCRN_OBSSTDAT, M02_CONSENT_IEORRES,
    M03_SMOKE_OECOCCUR, M03_CHEW_BNUT_OECOCCUR, M03_CHEW_OECOCCUR, M03_DRINK_OECOCCUR,
    M05_ANT_PEDAT_V1, M05_WEIGHT_PERES_V1, M05_HEIGHT_PERES_V1, M05_MUAC_PERES_V1,
    M04_PRETERM_RPORRES_V1, M04_PH_PREV_RPORRES_V1, M04_PH_PREVN_RPORRES_V1, M04_PH_LIVE_RPORRES_V1, 
    M04_MISCARRIAGE_RPORRES_V1, M04_MISCARRIAGE_CT_RPORRES_V1, M04_PH_OTH_RPORRES_V1,M04_STILLBIRTH_RPORRES_V1,
    M04_LOWBIRTHWT_RPORRES_V1, M04_MALARIA_EVER_MHOCCUR_V1, 
    M04_CANCER_EVER_MHOCCUR_V1, M04_KIDNEY_EVER_MHOCCUR_V1, M04_CARDIAC_EVER_MHOCCUR_V1,
    M04_HIV_MHOCCUR_V1, M04_HIV_EVER_MHOCCUR_V1, M04_UNPL_CESARIAN_PROCCUR_V1, M04_PREECLAMPSIA_RPORRES_V1,
    M04_GEST_DIAB_RPORRES_V1, M04_PREMATURE_RUPTURE_RPORRES_V1,
    M04_MACROSOMIA_RPORRES_V1, M04_OLIGOHYDRAMNIOS_RPORRES_V1,
    M04_APH_RPORRES_V1, M04_PPH_RPORRES_V1,
    M06_SINGLETON_PERES_V1, 
    M06_BP_SYS_VSORRES_1_V1, M06_BP_SYS_VSORRES_2_V1, M06_BP_SYS_VSORRES_3_V1,
    M06_BP_DIA_VSORRES_1_V1, M06_BP_DIA_VSORRES_2_V1, M06_BP_DIA_VSORRES_3_V1,
    M06_HBV_POC_LBORRES_V1, M06_HBV_POC_LBPERF_V1, M06_HCV_POC_LBORRES_V1, M06_HCV_POC_LBPERF_V1,
    M06_HIV_POC_LBORRES_V1, M06_HIV_POC_LBPERF_V1,
    M08_MN_LBPERF_8_V1, M08_FERRITIN_LBORRES_V1, 
    M08_RBC_LBPERF_2_V1, M08_RBC_THALA_LBORRES_V1, M08_RBC_LBPERF_3_V1, M08_RBC_GLUC6_LBORRES_V1,
    M08_MN_LBPERF_12_V1, M08_CRP_LBORRES_V1, M08_MN_LBPERF_13_V1, M08_AGP_LBORRES_V1
  ) %>%
  left_join(matOutcome %>% 
              select(SCRNID, MOMID, PREGID, SITE, BESTEDD, TRIMES_ENROLL), by = c("SCRNID", "MOMID", "PREGID", "SITE")) %>% 
  distinct(SCRNID, MOMID, PREGID, SITE, .keep_all = TRUE) %>% 
  filter(M02_CONSENT_IEORRES == 1) %>% 
  mutate(
    # 1. age at enrollment
    # age at enrollment
    AGE_ENROLL = case_when(
      !is.na(M02_SCRN_OBSSTDAT) & !is.na(M00_BRTHDAT) & 
        !M00_BRTHDAT %in% c("07/07/1907", "07-07-1907") ~ as.numeric(dmy(M02_SCRN_OBSSTDAT) - dmy(M00_BRTHDAT)) %/% 365,
      !is.na(M00_ESTIMATED_AGE) ~ M00_ESTIMATED_AGE,
      TRUE ~ -5
    ),
    CRIT_AGE = case_when(
      (AGE_ENROLL > 0 & AGE_ENROLL < 18) | AGE_ENROLL > 34 ~ 0,
      AGE_ENROLL >= 18 & AGE_ENROLL <= 34 ~ 1, 
      TRUE ~ 55
    ),
    # 2. GA at enrollment
    # gestational age at enrollment 
    CRIT_GA = case_when(
      TRIMES_ENROLL == 2 ~ 0,
      TRIMES_ENROLL == 1 ~ 1, 
      TRUE ~ 55
    ),
    # 3. BMI 
    # BMI
    BMI = M05_WEIGHT_PERES_V1 / M05_HEIGHT_PERES_V1 / M05_HEIGHT_PERES_V1 * 10000,
    CRIT_BMI = case_when(
      BMI <= 18.5 | BMI >= 30 ~ 0, 
      BMI > 18.5 & BMI < 30 ~ 1, 
      TRUE ~ 55
      ),
    # 4. MUAC mid-upper arm circumference 
    # MUAC
    CRIT_MUAC = case_when(
      M05_MUAC_PERES_V1 <= 23 ~ 0, 
      M05_MUAC_PERES_V1 > 23 ~ 1, 
      TRUE ~ 55
    ),
    # 5. height
    CRIT_HEIGHT = case_when(
      M05_HEIGHT_PERES_V1 <= 153 ~ 0,
      M05_HEIGHT_PERES_V1 > 153 ~ 1, 
      TRUE ~ 55
    ),
    # 6. single fetus
    CRIT_SINGLEPREG = case_when(
      M06_SINGLETON_PERES_V1 == 0 ~ 0,
      M06_SINGLETON_PERES_V1 == 1 ~ 1, 
      TRUE ~ 55
    )) %>% 
  rowwise() %>% 
  mutate(
    # 7. blood pressure
    M06_BP_SYS_V1 = mean(c(M06_BP_SYS_VSORRES_1_V1, M06_BP_SYS_VSORRES_2_V1, M06_BP_SYS_VSORRES_3_V1), na.rm = TRUE),
    M06_BP_DIA_V1 = mean(c(M06_BP_DIA_VSORRES_1_V1, M06_BP_DIA_VSORRES_2_V1, M06_BP_DIA_VSORRES_3_V1), na.rm = TRUE),
    CRIT_BP = case_when(
      M06_BP_SYS_V1 > 0 & M06_BP_SYS_V1 < 140 & M06_BP_DIA_V1 > 0 & M06_BP_DIA_V1 < 90 ~ 1,
      M06_BP_SYS_V1 >= 140 | M06_BP_DIA_V1 >= 90 ~ 0, 
      TRUE ~ 55
    )) %>% 
  ungroup() %>% 
  mutate( 
    # 8. <= 1 miscarriage in two consecutive pregnancies (any previous pregnancy) skip patter not follow correctly
    CRIT_PREV_MISCARR = case_when(
      M04_MISCARRIAGE_RPORRES_V1 == 1 & M04_MISCARRIAGE_CT_RPORRES_V1 > 1 ~ 0,
      M04_PH_PREV_RPORRES_V1 == 0 | # no previous pregnancy (Have you ever been pregnant? Include all live births, stillbirths, miscarriages, or abortions. Do not include the current pregnancy.)
        M04_PH_OTH_RPORRES_V1 == 0 | # How many of these pregnancies ended in a loss? This includes pregnancies that ended in a stillbirth, miscarriage, or an abortion? 
        M04_MISCARRIAGE_RPORRES_V1 == 0 | #(During any of your previous pregnancies or births, did you experience a spontaneous miscarriage (pregnancy loss before 20 weeks GA)
        (M04_MISCARRIAGE_RPORRES_V1 == 1 & M04_MISCARRIAGE_CT_RPORRES_V1 <= 1) ~ 1, #(Specify total # of spontaneous miscarriage: )
      TRUE ~ 55
    ),
    # 9. no previous preterm or low birth weight delivery
    CRIT_PREV_PRETERM_LBW = case_when(
      M04_PRETERM_RPORRES_V1 == 1 | M04_LOWBIRTHWT_RPORRES_V1 == 1 ~ 0,
      M04_PH_PREV_RPORRES_V1 == 0 | (M04_PRETERM_RPORRES_V1 == 0 & M04_LOWBIRTHWT_RPORRES_V1) == 0 ~ 1,
      M04_PRETERM_RPORRES_V1 == 99 | M04_LOWBIRTHWT_RPORRES_V1 == 99 ~ 0, 
      TRUE ~ 55
    ),
    # 10. No previous neonatal or fetal death (stillbirth/neonatal death within first 28 days of live)
    CRIT_PREV_NEODEATH = case_when(
      M04_PH_OTH_RPORRES_V1 > 0 | M04_STILLBIRTH_RPORRES_V1 == 1 ~ 0, #stillbirth,miscarriage or an abortion>0 or experienced a still birth
      M04_PH_PREV_RPORRES_V1 == 0 | #never pregnant
        (M04_PH_PREVN_RPORRES_V1 >= 0 & M04_PH_LIVE_RPORRES_V1 >=0 & M04_PH_PREVN_RPORRES_V1 == M04_PH_LIVE_RPORRES_V1) | #preg # = live preg #
        M04_PH_OTH_RPORRES_V1 == 0 ~ 1,  #stillbirth,miscarriage or an abortion = 0 
      TRUE ~ 55
    ),
    # 11. combine complication
    CRIT_COMPLICATION = case_when(
      M04_UNPL_CESARIAN_PROCCUR_V1 == 1 | M04_PREECLAMPSIA_RPORRES_V1== 1 |  
        M04_GEST_DIAB_RPORRES_V1 == 1 | M04_PREMATURE_RUPTURE_RPORRES_V1 == 1 |  
        M04_MACROSOMIA_RPORRES_V1 == 1 | M04_OLIGOHYDRAMNIOS_RPORRES_V1 == 1 |  
        M04_APH_RPORRES_V1 == 1 |  M04_PPH_RPORRES_V1 == 1 ~ 0,
      M04_PH_PREV_RPORRES_V1 == 0 |
        (M04_UNPL_CESARIAN_PROCCUR_V1 == 0 & M04_PREECLAMPSIA_RPORRES_V1 == 0 & 
        M04_GEST_DIAB_RPORRES_V1 == 0 & M04_PREMATURE_RUPTURE_RPORRES_V1 == 0 & 
        M04_MACROSOMIA_RPORRES_V1 == 0 & M04_OLIGOHYDRAMNIOS_RPORRES_V1 == 0 & 
        M04_APH_RPORRES_V1 == 0 & M04_PPH_RPORRES_V1 == 0) ~ 1,
      M04_UNPL_CESARIAN_PROCCUR_V1 == 99 | M04_PREECLAMPSIA_RPORRES_V1 == 99 |  
        M04_GEST_DIAB_RPORRES_V1 == 99 | M04_PREMATURE_RUPTURE_RPORRES_V1== 99 |    
        M04_MACROSOMIA_RPORRES_V1== 99 | M04_OLIGOHYDRAMNIOS_RPORRES_V1 == 99 |    
        M04_APH_RPORRES_V1 == 99 | M04_PPH_RPORRES_V1 == 99 ~ 0,
      TRUE ~ 55
    ),
    # 12. no cigarette smoking
    CRIT_SMOKE = case_when(
      M03_SMOKE_OECOCCUR == 1 | M03_CHEW_BNUT_OECOCCUR == 1 | M03_CHEW_OECOCCUR == 1 ~ 0,
      M03_SMOKE_OECOCCUR == 0 & M03_CHEW_BNUT_OECOCCUR == 0 & M03_CHEW_OECOCCUR == 0 ~ 1, 
      TRUE ~ 55
    ),
    # 13. no heavy alcohol use (>3 drinks per day, or >7 drinks per week)
    CRIT_DRINK = case_when(
      SITE == "Pakistan" ~ 666,
      M03_DRINK_OECOCCUR == 1 ~ 0,
      M03_DRINK_OECOCCUR == 0 ~ 1,
      M03_DRINK_OECOCCUR == 66 ~ 0,
      M03_DRINK_OECOCCUR == 77 ~ 0, #temporary code for Kenya, check for other country
      TRUE ~ 55 
    ),
    # 14. no HIV M04_HIV_MHOCCUR_V1(1,0.99): M06_HIV_POC_LBPERF_V, M06_HIV_POC_LBORRES_V1(1.0)
    CRIT_HIV = case_when(
        M06_HIV_POC_LBORRES_V1 == 1 ~ 0,#Record HIV results (1,0)
        M06_HIV_POC_LBORRES_V1 == 0 ~ 1, 
        M04_HIV_EVER_MHOCCUR_V1 == 1 | #Have you ever been diagnosed with HIV? (1,0,99)
          M04_HIV_MHOCCUR_V1 == 1 ~ 0, #had HIV since becoming pregnant with the current pregnancy (1,0,99)
        M04_HIV_EVER_MHOCCUR_V1 == 0 & M04_HIV_MHOCCUR_V1 == 0 & M06_HIV_POC_LBPERF_V1 == 0 ~ 1,
        M04_HIV_EVER_MHOCCUR_V1 == 99 | M04_HIV_MHOCCUR_V1 == 99 | 
          M06_HIV_POC_LBPERF_V1 == 0 ~ 0, #Was point-of-care HIV test performed at this visit? (1,0)
        M04_HIV_EVER_MHOCCUR_V1 == 77 | M04_HIV_MHOCCUR_V1 == 77 | 
          M06_HIV_POC_LBPERF_V1 == 77 | M06_HIV_POC_LBORRES_V1 == 77 ~ 0, #Was point-of-care HIV test performed at this visit? (1,0)
        TRUE ~ 55 
    ),
    # 15. no cancer, kidney disease, cardiac disease
    CRIT_CHRONIC = case_when(
      M04_CANCER_EVER_MHOCCUR_V1 == 1 | M04_KIDNEY_EVER_MHOCCUR_V1 == 1 | 
        M04_CARDIAC_EVER_MHOCCUR_V1 == 1 ~ 0,
      M04_CANCER_EVER_MHOCCUR_V1 == 0 & M04_KIDNEY_EVER_MHOCCUR_V1 == 0 & 
        M04_CARDIAC_EVER_MHOCCUR_V1 == 0 ~ 1,
      M04_CANCER_EVER_MHOCCUR_V1 == 99 | M04_KIDNEY_EVER_MHOCCUR_V1 == 99 | 
        M04_CARDIAC_EVER_MHOCCUR_V1 == 99 ~ 0, 
      TRUE ~ 55 
    ),
    # 16. no current malaria infection (per RDT)
    CRIT_MALARIA = case_when(
      M04_MALARIA_EVER_MHOCCUR_V1 == 0 ~ 1,
      M04_MALARIA_EVER_MHOCCUR_V1 == 1 ~ 0,
      M04_MALARIA_EVER_MHOCCUR_V1 == 99 ~ 99, 
      TRUE ~ 55 
    ),
    # 17. no Hepatitis B virus infection
    CRIT_HEPATITISB = case_when(
      M06_HBV_POC_LBORRES_V1 == 1 ~ 0,
      M06_HBV_POC_LBORRES_V1 == 0 ~ 1,
      M06_HBV_POC_LBPERF_V1 == 0 ~ 0, 
      TRUE ~ 55 
    ),
    # 18. no Hepatitis C virus infection
    CRIT_HEPATITISC = case_when(
      M06_HCV_POC_LBORRES_V1 == 1 ~ 0,
      M06_HCV_POC_LBORRES_V1 == 0 ~ 1,
      M06_HCV_POC_LBPERF_V1 == 0 ~ 0, 
      TRUE ~ 55 
  ),
    # 19. no hemoglobinopathies (include glucose-6-phosphate dehydrogenase deficiency or not?)
    CRIT_HEMOGLOBINOPATHIES = case_when(
      M08_RBC_THALA_LBORRES_V1 == 0 & M08_RBC_GLUC6_LBORRES_V1 == 0 ~ 1,
      M08_RBC_THALA_LBORRES_V1 == 1 | M08_RBC_GLUC6_LBORRES_V1 == 1 ~ 0,
      M08_RBC_LBPERF_2_V1 == 0 | M08_RBC_LBPERF_3_V1 == 0 ~ 0, 
      TRUE ~ 55 
    ),
    # 20. no iron deficiency (not iron deficient: serum ferritin > 15 mcg/L) 
    CRIT_IRON = case_when(
      M08_FERRITIN_LBORRES_V1 > 15*10 ~ 1,
      M08_FERRITIN_LBORRES_V1 >0 & M08_FERRITIN_LBORRES_V1 <= 15*10 ~ 0,
      M08_MN_LBPERF_8_V1 == 0 ~ 0, 
      TRUE ~ 55 
    ),
    # 21. no subclinical inflammation (CRP≤5 and/or AGP≤1) 
    CRIT_INFLAM = case_when(
      M08_CRP_LBORRES_V1 > 0 & M08_CRP_LBORRES_V1 <= 5 & 
        M08_AGP_LBORRES_V1 >0 & M08_AGP_LBORRES_V1 <= 1 ~ 1,
      M08_CRP_LBORRES_V1 > 5 | M08_AGP_LBORRES_V1 > 1 ~ 0,
      M08_MN_LBPERF_12_V1 == 0 | M08_MN_LBPERF_13_V1 == 0 ~ 0, 
      TRUE ~ 55 
    )) %>%
  select(SCRNID, MOMID, PREGID, SITE,
         starts_with("CRIT_"))


#**************************************************************************************
#*check eligibility and save data
#**************************************************************************************
#code 666 for any not applicable by site
healthyOutcome <- vars_criteria %>% 
  rowwise() %>%
  mutate(HEALTHY_CHECK = sum(across(starts_with("CRIT_"), ~ .x %in% c(1, 0, 666)), na.rm = TRUE)) %>% 
  mutate(
    HEALTHY_ELIGIBLE = case_when(
      if_all(starts_with("CRIT_"), ~.x %in% c(1, 666)) ~ 1, #eligible
      if_any(starts_with("CRIT_"), ~.x == 0) ~ 0, #Not eligible
      HEALTHY_CHECK < 21 ~ 3 #pending 
    ) )%>%
  ungroup() 

 save(healthyOutcome, file = "derived_data/healthyOutcome.rda")
saveRDS(healthyOutcome, file = "derived_data/healthyOutcome.rds")
