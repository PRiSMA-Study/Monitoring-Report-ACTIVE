#**************************************************************************************
#*Neo data
#*Output: neoOutcome.rda

#**************************************************************************************
library(tidyverse)
load("derived_data/dataMerged.rda")
load("derived_data/matOutcome.rda")


#******************************** Create Variables for Birth Outcome
#* vars: NEO_GADELIV, NEO_GADELIV_WKS, NEO_GADELIV_DAYS, NEO_LB

#first clean the multiple birth results
vars_birth <- neoData %>%
  # keep those who have delivered baby
  filter(!is.na(M09_FORMCOMPLDAT_MNH09)) %>%
  select(MOMID, PREGID, INFANTID, SITE, M09_INFANTS_FAORRES,
         matches("^M09_BIRTH_DSTERM_INF\\d"),
         matches("^M09_DELIV_DSSTDAT_INF\\d"), 
         matches("^M09_SEX_INF\\d"),
         matches("^M09_CRY_CEOCCUR_INF\\d"),
         matches("^M09_FHR_VSTAT_INF\\d"),
         matches("^M09_MACER_CEOCCUR_INF\\d"),
         matches("^M09_CORD_PULS_CEOCCUR_INF\\d")
  ) %>%
  pivot_longer(!c(MOMID, PREGID, INFANTID, SITE, M09_INFANTS_FAORRES),
               names_to = c(".value", "INFANT_ORDER"),
               names_pattern = "^M09_(.+)_INF(\\d+)"
  ) %>%
  filter(!is.na(BIRTH_DSTERM)) %>% 
  left_join(matOutcome %>%
              select(MOMID, PREGID, SITE, BESTEDD), by = c("MOMID", "PREGID", "SITE")) %>%
  mutate(
    # birth date
    NEO_DSSTDAT = DELIV_DSSTDAT,
    # live birth
    NEO_LB = case_when(
      # alive
      BIRTH_DSTERM == 1 ~ 1,
      # death
      BIRTH_DSTERM == 2 ~ 0
    ),
    # gestational age at birth
    NEO_GADELIV = as.numeric(dmy(DELIV_DSSTDAT) - (BESTEDD - 280)) / 7,
    # gestational age outlier
    NEO_GADELIV_OUTLIER = case_when(
      # outlier for live birth
      NEO_LB == 1 & (NEO_GADELIV <=28 | NEO_GADELIV > 42) ~ 1,
      # outlier for stillbirth
      NEO_LB == 0 & (NEO_GADELIV <=20 | NEO_GADELIV > 42) ~ 1,
      # normal range
      !is.na(NEO_GADELIV) & (NEO_LB == 1 | NEO_LB == 0) ~ 0,
      # missing
      TRUE ~ 99
    ),
    NEO_GADELIV = ifelse(NEO_GADELIV_OUTLIER == 0, NEO_GADELIV, NA)
  ) %>%
  select(MOMID, PREGID, INFANTID, SITE, NEO_LB, NEO_GADELIV, BESTEDD, NEO_DSSTDAT, 
         BIRTH_DSTERM, CRY_CEOCCUR, FHR_VSTAT, MACER_CEOCCUR, CORD_PULS_CEOCCUR)


#******************************** Create Variables for Preterm Birth
#* vars: NEO_PRETERM_LB, NEO_PRETERM_TB

vars_preterm <- vars_birth %>%
  mutate(
    # preterm of total birth (live + stillbirth)
    NEO_PRETERM_TB = case_when(
    # extremely preterm
    NEO_GADELIV < 28 ~ 1,
    # very preterm
    NEO_GADELIV >= 28 & NEO_GADELIV < 32 ~ 2,
    # moderate to late
    NEO_GADELIV >= 32 & NEO_GADELIV < 37 ~ 3,
    # not preterm
    NEO_GADELIV >= 37 ~ 4,
    # missing
    TRUE ~ 99
  )) %>%
  select(MOMID, PREGID, INFANTID, SITE, NEO_PRETERM_TB)

table(vars_preterm$NEO_PRETERM_TB, useNA = "ifany")



#******************************** Create Variables for Stillbirth
#* vars: NEO_SB28D, NEO_SB_CDC

vars_sb <- neoData %>% 
  # keep infants who have completed M11 visit
  filter(!is.na(M11_FORMCOMPLDAT_MNH11)) %>% 
  select(MOMID, PREGID, INFANTID, SITE, M11_BW_FAORRES) %>% 
  left_join(vars_birth %>% 
              select(MOMID, PREGID, INFANTID, SITE,
                     BIRTH_DSTERM, CRY_CEOCCUR, FHR_VSTAT, MACER_CEOCCUR, CORD_PULS_CEOCCUR, NEO_LB, NEO_GADELIV), 
            by = c("MOMID", "PREGID", "INFANTID", "SITE")) %>% 
  distinct() %>% 
  mutate(
    # is fetal death stillbirth ???
    NEO_SB = case_when(NEO_LB == 0 ~ 1,
                       NEO_LB == 1 ~ 0,
                       TRUE ~ 99),
    NEO_SBTYPE = case_when(
      #live birth
    BIRTH_DSTERM == 1 ~ 1,
    # macerated  still birth
    BIRTH_DSTERM == 2 & (MACER_CEOCCUR == 1) ~ 3,
    # fresh still birth
    BIRTH_DSTERM == 2 & (MACER_CEOCCUR == 0 | CRY_CEOCCUR == 1 | FHR_VSTAT == 1 | CORD_PULS_CEOCCUR == 1) ~ 2
    ),
    NEO_SB20D = case_when(
      NEO_SB == 1 & (NEO_GADELIV > 20 |  M11_BW_FAORRES > 0.5) ~ 1,
      TRUE ~ NA_real_), 
    NEO_SB28D = case_when(
      NEO_SB == 1 & (NEO_GADELIV > 28 |  M11_BW_FAORRES > 1) ~ 1,
      TRUE ~ NA_real_)
  ) %>%
  mutate(NEO_SB_CDC = case_when(
    NEO_SB == 1 & NEO_GADELIV >= 20 & NEO_GADELIV < 28 ~ 1,
    NEO_SB == 1 & NEO_GADELIV >= 28 & NEO_GADELIV < 37 ~ 2,
    NEO_SB == 1 & NEO_GADELIV >= 37 ~ 3,
    NEO_SB == 1 | NEO_SB == 0 ~ 0,
    TRUE ~ 99
  )) %>%
  select(MOMID, PREGID, INFANTID, SITE, NEO_SB, NEO_SBTYPE, NEO_SB20D, NEO_SB28D, NEO_SB_CDC)


#******************************** Create Variables for Low Birth Weight
#* vars: NEO_LBW

vars_LBW <- neoData %>%
  filter(!is.na(M11_FORMCOMPLDAT_MNH11)) %>% 
  select(MOMID, PREGID, INFANTID, SITE, M11_BW_FAORRES) %>% 
  distinct() %>% 
  mutate(
    NEO_LBW = case_when(
      M11_BW_FAORRES < 1.5 ~ 1,
      M11_BW_FAORRES >= 1.5 & M11_BW_FAORRES < 2.5 ~ 2,
      M11_BW_FAORRES >= 2.5 & M11_BW_FAORRES < 4 ~ 3,
      M11_BW_FAORRES >= 4 ~ 4
    )) %>% 
  select(MOMID, PREGID, INFANTID, SITE, NEO_LBW)

table(vars_LBW$NEO_LBW, useNA = "ifany")


#******************************** Merge Birth outcome
#* check the duplicates
birthOutcome <- vars_LBW %>% 
  full_join(vars_preterm, by = c("MOMID", "PREGID", "INFANTID", "SITE")) %>% 
  full_join(vars_sb, by = c("MOMID", "PREGID", "INFANTID", "SITE")) %>% 
  left_join(vars_birth, by = c("MOMID", "PREGID", "INFANTID", "SITE")) %>% 
  left_join(matData %>% 
              select(MOMID, PREGID, INFANTID, SITE), 
            by = c("MOMID", "PREGID", "INFANTID", "SITE")) 

save(birthOutcome, file = "derived_data/birthOutcome.rda")
saveRDS(birthOutcome, file = "derived_data/birthOutcome.rds")


