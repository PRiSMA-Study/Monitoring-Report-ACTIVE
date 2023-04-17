#function for ANC visit
visit_func <- function(data, varDates, varDate, varVisits){
  
  data1 <- data %>% 
    select(MOMID, PREGID, SITE, BESTEDD,
           matches(varVisits),
           matches(varDates)) %>% 
    filter(!is.na(eval(parse(text = paste0(varVisits,"_TOT"))))) %>% ###
    pivot_longer(matches(varDates)| matches(paste0("^",varVisits,"_TYPE_V\\d+")), ###
                 names_to = c(".value", "ANC_ORDER"),
                 names_pattern = "^M\\d{2}_(.+)_V(\\d+)"
    ) %>% 
    mutate(ANC_ORDER = as.numeric(ANC_ORDER),
           GA_WKS_MISS = as.numeric(max(dmy(get(varDate)), na.rm = TRUE) - (BESTEDD - 280)) %/% 7) %>%
    group_by(MOMID, PREGID) %>%
    arrange(ANC_ORDER) %>%
    slice(c(1:(eval(parse(text = paste0(varVisits,"_TOT")))[1] + 1))) %>% ###
    mutate(GA_WKS = as.numeric(dmy(get(varDate)) - (BESTEDD - 280)) %/% 7) %>%
    summarise(
      ANCLESS20 = any(VISIT_TYPE == 1)*1,
      ANC20 = any(VISIT_TYPE == 2)*1,
      ANC28 = any(VISIT_TYPE == 3)*1,
      ANC32 = any(VISIT_TYPE == 4)*1,
      ANC36 = any(VISIT_TYPE == 5)*1,
      US3238 = any(GA_WKS >= 32 & GA_WKS <= 38)*1,
      DANC20 = any((GA_WKS_MISS > 22 & is.na(VISIT_TYPE)) | VISIT_TYPE == 2)*1,
      DANC28 = any((GA_WKS > 30 & is.na(VISIT_TYPE)) | VISIT_TYPE == 3)*1,
      DANC32 = any((GA_WKS > 33 & is.na(VISIT_TYPE)) | VISIT_TYPE == 4)*1,
      DANC36 = any((GA_WKS > 38 & is.na(VISIT_TYPE)) | VISIT_TYPE == 5)*1,
      SITE = SITE[1]) %>% ungroup()
  
  data1 %>% 
    group_by(SITE) %>% 
    summarise(
      "ANC < 20" = paste0(
        format(sum(ANCLESS20 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
        " (",
        format(round(sum(ANCLESS20 == 1, na.rm = TRUE)/n()*100, 2), nsmall = 2, ndigits = 2),
        ")"),
      "ANC = 20" = paste0(
        format(sum(ANC20 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
        " (",
        format(round(sum(ANC20 == 1, na.rm = TRUE)/sum(DANC20 == 1, na.rm = TRUE)*100, 2), nsmall = 2, ndigits = 2),
        ")"),
      "ANC = 28" = paste0(
        format(sum(ANC28 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
        " (",
        format(round(sum(ANC28 == 1, na.rm = TRUE)/sum(DANC28 == 1, na.rm = TRUE)*100, 2), nsmall = 2, ndigits = 2),
        ")"),
      "ANC = 32" = paste0(
        format(sum(ANC32 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
        " (",
        format(round(sum(ANC32 == 1, na.rm = TRUE)/sum(DANC32 == 1, na.rm = TRUE)*100, 2), nsmall = 2, ndigits = 2),
        ")"),
      "ANC = 36" = paste0(
        format(sum(ANC36 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
        " (",
        format(round(sum(ANC36 == 1, na.rm = TRUE)/sum(DANC36 == 1, na.rm = TRUE)*100, 2), nsmall = 2, ndigits = 2),
        ")"),
      "Ultrasound at 32-28 Weeks" = paste0(
        format(sum(US3238 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
        " (",
        format(round(sum(US3238  == 1, na.rm = TRUE)/sum(DANC32 == 1, na.rm = TRUE)*100, 2), nsmall = 2, ndigits = 2),
        ")")
    ) %>% 
    t() %>% 
    as.data.frame() %>% 
    `colnames<-`(c(.[1,])) %>% 
    slice(-1) %>% 
    add_column(
      .before = 1,
      "All sites" = data1 %>% 
        summarise(
          "ANC < 20" = paste0(
            format(sum(ANCLESS20 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
            " (",
            format(round(sum(ANCLESS20 == 1, na.rm = TRUE)/n()*100, 2), nsmall = 2, ndigits = 2),
            ")"),
          "ANC = 20" = paste0(
            format(sum(ANC20 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
            " (",
            format(round(sum(ANC20 == 1, na.rm = TRUE)/sum(DANC20 == 1, na.rm = TRUE)*100, 2), nsmall = 2, ndigits = 2),
            ")"),
          "ANC = 28" = paste0(
            format(sum(ANC28 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
            " (",
            format(round(sum(ANC28 == 1, na.rm = TRUE)/sum(DANC28 == 1, na.rm = TRUE)*100, 2), nsmall = 2, ndigits = 2),
            ")"),
          "ANC = 32" = paste0(
            format(sum(ANC32 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
            " (",
            format(round(sum(ANC32 == 1, na.rm = TRUE)/sum(DANC32 == 1, na.rm = TRUE)*100, 2), nsmall = 2, ndigits = 2),
            ")"),
          "ANC = 36" = paste0(
            format(sum(ANC36 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
            " (",
            format(round(sum(ANC36 == 1, na.rm = TRUE)/sum(DANC36 == 1, na.rm = TRUE)*100, 2), nsmall = 2, ndigits = 2),
            ")"),
          "Ultrasound at 32-28 Weeks" = paste0(
            format(sum(US3238 == 1, na.rm = TRUE), nsmall = 2, ndigits = 2),
            " (",
            format(round(sum(US3238  == 1, na.rm = TRUE)/sum(DANC32 == 1, na.rm = TRUE)*100, 2), nsmall = 2, ndigits = 2),
            ")")
        )  %>% 
        t() %>% as.data.frame() %>% unlist() 
    )
}
