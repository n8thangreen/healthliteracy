
#' @import dplyr
#'
clean_aps_data <- function() {
  aps_data <-
    read.delim(here::here("../../data/annual population survey/UKDA-9451-tab/tab/apsh_jd24_eul_phhwta22.tab"))

  res <- aps_data |>
    as_tibble() |>
    select(
      LEVQUL22,  # highest qualification
      BANDG,     # Expected gross earnings, <10,000 = < 1.17 code
      CRYOX7_EUL_Sub,   # Country of birth, 1 UNITED KINGDOM
      CRYOX7_EUL_Main,
      CRY12,     # (921) England, (924) Wales, (923) Scotland, (922) Northern Ireland, (926) UK, Britain (don’t know country)

      ## MISSING
      # LANG,      # First language at home, (1) English

      NSECMJ20,   # NS-SEC major group (SOC2020 based)

      SEX,        # Gender
      AGE,        # Age (continuous) or AGEBAND
      ETHUKEUL,   # Ethnicity (EUL simplified)
      ILODEFR,    # Economic Activity (for Working Status)
      TEN1        # Tenure (for Own Home)
    ) |>
    mutate(
      # english_lang = ifelse(LANG == 1, "Yes", "No"),

      uk_born = ifelse(CRYOX7_EUL_Sub == 1, "Yes" ,"No"),

      gross_income = ifelse(as.numeric(sub("^.*\\.", "", BANDG)) %in% 1:16,
                            "<10000",
                            ">=10000"),

      qualification = ifelse(LEVQUL22 %in% c(1,2,3,4,5,6,7),
                             ">=Level 2", "<=Level 1"),

      job_status = ifelse(NSECMJ20 %in% c(1,2), "higher",  # managerial
                          ifelse(NSECMJ20 %in% c(3,4), "intermediate",
                                 ifelse(NSECMJ20 %in% c(5,6,7), "lower", "other"))),

      # also in Newham resident survey ---

      sex = ifelse(SEX == 1, "Male", "Female"),

      age = ifelse(AGE >= 16 & AGE <= 44, "16-44",
                   ifelse(AGE >= 45, ">=45", "other")),

      ethnicity = ifelse(ETHUKEUL == 1, "White", "BME"),

      workingstatus = ifelse(ILODEFR == 1, "Yes", "No"),

      own_home = ifelse(TEN1 %in% c(1, 2), "Yes", "No"),

      # doesn't provide IMD
      # --- TODO --- (approx uniform across quintiles)
      imd = sample(c("1", "2", "3", "4", "5"), size = n(), replace = TRUE)
    ) |>

    select(qualification, gross_income, uk_born,
           # english_lang,
           job_status,
           sex, age, ethnicity, workingstatus, own_home, imd) |>
    filter(age != "other") |>
    mutate(across(where(is.character), as.factor))
}
