
#' @title Clean PIAAC 2023 data
#'
#' Following Rowlands paper, we create derived variables and reorder
#'
#' @param data Survey data at individual level
#' @param save Logical indicating whether to save the cleaned data sets
#' @return List of cleaned data sets for literacy, ict and numeracy
#'
#' @import dplyr
#' @importFrom tibble lst
#'
clean_PIAAC_data <- function(data, save = FALSE) {

  # select variables
  model_dat <-
    data |>
    dplyr::select(
      #WORKINGSTATUS2,     # General labour market status
      C2_Q07_T,         # Current work situation
      SUBJSTATUS_BQDI,  # Current work more groups

      #GROSS_ANNUAL_INCOME_OLDBANDS, # Personal earnings before tax in last year
      EARNFLAGC2,  # Earnings including bonuses reporting method (derived)
      D2_Q14b,  # Current work - Earnings - Gross pay
      D2_Q14d6,  # Current work - Earnings - Broad categories - Gross pay per year
      D2_Q16b,   # Current work - Earnings - Total earnings broad categories

      # BUK,                # 19 - Whether born in the UK
      IMYRS_BQDI,  # Years in country; .A “Native born”
      CNT_BRTH,  # country of birth
      A2_Q03a,  # Background - Born in country

      #QxTenu1,            # 763 - Home ownership status
      ##TODO: missing

      #Sex1,               # 6 -Gender (Respondent)
      A2_N02_T,

      #AGE1NET,            # 11 - Age of the respondent (3 band nets)
      CALCAGE,   # Person age
      AGE_BQDI,  # Age in 10 year bands
      AGEG10LFS, # Age in 10 year bands

      #Sesol,              # is English first language
      LNG_L1,  # First language learned at home in childhood and still understood - Respondent (IS0 639-2/T)
      LNG_HOME,  # Language most often spoken at home - Respondent (ISO 639-2/T)
      BORNLANG, # Interactions between place of birth and language status (derived)


      #ETHNICSIMPLE,       # 17 - Simple ethnic group identifier
      # this is missing in the available data extract
      A2_Q03d,  # Background - Mother - Whether born in #CountryName
      A2_Q03e,  # Background - Father - Whether born in #CountryName
      HOMLGRGN,  # Source region of language spoken most often at home (9 regions) (derived)

      #HIQUAL,             # 581 - Highest qualification currently held
      B2_Q01_TC1,  # Highest level of education (Trend PIAAC 1/2, ISCED 97)
      B2_Q01,  # Education - Highest qualification

      #IMDSCOREB4,      # Index of Multiple Deprivation banded into deciles
      ##TODO

      #NSSEC7,             # current/most recent occupation - 7 groups
      ISCO08_C,  # Current Job Occupation - Respondent (ISCO 2008)
      ISCO2C,  # Occupational classification of respondent's job at 2-digit level (ISCO 2008), current job

      # --- outcomes ---

      # literacy
      starts_with("PVLIT"),
      # LEVLIT,  # missing

      # numeracy
      starts_with("PVNUM"),
      # LEVNUM,  # missing

      # ICT
      starts_with("PVPSL"),
      # LEVPSL,  # missing

      # weights
      SPFWT0
    ) |>

    # --- calculate OECD levels ---

    # calculate means of plausible values for each person
    rowwise() %>%
    mutate(PVNUM_AVG = mean(c_across(PVNUM1:PVNUM10))) %>%
    ungroup() %>%
    mutate(LEVNUM = case_when(
      PVNUM_AVG < 176 ~ "Below Level 1",
      PVNUM_AVG >= 176 & PVNUM_AVG < 226 ~ "Level 1",
      PVNUM_AVG >= 226 & PVNUM_AVG < 276 ~ "Level 2",
      PVNUM_AVG >= 276 & PVNUM_AVG < 326 ~ "Level 3",
      PVNUM_AVG >= 326 & PVNUM_AVG < 376 ~ "Level 4",
      PVNUM_AVG >= 376 ~ "Level 5",
      TRUE ~ NA_character_  # missing cases
    )) |>

    rowwise() %>%
    mutate(PVLIT_AVG = mean(c_across(PVLIT1:PVLIT10))) %>%
    ungroup() %>%
    mutate(LEVLIT = case_when(
      PVLIT_AVG < 176 ~ "Below Level 1",
      PVLIT_AVG >= 176 & PVLIT_AVG < 226 ~ "Level 1",
      PVLIT_AVG >= 226 & PVLIT_AVG < 276 ~ "Level 2",
      PVLIT_AVG >= 276 & PVLIT_AVG < 326 ~ "Level 3",
      PVLIT_AVG >= 326 & PVLIT_AVG < 376 ~ "Level 4",
      PVLIT_AVG >= 376 ~ "Level 5",
      TRUE ~ NA_character_
    )) |>

    rowwise() %>%
    mutate(PVPSL_AVG = mean(c_across(PVPSL1:PVPSL10))) %>%
    ungroup() %>%
    mutate(LEVPSL = case_when(
      PVPSL_AVG < 241 ~ "Below Level 1",
      PVPSL_AVG >= 241 & PVPSL_AVG < 291 ~ "Level 1",
      PVPSL_AVG >= 291 & PVPSL_AVG < 341 ~ "Level 2",
      PVPSL_AVG >= 341 ~ "Level 3",
      TRUE ~ NA_character_
    )) |>

    # --- calculate NQF (OK) levels ---

    mutate(
      # literacy
      NQF_LIT = case_when(
        LEVLIT == "Below Level 1" ~ "Entry Level 1 / 2",
        LEVLIT == "Level 1"       ~ "Entry Level 3",
        LEVLIT == "Level 2"       ~ "Level 1",
        LEVLIT %in% c("Level 3", "Level 4", "Level 5") ~ "Level 2 and above",
        TRUE ~ NA_character_ # Handles any missing cases
      ),

      # numeracy
      NQF_NUM = case_when(
        LEVNUM == "Below Level 1" ~ "Entry Level 1",
        LEVNUM == "Level 1"       ~ "Entry Level 2",
        LEVNUM == "Level 2"       ~ "Entry Level 3",
        LEVNUM == "Level 3"       ~ "Level 1",
        LEVNUM %in% c("Level 4", "Level 5") ~ "Level 2 and above",
        TRUE ~ NA_character_
      )
    ) |>

    # remove class
    dplyr::mutate(
      # --- Predictor Variables ---
      EMPLOY = unclass(EMPLOY),
      INCOMX = unclass(INCOMX),
      ENGSTAT = unclass(ENGSTAT),
      TENURE = unclass(TENURE),
      SEX = unclass(SEX),
      AGEBAND = unclass(AGEBAND),
      ETHNIC = unclass(ETHNIC),
      HIQUAG1 = unclass(HIQUAG1),
      RNSSEC = unclass(RNSSEC),
      IMDSCOREB4 = unclass(IMDSCOREB4),

      # --- Outcome Variables ---

      LITERACYTHRESHOLD = unclass(LITERACYTHRESHOLD),
      LITLEV = unclass(LITLEV),
      NUMERACYTHRESHOLD = unclass(NUMERACYTHRESHOLD),
      NUMLEV = unclass(NUMLEV),
      PLEV = unclass(PLEV),

      # --- weights ---

      WTALL = unclass(WTALL),
      WTLIT = unclass(WTLIT),
      WTNUM = unclass(WTNUM),
      WTICT = unclass(WTICT)
    ) |>

    # relabel and order levels
    dplyr::transmute(
      workingstatus = ifelse(EMPLOY %in% c(1,2), yes = 1, no = 0),
      workingstatus = factor(workingstatus,
                             levels = c(0,1), labels = c("No", "Yes")),

      gross_income = ifelse(INCOMX %in% 1:3,
                            "<10000",
                            ifelse(INCOMX %in% 4:12,
                                   ">=10000", "other")),
      gross_income = factor(gross_income,
                            levels = c("<10000", ">=10000", "other")),

      uk_born = factor(ENGSTAT, levels = c(2,1), labels = c("No", "Yes")),  # proxy

      sex = factor(SEX, levels = c(2,1), c("Female", "Male")),

      own_home = ifelse(TENURE == 1, "Yes", "No") |>
        factor(levels = c("No", "Yes")),

      age = ifelse(AGEBAND %in% 1:4, "16-44",
                   ifelse(AGEBAND %in% 5:7, ">=45", "other")) |>
        factor(levels = c("16-44", ">=45", "other")),

      english_lang = factor(ENGSTAT, levels = c(2,1), labels = c("No", "Yes")),

      ethnicity = ifelse(ETHNIC %in% c(1,2,3), 1, 2),
      # ethnicity = ifelse(ETHNIC %in% c(1,2,3,4,5,6), 1, 2),  # include white mixed

      ethnicity = factor(ethnicity, levels = c(1,2),
                         labels = c("White", "BME")),

      qualification = ifelse(HIQUAG1 %in% 1:4, ">=level 2", "<=Level 1") |>
        factor(levels = c("<=Level 1", ">=level 2")),

      imd = case_when(
        IMDSCOREB4 %in% c(1, 2) ~ 5,
        IMDSCOREB4 %in% c(3, 4) ~ 4,
        IMDSCOREB4 %in% c(5, 6) ~ 3,
        IMDSCOREB4 %in% c(7, 8) ~ 2,
        IMDSCOREB4 == 9         ~ 1),

      job_status = ifelse(RNSSEC %in% 1:2, "higher",  # managerial
                          ifelse(RNSSEC == 3, "intermediate",
                                 ifelse(RNSSEC %in% 4:7, "lower", "other"))) |>
        factor(levels = c("lower", "intermediate", "higher", "other")),

      # --- thresholds ---

      lit_thresholdL1 =
        ifelse(NQF_LIT %in% c("Entry Level 1 / 2", "Entry Level 3"), "below",  # EL and below
               ifelse(NQF_LIT %in% c("Level 1", "Level 2 and above"),
                      "above", "other")),

      lit_thresholdL2 = ifelse(NQF_LIT == "Level 2 and above", "above",  # L2 or above
                               ifelse(NQF_LIT %in% c("Entry Level 1 / 2", "Entry Level 3", "Level 1"),
                                      "below", "other")),

      num_thresholdEL3 =
        ifelse(NQF_NUM %in% c("Entry Level 1", "Entry Level 2"), "below",  # EL2 and below
               ifelse(NQF_NUM %in% c("Entry Level 3", "Level 1", "Level 2 and above"),
                      "above", "other")),

      num_thresholdL1 = ifelse(NQF_NUM %in% c("Level 1", "Level 2 and above"), "above",  # L1 or above
                               ifelse(NQF_NUM %in% c("Entry Level 1", "Entry Level 2", "Entry Level 3"),
                                      "below", "other")),
    ) |>

    # remove missing
    dplyr::filter(!is.na(age), age != "other",
                  !is.na(ethnicity))

  # health literacy assessment specific data sets
  # filtered by answered question

  lit <- model_dat |>
    dplyr::filter(lit_thresholdL2 %in% c("above", "below")) |>
    mutate(lit_thresholdL2 = factor(lit_thresholdL2, levels = c("above", "below")),
           lit_thresholdL2_bin = as.integer(lit_thresholdL2) - 1L,
           lit_thresholdL1 = factor(lit_thresholdL1, levels = c("above", "below")),
           lit_thresholdL1_bin = as.integer(lit_thresholdL1) - 1L,
           weights = WTLIT) |>
    select(-num_thresholdEL3, -num_thresholdL1)

  num <- model_dat |>
    dplyr::filter(num_thresholdL1 %in% c("above", "below")) |>
    mutate(num_thresholdL1 = factor(num_thresholdL1, levels = c("above", "below")),
           num_thresholdL1_bin = as.integer(num_thresholdL1) - 1L,
           weights = WTNUM) |>
    select(-num_thresholdEL3, -lit_thresholdL2)

  tibble::lst(lit, num)
}

