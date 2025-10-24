
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

  qualif_mapping <- tibble::tribble(
    ~PIAAC,                              ~ISCED_1997,                            ~UK_RQF,         ~examples,
    #-----------------------------------|---------------------------------------|----------------|--------------------------------------------------------------------------------|
    "No formal qualification or below ISCED 1",    "Below ISCED 1",               "Entry Level",   "Entry Level certificates/diplomas, Skills for Life",
    "ISCED 1",                              "ISCED 1",                            "Below Level 1", "Primary school education (Key Stages 1 & 2)",
    "ISCED 2",                              "ISCED 2",                            "Level 1",       "GCSEs (grades 3-1 or D-G), NVQ Level 1",
    "ISCED 3C shorter than 2 years",        "ISCED 3",                            "Level 2",       "GCSEs (grades 9-4 or A*-C), BTEC Level 2, NVQ Level 2",
    "ISCED 3C (2y+), 3A-B, 3 (2y+)",        "ISCED 3",                            "Level 3",       "A-Levels, T-Levels, BTEC Level 3 National Diploma, Access to HE Diploma",
    "ISCED 4C, 4A-B, 4",                    "ISCED 4",                            "Level 4",       "Certificate of Higher Education (CertHE), Higher National Certificate (HNC)",
    "ISCED 5B",                             "ISCED 5",                            "Level 5",       "Foundation Degree, Higher National Diploma (HND), Diploma of Higher Education (DipHE)",
    "ISCED 5A, bachelor degree",            "ISCED 5 (leading to Bachelor's)",    "Level 6",       "Bachelor's Degree (e.g., BA, BSc), Graduate Certificate",
    "ISCED 5A, master degree",              "ISCED 5 (leading to Master's)",      "Level 7",       "Master's Degree (e.g., MA, MSc), PGCE, Postgraduate Diploma",
    "ISCED 6",                              "ISCED 6",                            "Level 8",       "Doctorate (PhD, DPhil)"
  )

  # select variables
  model_dat <-
    data |>
    dplyr::select(
      #WORKINGSTATUS2,     # General labour market status
      C2_Q07_T,         # Current work situation
      SUBJSTATUS_BQDI,  # Current work more groups

      #GROSS_ANNUAL_INCOME_OLDBANDS, # Personal earnings before tax in last year
      #
      YEARLYINCPR,  # Yearly income percentile rank category (derived); 1: Less than #10%; 2: #10% to less than #25%; 3: #25% to less than #50%; 4: #50% to less than #75%; 5: #75% to less than #90%; 6: #90% or more
      # # missing / suppressed
      # D2_Q14d6,  # Current work - Earnings - Broad categories - Gross pay per year

      # BUK,                # 19 - Whether born in the UK
      A2_Q03a,  # Background - Born in country

      #QxTenu1,            # 763 - Home ownership status
      ##TODO: missing

      #Sex1,               # 6 -Gender (Respondent)
      A2_N02_T,

      #AGE1NET,            # 11 - Age of the respondent (3 band nets)
      # CALCAGE,   # Person age
      # AGE_BQDI,  # Age in 10 year bands
      AGEG10LFS, # Age in 10 year bands

      #Sesol,              # is English first language
      BORNLANG, # Interactions between place of birth and language status (derived)

      ## missing
      # #ETHNICSIMPLE,       # 17 - Simple ethnic group identifier
      # # this is missing in the available data extract
      # LNG_L1,  # First language learned at home in childhood and still understood - Respondent (IS0 639-2/T)
      # LNG_HOME,  # Language most often spoken at home - Respondent (ISO 639-2/T)
      # CNT_BRTH,  # country of birth
      # A2_Q03d,  # Background - Mother - Whether born in #CountryName
      # A2_Q03e,  # Background - Father - Whether born in #CountryName
      # HOMLGRGN,  # Source region of language spoken most often at home (9 regions) (derived)

      #HIQUAL,             # 581 - Highest qualification currently held
      B2_Q01_TC1,  # Highest level of education (Trend PIAAC 1/2, ISCED 97)
      B2_Q01,  # Education - Highest qualification

      #IMDSCOREB4,      # Index of Multiple Deprivation banded into deciles
      ##TODO

      #NSSEC7,             # current/most recent occupation - 7 groups

      # --- outcomes ---

      # literacy
      starts_with("PVLIT"),
      # LEVLIT,  # missing

      # numeracy
      starts_with("PVNUM"),
      # LEVNUM,  # missing

      # # ICT  # not available
      # starts_with("PVPSL"),
      # LEVPSL,  # missing

      # adaptive problem solving
      starts_with("PVAPS"),

      # weights
      SPFWT0
    ) |>

    # --- calculate OECD levels ---

    # calculate means of plausible values for each person

    # numeracy
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

    # literacy
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

    # # ICT
    # rowwise() %>%
    # mutate(PVPSL_AVG = mean(c_across(PVPSL1:PVPSL10))) %>%
    # ungroup() %>%
    # mutate(LEVPSL = case_when(
    #   PVPSL_AVG < 241 ~ "Below Level 1",
    #   PVPSL_AVG >= 241 & PVPSL_AVG < 291 ~ "Level 1",
    #   PVPSL_AVG >= 291 & PVPSL_AVG < 341 ~ "Level 2",
    #   PVPSL_AVG >= 341 ~ "Level 3",
    #   TRUE ~ NA_character_
    # )) |>

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

    # relabel and order levels
    dplyr::transmute(
      workingstatus = ifelse(C2_Q07_T == 1, yes = 1, no = 0),
      workingstatus = factor(workingstatus,
                             levels = c(0,1), labels = c("No", "Yes")),

      # percentiles only available
      # gross_income = ifelse(YEARLYINCPR == 1,
      #                       "<10pc",
      #                       ifelse(YEARLYINCPR %in% 2:6,
      #                              ">=10pc", "other")) |>
      #   factor(levels = c("<10pc", ">=10pc", "other")),

      uk_born = factor(A2_Q03a, levels = c(2,1), labels = c("No", "Yes")),

      sex = factor(A2_N02_T, levels = c(2,1), c("Female", "Male")),

      age = ifelse(AGEG10LFS %in% 1:3, "16-44",
                   ifelse(AGEG10LFS %in% 4:5, ">=45", "other")) |>
        factor(levels = c("16-44", ">=45", "other")),

      english_lang = ifelse(BORNLANG %in% c(1,3), 1, 2) |>
        factor(levels = c(2,1), labels = c("No", "Yes")),

      ## missing
      # ethnicity = ifelse(ETHNIC %in% c(1,2,3), 1, 2) |>
      #   factor(levels = c(1,2), labels = c("White", "BME")),

      qualification = ifelse(B2_Q01_TC1 %in% 1:3, "<=Level 1", ">=level 2") |>
        factor(levels = c("<=Level 1", ">=level 2")),

      ## missing
      # job_status = ifelse(RNSSEC %in% 1:2, "higher",  # managerial
      #                     ifelse(RNSSEC == 3, "intermediate",
      #                            ifelse(RNSSEC %in% 4:7, "lower", "other"))) |>
      #   factor(levels = c("lower", "intermediate", "higher", "other")),

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
      SPFWT0
    ) |>

    # remove missing
    dplyr::filter(!is.na(age),
                  !is.na(uk_born),
                  !is.na(sex)
                  # !is.na(ethnicity)
                  ) |>

    mutate(age = droplevels(age))

  # health literacy assessment specific data sets
  # filtered by answered question

  lit <- model_dat |>
    dplyr::filter(lit_thresholdL2 %in% c("above", "below")) |>
    mutate(lit_thresholdL2 = factor(lit_thresholdL2, levels = c("above", "below")),
           lit_thresholdL2_bin = as.integer(lit_thresholdL2) - 1L,
           lit_thresholdL1 = factor(lit_thresholdL1, levels = c("above", "below")),
           lit_thresholdL1_bin = as.integer(lit_thresholdL1) - 1L,
           weights = SPFWT0) |>
    select(-num_thresholdEL3, -num_thresholdL1, -SPFWT0)

  num <- model_dat |>
    dplyr::filter(num_thresholdL1 %in% c("above", "below")) |>
    mutate(num_thresholdL1 = factor(num_thresholdL1, levels = c("above", "below")),
           num_thresholdL1_bin = as.integer(num_thresholdL1) - 1L,
           weights = SPFWT0) |>
    select(-num_thresholdEL3, -lit_thresholdL2, -SPFWT0)

  tibble::lst(lit, num)
}

