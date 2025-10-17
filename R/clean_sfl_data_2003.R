
#' @title Clean skills for life survey 2003 data
#'
#' Following Rowlands paper, we create derived variables and reorder
#' Want to correspond to SfL 2011 data for comparison.
#'
#' @param data Survey data at individual level i.e. raw Skills for Life data
#' @param save Logical indicating whether to save the cleaned data sets
#' @return List of cleaned data sets for literacy, ict and numeracy
#'
#' @import dplyr
#' @importFrom tibble lst
#'
clean_sfl_data_2003 <- function(data, save = FALSE) {

  # select variables
  model_dat <-
    data |>
    dplyr::select(
      EMPLOY,   #WORKINGSTATUS2,     # General labour market status

      INCOMX,   #GROSS_ANNUAL_INCOME_OLDBANDS, # Personal earnings before tax in last year

      #missing  # BUK,                # 19 - Whether born in the UK
      ENGSTAT,  # English as first language

      TENURE,   #QxTenu1,            # 763 - Home ownership status
      SEX,      #Sex1,               # 6 -Gender (Respondent)
      AGEBAND,  #AGE1NET,            # 11 - Age of the respondent (3 band nets)
      ENGSTAT,  #Sesol,              # is English first language
      ETHNIC,   #ETHNICSIMPLE,       # 17 - Simple ethnic group identifier
      HIQUAG1,  #HIQUAL,             # 581 - Highest qualification currently held
      IMDSCOREB4,  #IMDSCOREB4,      # Index of Multiple Deprivation banded into deciles
      RNSSEC,   #NSSEC7,             # 828	- NS SEC respondent - current/most recent occupation - 7 groups

      # --- outcomes ---

      # literacy
      LITERACYSCORENET,    # 1.00-EL3 or below; 2.00-L1; 3.00-L2  # literacy level
      LITERACYTHRESHOLD,   # Below Adequate (EL and below)  # literacy threshold
      LITLEV,              # EL1 or below; EL2; EL3; L1; L2 or above

      # numeracy
      NUMERACYSCORENET,   #NumeracyScoreA_1,                  # numeracy level
      NUMERACYTHRESHOLD,  #starts_with("NumeracyThreshold"),  # numeracy threshold,
      NUMLEV,

      # ICT
      PLEV,  # ICT Practical assessment level - EL/L1+
      # MultipleChoiceLevelA_1,            # ICT level
      # MultipleChoiceLevelA_1Thres,       # ICT threshold

      # weights
      WTALL,  #rimweight2003,
      WTLIT,  #rimweightLIT2003,
      WTNUM,  #rimweightNUM2003,
      WTICT,  #rimweightICT2003,
      # rimweightNUMICT2003,
      # rimweightLITICT2003,
      WTBOTH  #rimweightLITNUM2003
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
                   ifelse(AGEBAND %in% 5:7, ">=45", NA)) |>
        factor(levels = c("16-44", ">=45")),

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
        ifelse(LITERACYTHRESHOLD == 1, "below",  # EL and below
               ifelse(LITERACYTHRESHOLD == 2, "above", "other")),

      lit_thresholdL2 = ifelse(LITLEV == 5, "above",  # L2 or above
                               ifelse(LITLEV %in% 1:4, "below", "other")),

      num_thresholdEL3 =
        ifelse(NUMERACYTHRESHOLD == 1, "below",  # EL2 and below
               ifelse(NUMERACYTHRESHOLD == 2, "above", "other")),

      num_thresholdL1 = ifelse(NUMLEV == 4:5, "above",  # L1 or above
                               ifelse(NUMLEV %in% 1:3, "below", "other")),

      ict_thresholdEL3 =
        ifelse(PLEV == 1, "below",  ##TODO: EL or below, but want EL2 and below for comparison ie without EL3
               ifelse(PLEV == 2, "above", "other")),

      WTALL, WTLIT, WTNUM, WTICT
      ) |>

    # remove missing
    dplyr::filter(!is.na(age),
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
    select(-WTLIT, -WTNUM, -WTICT, -WTALL,
           -num_thresholdEL3, -num_thresholdL1, -ict_thresholdEL3)

  num <- model_dat |>
    dplyr::filter(num_thresholdL1 %in% c("above", "below")) |>
    mutate(num_thresholdL1 = factor(num_thresholdL1, levels = c("above", "below")),
           num_thresholdL1_bin = as.integer(num_thresholdL1) - 1L,
           weights = WTNUM) |>
    select(-WTLIT, -WTNUM, -WTICT, -WTALL,
           -num_thresholdEL3, -lit_thresholdL2, -ict_thresholdEL3)

  ict <- model_dat |>
    dplyr::filter(ict_thresholdEL3 %in% c("above", "below")) |>
    mutate(ict_thresholdEL3 = factor(ict_thresholdEL3, levels = c("above", "below")),
           ict_thresholdEL3_bin = as.integer(ict_thresholdEL3) - 1L,
           weights = WTICT) |>
    select(-WTLIT, -WTNUM, -WTICT, -WTALL,
           -num_thresholdEL3, -num_thresholdL1, -lit_thresholdL2)

  tibble::lst(lit, num, ict)
}

