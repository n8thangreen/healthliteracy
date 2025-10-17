
#' @title Clean skills for life survey 2011 data
#'
#' Following Rowlands paper, we create derived variables and reorder
#'
#' @param data Survey data at individual level i.e. raw Skills for Life data
#' @param save Logical indicating whether to save the cleaned data sets
#' @return List of cleaned data sets for literacy, ict and numeracy
#'
#' @import dplyr
#' @importFrom tibble lst
#'
clean_sfl_data_2011 <- function(data, save = FALSE) {

  # select variables
  model_dat <-
    data |>
    dplyr::select(
      WORKINGSTATUS2,     # 724 - Whether working (paid work, government training scheme, own business, or unpaid work for relatives)
      GROSS_ANNUAL_INCOME_OLDBANDS,  # 815 - Banded annual gross income for employees and self-employed (banded in line with 2003)
      BUK,                # 19 - Whether born in the UK
      QxTenu1,            # 763 - Home ownership status
      Sex1,               # 6 -Gender (Respondent)
      AGE1NET,            # 11 - Age of the respondent (3 band nets)
      Sesol,              # is English first language
      ETHNICSIMPLE,       # 17 - Simple ethnic group identifier
      HIQUAL,             # 581 - Highest qualification currently held
      CLITSPEAK,          # ENFL everyday English skills (literacy and speaking)
      IMDSCOREB4,         # Index of Multiple Deprivation banded into deciles
      NSSEC7,             # 828	- NS SEC respondent - current/most recent occupation - 7 groups

      # --- outcomes ---

      LiteracyScoreA_1,                  # literacy level
      starts_with("LiteracyThreshold"),  # literacy threshold
      NumeracyScoreA_1,                  # numeracy level
      starts_with("NumeracyThreshold"),  # numeracy threshold,
      MultipleChoiceLevelA_1,            # ICT level
      MultipleChoiceLevelA_1Thres,       # ICT threshold

      # --- weights ---

      rimweight2003,
      rimweightLIT2003,
      rimweightNUM2003,
      rimweightICT2003,
      rimweightNUMICT2003,
      rimweightLITICT2003,
      rimweightLITNUM2003) |>

    # remove class
    dplyr::mutate(
      WORKINGSTATUS2 = unclass(WORKINGSTATUS2),
      GROSS_ANNUAL_INCOME_OLDBANDS = unclass(GROSS_ANNUAL_INCOME_OLDBANDS),
      BUK = unclass(BUK),
      QxTenu1 = unclass(QxTenu1),
      Sex1 = unclass(Sex1),
      AGE1NET = unclass(AGE1NET),
      Sesol = unclass(Sesol),
      ETHNICSIMPLE = unclass(ETHNICSIMPLE),
      HIQUAL = unclass(HIQUAL),
      IMDSCOREB4 = unclass(IMDSCOREB4),
      NSSEC7 = unclass(NSSEC7),

      LiteracyThresholdA_1 = unclass(LiteracyThresholdA_1),
      NumeracyThresholdA_1 = unclass(NumeracyThresholdA_1),
      MultipleChoiceLevelA_1Thres = unclass(MultipleChoiceLevelA_1Thres),
      LiteracyScoreA_1 = unclass(LiteracyScoreA_1),
      NumeracyScoreA_1 = unclass(NumeracyScoreA_1)) |>

    # relabel and order levels
    dplyr::transmute(
      workingstatus = factor(WORKINGSTATUS2,
                             levels = c(0,1), labels = c("No", "Yes")),
      gross_income =
        ifelse(GROSS_ANNUAL_INCOME_OLDBANDS %in% 1:2,
               "<10000",
               ifelse(GROSS_ANNUAL_INCOME_OLDBANDS %in% 3:6,
                      ">=10000", "other")) |>
        factor(levels = c("<10000", ">=10000", "other")),

      uk_born = factor(BUK, levels = c(2,1), labels = c("No", "Yes")),

      sex = factor(Sex1, levels = c(2,1), c("Female", "Male")),

      own_home = ifelse(QxTenu1 == 1, "Yes", "No") |>   # assume shared ownership is not own home
        factor(levels = c("No", "Yes")),

      age = ifelse(AGE1NET %in% 1:2, "16-44",
                   ifelse(AGE1NET == 3, ">=45", NA)) |>
        factor(levels = c("16-44", ">=45")),            # other entries as NA

      english_lang = factor(Sesol, levels = c(2,1), labels = c("No", "Yes")),

      ethnicity = factor(ETHNICSIMPLE, levels = c(1,2),
                         labels = c("White", "BME")),

      qualification = ifelse(HIQUAL %in% 1:4, ">=level 2", "<=Level 1") |>
        factor(levels = c("<=Level 1", ">=level 2")),

      # imd = factor(10 - IMDSCOREB4),     # original greater deprivation being higher number
      imd = case_when(
        IMDSCOREB4 %in% c(1, 2) ~ 5,
        IMDSCOREB4 %in% c(3, 4) ~ 4,
        IMDSCOREB4 %in% c(5, 6) ~ 3,
        IMDSCOREB4 %in% c(7, 8) ~ 2,
        IMDSCOREB4 == 9         ~ 1),

      # National Statistics Socio-economic Classification (NS-SEC)
      job_status = ifelse(NSSEC7 %in% 1:2, "higher",  # managerial
                          ifelse(NSSEC7 == 3:4, "intermediate",
                                 ifelse(NSSEC7 %in% 5:7, "lower", "other"))) |>
        factor(levels = c("lower", "intermediate", "higher", "other")),


      # --- outcomes ---

      lit_thresholdL1 =
        ifelse(LiteracyThresholdA_1 == 1, "below",  # EL and below
               ifelse(LiteracyThresholdA_1 == 2, "above", "other")),   # >= L1

      lit_thresholdL2 = ifelse(LiteracyScoreA_1 == 5, "above",  # >= L2
                               ifelse(LiteracyScoreA_1 %in% 1:4, "below", "other")),  # L1 and below
      num_thresholdEL3 =
        ifelse(NumeracyThresholdA_1 == 1, "below",
               ifelse(NumeracyThresholdA_1 == 2, "above", "other")),
      num_thresholdL1 = ifelse(NumeracyScoreA_1 == 4:5, "above",
                               ifelse(NumeracyScoreA_1 %in% 1:3, "below", "other")),  # >= L1
      ict_thresholdEL3 =
        ifelse(MultipleChoiceLevelA_1Thres == 1, "below",
               ifelse(MultipleChoiceLevelA_1Thres == 2, "above", "other")),

      # weights
      weights = unclass(rimweight2003),
      lit_weightsL1 = unclass(rimweightLIT2003),
      num_weightsEL3 = unclass(rimweightNUM2003),
      ict_weightsEL3 = unclass(rimweightICT2003)) |>

    # remove missing
    dplyr::filter(!is.na(age),
                  !is.na(ethnicity))

  # health literacy assessment specific data sets
  # filtered by have answered question

  lit <- model_dat |>
    dplyr::filter(lit_thresholdL2 %in% c("above", "below")) |>
    mutate(lit_thresholdL2 = factor(lit_thresholdL2, levels = c("above", "below")),
           lit_thresholdL2_bin = as.integer(lit_thresholdL2) - 1L,
           lit_thresholdL1 = factor(lit_thresholdL1, levels = c("above", "below")),
           lit_thresholdL1_bin = as.integer(lit_thresholdL1) - 1L,
           weights = lit_weightsL1) |>
    select(-lit_weightsL1, -num_weightsEL3, -ict_weightsEL3,
           -num_thresholdEL3, -num_thresholdL1, -ict_thresholdEL3)

  num <- model_dat |>
    dplyr::filter(num_thresholdL1 %in% c("above", "below")) |>
    mutate(num_thresholdL1 = factor(num_thresholdL1, levels = c("above", "below")),
           num_thresholdL1_bin = as.integer(num_thresholdL1) - 1L,
           weights = num_weightsEL3) |>
    select(-lit_weightsL1, -num_weightsEL3, -ict_weightsEL3,
           -num_thresholdEL3, -lit_thresholdL2, -ict_thresholdEL3)

  ict <- model_dat |>
    dplyr::filter(ict_thresholdEL3 %in% c("above", "below")) |>
    mutate(ict_thresholdEL3 = factor(ict_thresholdEL3, levels = c("above", "below")),
           ict_thresholdEL3_bin = as.integer(ict_thresholdEL3) - 1L,
           weights = ict_weightsEL3) |>
    select(-lit_weightsL1, -num_weightsEL3, -ict_weightsEL3,
           -num_thresholdEL3, -num_thresholdL1, -lit_thresholdL2)

  tibble::lst(lit, num, ict)
}

#' @title Fit health literacy, numeracy and ICT regression models
#'
#' @param survey_data List of data frames containing the cleaned survey data
#' @param stan Logical indicating whether to use Stan or not
#' @param save Logical indicating whether to save the fitted models
#' @param ... Additional arguments to pass to the Stan model
#' @return List of fitted models
#'
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom rstanarm stan_glm
#' @importFrom tibble lst
#' @seealso [clean_data()]
#'
fit_models <- function(survey_data, stan = TRUE, save = FALSE, file_suffix = "", ...) {

  lit_dat <- survey_data$lit
  num_dat <- survey_data$num
  ict_dat <- survey_data$ict

  model_type <- if (stan) "stan" else "freq"

  # --- construct formula object

  # # all FE
  # if (file_suffix == "2003") {
  #   # remove uk_born which is not available in 2003
  #   fe_names <- c("sex", "age", "ethnicity", "english_lang", "qualification",
  #                 "workingstatus", "job_status", "gross_income", "own_home", "imd")
  # } else {
  #   fe_names <- c("sex", "age", "ethnicity", "uk_born", "english_lang", "qualification",
  #                 "workingstatus", "job_status", "gross_income", "own_home", "imd")
  # }
  #
  # re_names <- NULL

  # with RE for IMD
  if (file_suffix == "2003") {
    # remove uk_born which is not available in 2003
    fe_names <- c("sex", "age", "ethnicity", "english_lang", "qualification",
                  "workingstatus", "job_status", "gross_income", "own_home")
  } else {
    fe_names <- c("sex", "age", "ethnicity", "uk_born", "english_lang", "qualification",
                  "workingstatus", "job_status", "gross_income", "own_home")
  }

  re_names <- "imd"

  fe_form <- paste(fe_names, collapse = " + ")

  # fixed effect only?
  has_re <- length(re_names) > 0

  if (has_re) {
    re_form <- paste0("(1|", re_names, ")", collapse = " + ")
    re_form <- paste("+", re_form)
  } else {
    re_form <- NULL
  }

  rhs <- paste("1 +", fe_form, re_form)

  if (!stan) {
    if (has_re) {
      lit <- lme4::glmer(glue("lit_thresholdL2_bin ~ {rhs}"),
                         data = lit_dat, family = binomial(),
                         weights = weights, ...)

      num <- lme4::glmer(glue("num_thresholdL1_bin ~ {rhs}"),
                         data = num_dat, family = binomial(),
                         weights = weights, ...)

      ict <- lme4::glmer(glue("ict_thresholdEL3_bin ~ {rhs}"),
                         data = ict_dat, family = binomial(),
                         weights = weights, ...)
    } else {
      lit <- stats::glm(glue("lit_thresholdL2_bin ~ {rhs}"),
                        data = lit_dat, family = binomial(),
                        weights = weights, ...)

      num <- stats::glm(glue("num_thresholdL1_bin ~ {rhs}"),
                        data = num_dat, family = binomial(),
                        weights = weights, ...)

      ict <- stats::glm(glue("ict_thresholdEL3_bin ~ {rhs}"),
                        data = ict_dat, family = binomial(),
                        weights = weights, ...)
    }
  } else {

    if (has_re) {
      lit <- rstanarm::stan_glmer(
        glue("lit_thresholdL2_bin ~ {rhs}"),
        data = lit_dat,
        family = binomial(),
        weights = weights,
        chains = 2, iter = 2000, ...)

      num <- rstanarm::stan_glmer(
        glue("num_thresholdL1_bin ~ {rhs}"),
        data = num_dat,
        family = binomial(),
        weights = weights,
        chains = 2, iter = 2000, ...)

      ict <- rstanarm::stan_glmer(
        glue("ict_thresholdEL3_bin ~ {rhs}"),
        data = ict_dat,
        family = binomial(),
        weights = weights,
        chains = 2, iter = 2000, ...)
    } else {
      lit <- rstanarm::stan_glm(
        glue("lit_thresholdL2_bin ~ {rhs}"),
        data = lit_dat,
        family = binomial(),
        weights = weights,
        chains = 2, iter = 2000, ...)

      num <- rstanarm::stan_glm(
        glue("num_thresholdL1_bin ~ {rhs}"),
        data = num_dat,
        family = binomial(),
        weights = weights,
        chains = 2, iter = 2000, ...)

      ict <- rstanarm::stan_glm(
        glue("ict_thresholdEL3_bin ~ {rhs}"),
        data = ict_dat,
        family = binomial(),
        weights = weights,
        chains = 2, iter = 2000, ...)
    }
  }

  if (save) {

    if (nchar(file_suffix) > 0) {
      file_suffix <- paste0("_", file_suffix)
    }

    save(lit, num, ict, file = here::here(glue::glue("data/{model_type}{file_suffix}_fits.RData")))
  }

  tibble::lst(lit, num, ict)
}

#' @title Post-stratification
#' @importFrom rstanarm posterior_epred
#' @importFrom dplyr summarize
#'
poststratification <- function(fit, data) {

  is_stan <- inherits(fit, "stanreg")

  if (is_stan) {
    posterior_draws <- rstanarm::posterior_epred(fit, newdata = data)
    poststrat_est <- posterior_draws %*% data$product_p
  } else {
    data$predicted_prob <- predict(fit, data, type = 'response')
    poststrat_est <-
      data |>
      dplyr::summarize(estimate = weighted.mean(predicted_prob, product_p))
  }

  poststrat_est
}
