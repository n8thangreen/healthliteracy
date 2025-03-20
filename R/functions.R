
#' @title Clean survey data
#'
#' @param data Survey data at individual level i.e. Skills for Life data
#' @param save Logical indicating whether to save the cleaned data sets
#' @return List of cleaned data sets
#' @import dplyr
#'
clean_data <- function(data, save = FALSE) {

  # select variables
  model_dat <-
    data |>
    dplyr::select(
      WORKINGSTATUS2,
      GROSS_ANNUAL_INCOME_OLDBANDS,
      BUK,
      QxTenu1,
      Sex1,
      AGE1NET,
      Sesol,              # is English first language
      ETHNICSIMPLE,
      HIQUAL,
      CLITSPEAK,          # ENFL everyday English skills (literacy and speaking)
      IMDSCOREB4,         # Index of Multiple Deprivation banded into deciles
      NSSEC7,
      # outcomes
      SUMMARYCOMP,        # self-assessed computer skills (summary)
      TSKILLA,            # self-assessed computer skills (summary 2)
      COMBLIT,            # self-assessed reading a writing (summary)
      LiteracyScoreA_1,                  # literacy level
      starts_with("LiteracyThreshold"),  # literacy threshold
      NumeracyScoreA_1,                  # numeracy level
      starts_with("NumeracyThreshold"),  # numeracy threshold,
      MultipleChoiceLevelA_1,            # ICT level
      MultipleChoiceLevelA_1Thres,       # ICT threshold
      # weights
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
      workingstatus = factor(WORKINGSTATUS2, levels = c(0,1), labels = c("No", "Yes")),
      gross_income =
        ifelse(GROSS_ANNUAL_INCOME_OLDBANDS %in% 1:2,
               "<10000",
               ifelse(GROSS_ANNUAL_INCOME_OLDBANDS %in% 3:6,
                      ">=10000", "other")) |>
        factor(levels = c("<10000", ">=10000", "other")),
      uk_born = factor(BUK, levels = c(2,1), labels = c("No", "Yes")),
      sex = factor(Sex1, levels = c(2,1), c("Female", "Male")),
      own_home = ifelse(QxTenu1 == 1, "Yes", "No") |>
        factor(levels = c("No", "Yes")),
      age = ifelse(AGE1NET %in% 1:2, "16-44",
                   ifelse(AGE1NET == 3, ">=45", "other")) |>
        factor(levels = c("16-44", ">=45")),
      english_lang = factor(Sesol, levels = c(2,1), labels = c("No", "Yes")),
      ethnicity = factor(ETHNICSIMPLE, levels = c(1,2), labels = c("White", "BME")),
      qualification = ifelse(HIQUAL %in% 1:4, ">=level 2", "<=Level 1") |>
        factor(levels = c("<=Level 1", ">=level 2")),
      imd = factor(10 - IMDSCOREB4),           # for some reason (?) these are the wrong way round. why?...
      job_status = ifelse(NSSEC7 %in% 1:2, "higher",
                          ifelse(NSSEC7 == 3, "intermediate",
                                 ifelse(NSSEC7 %in% 4:10, "lower", "other"))) |>
        factor(levels = c("lower", "intermediate", "higher")),
      lit_thresholdL1 =
        ifelse(LiteracyThresholdA_1 == 1, "below",
               ifelse(LiteracyThresholdA_1 == 2, "above", "other")),
      lit_thresholdL2 = ifelse(LiteracyScoreA_1 == 5, "above",
                               ifelse(LiteracyScoreA_1 %in% 1:4, "below", "other")),   # >= L2
      num_thresholdEL3 =
        ifelse(NumeracyThresholdA_1 == 1, "below",
               ifelse(NumeracyThresholdA_1 == 2, "above", "other")),
      num_thresholdL1 = ifelse(NumeracyScoreA_1 == 4:5, "above",
                               ifelse(NumeracyScoreA_1 %in% 1:3, "below", "other")),  # >= L1
      ict_thresholdEL3 =
        ifelse(MultipleChoiceLevelA_1Thres == 1, "below",
               ifelse(MultipleChoiceLevelA_1Thres == 2, "above", "other")),
      weights = unclass(rimweight2003),
      lit_weightsL1 = unclass(rimweightLIT2003),
      num_weightsEL3 = unclass(rimweightNUM2003),
      ict_weightsEL3 = unclass(rimweightICT2003)) |>
    dplyr::filter(!is.na(age),
                  !is.na(ethnicity))

  # health literacy assessment specific data sets
  # where have answered question

  lit <- model_dat |>
    filter(lit_thresholdL2 %in% c("above", "below")) |>
    mutate(lit_thresholdL2 = as.factor(lit_thresholdL2),
           lit_thresholdL2_bin = as.integer(lit_thresholdL2) - 1L) |>
    select(-lit_weightsL1, -num_weightsEL3, -ict_weightsEL3,
           -num_thresholdEL3, -num_thresholdL1, -ict_thresholdEL3)

  num <- model_dat |>
    filter(num_thresholdL1 %in% c("above", "below")) |>
    mutate(num_thresholdL1 = as.factor(num_thresholdL1),
           num_thresholdL1_bin = as.integer(num_thresholdL1) - 1L) |>
    select(-lit_weightsL1, -num_weightsEL3, -ict_weightsEL3,
           -num_thresholdEL3, -lit_thresholdL2, -ict_thresholdEL3)

  ict <- model_dat |>
    filter(ict_thresholdEL3 %in% c("above", "below")) |>
    mutate(ict_thresholdEL3 = as.factor(ict_thresholdEL3),
           ict_thresholdEL3_bin = as.integer(ict_thresholdEL3) - 1L) |>
    select(-lit_weightsL1, -num_weightsEL3, -ict_weightsEL3,
           -num_thresholdEL3, -num_thresholdL1, -lit_thresholdL2)

  tibble::lst(lit, num, ict)
}

#' @title Fit health literacy, numeracy and ICT models
#'
#' @param survey_data List of data frames containing the cleaned survey data
#' @param stan Logical indicating whether to use Stan or not
#' @param save Logical indicating whether to save the fitted models
#' @param ... Additional arguments to pass to the Stan model
#' @return List of fitted models
#' @import glue
#'
fit_models <- function(survey_data, stan = TRUE, save = FALSE, ...) {

  lit_dat <- survey_data$lit
  num_dat <- survey_data$num
  ict_dat <- survey_data$ict

  model <- if (stan) "stan" else "freq"

  rhs <- "1 + sex + age + ethnicity + uk_born + english_lang + qualification + workingstatus + job_status + gross_income + own_home + imd"

  if (!stan) {
    lit <- glm(glue("lit_thresholdL2_bin ~ {rhs}"), data = lit_dat, family = binomial(), weights = weights, ...)
    num <- glm(glue("num_thresholdL1_bin ~ {rhs}"), data = num_dat, family = binomial(), weights = weights, ...)
    ict <- glm(glue("ict_thresholdEL3_bin ~ {rhs}"), data = ict_dat, family = binomial(), weights = weights, ...)
  } else {
    lit <- rstanarm::stan_glm(glue("lit_thresholdL2_bin ~ {rhs}"), data = lit_dat, family = binomial(),
                              weights = weights, chains = 2, iter = 2000, ...)
    num <- rstanarm::stan_glm(glue("num_thresholdL1_bin ~ {rhs}"), data = num_dat, family = binomial(),
                              weights = weights, chains = 2, iter = 2000, ...)
    ict <- rstanarm::stan_glm(glue("ict_thresholdEL3_bin ~ {rhs}"), data = ict_dat, family = binomial(),
                              weights = weights, chains = 2, iter = 2000, ...)
  }

  if (save) {
    save(lit, num, ict, file = here::here(glue::glue("data/{model}_fits.RData")))
  }

  tibble::lst(lit, num, ict)
}

#' @title Post-stratification
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
