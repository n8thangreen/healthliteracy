
#' @title Create covariate data
#'
#' Select columns and create full factorial design.
#' For Skills for Life survey or PIAAC data
#'
#' @param survey_dat Survey data for a particular health literacy outcome
#' @return grid of all combinations of values
#'
create_covariate_data <- function(survey_dat) {

  if (inherits(survey_dat, "mids")) {
    # extract a multiple imputation complete data set
    data_to_scan <- survey_dat$data
  } else if (inherits(survey_dat, "data.frame")) {
    data_to_scan <- survey_dat
  } else {
    stop("Input 'survey_dat' must be a 'mids' object or a 'data.frame'.")
  }

  # potential all covariates
  all_covariate_names <- c(
    "workingstatus", "gross_income", "uk_born", "sex", "own_home",
    "age", "english_lang", "ethnicity", "qualification", "imd", "job_status"
  )

  # which actually exist in the input data
  existing_covariate_names <-
    intersect(all_covariate_names, names(data_to_scan))

  # get named list of unique values for existing columns
  unique_vals_list <-
    lapply(existing_covariate_names, function(col_name) {

      col_data <- data_to_scan[[col_name]]

      if (is.factor(col_data)) {
        levels(col_data)
      } else if (is.numeric(col_data)) {
        mean(col_data, na.rm = TRUE)
      } else {
        unique(col_data[!is.na(col_data)])
      }
    })

  names(unique_vals_list) <- existing_covariate_names

  # generate all combinations
  res_df <- do.call(expand.grid, unique_vals_list)

  tibble::as_tibble(res_df)
}


#' @title Create target population data without individual level data
#'
#' Marginal Resident Survey data taken from tables in report
#' other sources from ONS
#'
#' @param covariate_data Covariate data
#' @return dataframe of levels and joint probability
#'
create_target_marginal_pop_data <- function(covariate_data, save = FALSE) {

  LSOA_IMD_data <-
    read.csv(here::here("../../data/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv"))

  imd_lookup <-
    LSOA_IMD_data |>
    filter(`Local.Authority.District.name..2019.` == "Newham") |>
    group_by(Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.) |>
    summarize(pop = sum(Total.population..mid.2015..excluding.prisoners.)) |>
    mutate(p_imd = pop / sum(pop))

  res <-
    covariate_data |>
    c(demo_prop_tables(),
      res_survey_prop_tables()) |>
    reduce(left_join, .init = covariate_data) |>
    merge(imd_lookup) |>

    #####################
  # calculate product of probabilities, assuming independence
  rowwise() |>
    mutate(product_p = prod(c_across(starts_with("p_")))) |>
    ungroup()

  if (save) {
    write.csv(res, here::here("data/total_dat.csv"))
  }

  res
}


#' @title Create target population data
#'
#' Use individual level Newham Resident survey data
#' so can estimate full joint distribution
#'
#' @param additional_prob_data non-NRS data
#' @param save logical
#' @param covariate_data Covariate data from SfL
#' @return dataframe of levels and joint probability
#' @importFrom glue glue
#'
create_target_pop_data <- function(covariate_data,
                                   additional_prob_data = NULL,
                                   save = FALSE) {

  imd_lookup <- create_imd_lookup(quintile = TRUE)

  nrs_prob_data <- create_NRS_prob_data()

  # when its a single dataframe
  if (!inherits(additional_prob_data, "list")) {
    additional_prob_data <- list(additional_prob_data)
  }

  res <-
    covariate_data |>
    merge(nrs_prob_data)

  res <-
    additional_prob_data |>
    reduce(left_join, .init = res) |>
    merge(imd_lookup) |>

  # --- calculate product of probabilities, assuming independence
  rowwise() |>
    mutate(product_p = prod(c_across(starts_with("p_")))) |>
    ungroup() |>
    mutate(
      across(
        c(workingstatus, sex, own_home, age, ethnicity, gross_income,
          uk_born, english_lang, qualification, job_status),
        as.factor))

  if (save) {
    write.csv(res, here::here("data/total_dat.csv"))
  }

  res
}

#' Create Newham Resident Survey probability data
#'
#' from resident survey individual level data
#' joint distribution
#'
create_NRS_prob_data <- function(save = TRUE) {

  filename <- "Newham Resident Survey 2023/London Borough of Newham - Residents Survey - 2023 - Dataset v3.xlsx"
  file_loc <- here::here(glue("../../data/{filename}"))

  resident_survey <- readxl::read_xlsx(file_loc, sheet = "Labels")

  # select columns from questions
  res_dat <- resident_survey |>
    select(Q73,     # How old are you? (grouped)
           Q71,     # Are you...? [sex]
           Q82,     # How would you describe your ethnic group?
           Q47GRP,  # Which of these activities best describes what you are doing at present? (Grouped Responses) [work]
           Q69,     # In which of these ways does your household occupy your current accommodation? [ownership]
           Weight) |>
    # transform variables
    mutate(age = ifelse(Q73 %in% c("16-24", "25-34", "35-44"), "16-44", ">=45"),
           sex = ifelse(!Q71 %in% c("Male", "Female"), NA, Q71),
           ethnicity = ifelse(Q82 %in% c("White - British",
                                         "White - Irish",
                                         "White - Any other White background"), "White", "BME"),
           workingstatus = ifelse(Q47GRP %in% c("Employed"),
                                  "Yes", "No"),
           own_home = ifelse(Q69 %in% c("Own outright", "Own with a mortgage or loan", "Shared ownership"),
                             "Yes", "No")
    ) |>
    select(age, sex, ethnicity, workingstatus, own_home, Weight)

  nrs_joint <-
    res_dat |>
    dplyr::count(age, sex, ethnicity, workingstatus, own_home,
                 name = "frequency", wt = Weight) |>
    ungroup() |>
    mutate(p_nrs = round(frequency/sum(frequency), 3))

  if (save) {
    save(nrs_joint, file = here::here("data/nrs_joint.rda"))
  }

  nrs_joint
}

#
create_imd_lookup <- function(quintile = NA, decile = NA) {

  filename <- "File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv"
  LSOA_IMD_data <-
    read.csv(here::here(glue("../../data/{filename}"))) |>
    rename(imd = Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.)

  if (quintile) {
    LSOA_IMD_data <-
      mutate(LSOA_IMD_data,
             imd = case_when(
               imd %in% c(1, 2) ~ 1,
               imd %in% c(3, 4) ~ 2,
               imd %in% c(5, 6) ~ 3,
               imd %in% c(7, 8) ~ 4,
               imd %in% c(9,10) ~ 5))

    # for completeness include even empty deciles
    missing_imd <-
      data.frame(imd = 1:5,
                 pop_default = 10)  # small
  } else if (decile) {
    missing_imd <-
      data.frame(imd = 1:9,
                 pop_default = 10)  # small
  } else {
    stop("missing quantile argument.")
  }

  # population for Newham by decile
  imd_lookup <-
    LSOA_IMD_data |>
    filter(`Local.Authority.District.name..2019.` == "Newham") |>
    group_by(imd) |>
    summarize(pop = sum(Total.population..mid.2015..excluding.prisoners.)) |>
    full_join(missing_imd, by = "imd") |>
    mutate(pop = coalesce(pop, pop_default)) |>
    select(imd, pop) |>
    mutate(p_imd = pop / sum(pop))

  imd_lookup
}

#' @title Post-stratification
#'
#' Perform poststratification on a fitted model (glm, stanreg, or brmsfit)
#'
#' @param fit A fitted model object.
#' @param data The poststratification data frame, which must contain
#'             all covariates and a 'product_p' column with weights.
#' @return A vector of posterior draws of the poststratified estimate (for stanreg/brms)
#'         or a data frame with a single point estimate (for glm).
#'
#' @importFrom rstanarm posterior_epred
#' @importFrom dplyr summarize
#'
poststratification <- function(fit, data, ndraws = NULL) {

  is_stan <- inherits(fit, "stanreg")
  is_brms <- inherits(fit, "brmsfit")

  if (is_stan || is_brms) {

    if (is_stan) {
      posterior_draws <- rstanarm::posterior_epred(fit, newdata = data,
                                                   draws = ndraws)
    } else {
      posterior_draws <- brms::posterior_epred(fit, newdata = data,
                                               ndraws = ndraws)
    }

    poststrat_est <- posterior_draws %*% data$product_p
  } else {

    data$predicted_prob <- predict(fit, data, type = 'response')

    poststrat_est <- data |>
      dplyr::summarize(estimate = weighted.mean(predicted_prob, product_p))
  }

  return(poststrat_est)
}
