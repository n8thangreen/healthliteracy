
#' @title Create covariate data
#' Skills for Life survey
#'
#' @param survey_dat Survey data for a particular health literacy outcome
#' @return grid of all combinations of values
#'
create_covariate_data <- function(survey_dat) {

  unique_workingstatus <- unique(survey_dat$workingstatus)
  unique_gross_income <- unique(survey_dat$gross_income)
  unique_uk_born <- unique(survey_dat$uk_born)
  unique_sex <- unique(survey_dat$sex)
  unique_own_home <- unique(survey_dat$own_home)
  unique_age <- unique(survey_dat$age)
  unique_english_lang <- unique(survey_dat$english_lang)
  unique_ethnicity <- unique(survey_dat$ethnicity)
  unique_qualification <- unique(survey_dat$qualification)
  unique_imd <- unique(survey_dat$imd)
  unique_job_status <- unique(survey_dat$job_status)

  # generate all combinations
  res <- expand.grid(
    workingstatus = unique_workingstatus,
    gross_income = unique_gross_income,
    uk_born = unique_uk_born,
    sex = unique_sex,
    own_home = unique_own_home,
    age = unique_age,
    english_lang = unique_english_lang,
    ethnicity = unique_ethnicity,
    qualification = unique_qualification,
    imd = unique_imd,
    job_status = unique_job_status) |>
    as_tibble()

  names_vars <- names(res)

  res
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
#' Use individual level Resident survey data
#' so can estimate full joint distribution
#'
#' @param additional_prob_data non-NRS data
#' @param save logical
#' @param covariate_data Covariate data fro SfL
#'
#' @return dataframe of levels and joint probability
#'
create_target_pop_data <- function(covariate_data,
                                   additional_prob_data = NULL,
                                   save = FALSE) {

  LSOA_IMD_data <-
    read.csv(here::here("../../data/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv"))

  imd_lookup <-
    LSOA_IMD_data |>
    filter(`Local.Authority.District.name..2019.` == "Newham") |>
    rename(imd = Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.) |>
    group_by(imd) |>
    summarize(pop = sum(Total.population..mid.2015..excluding.prisoners.)) |>
    mutate(p_imd = pop / sum(pop))

  nrs_prob_data <- create_NRS_prob_data()

  # when its a single dataframe
  if (inherits(additional_prob_data, "list")) {
    additional_prob_data <- list(additional_prob_data)
  }

  res <-
    covariate_data |>
    merge(nrs_prob_data)

  res <-
    additional_prob_data |>
    reduce(left_join, .init = res) |>
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

#' Create Newhan Resident Survey probability data
#'
#' from resident survey individual level data
#' joint distribution
#'
create_NRS_prob_data <- function() {

  file_loc <- here::here("../../data/Newham Resident Survey 2023/London Borough of Newham - Residents Survey - 2023 - Dataset v3.xlsx")

  resident_survey <- readxl::read_xlsx(file_loc, sheet = "Labels")
  variable_labels <- readxl::read_xlsx(file_loc, sheet = "Variable Labels")

  # select columns
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

  resident_marginals <- res_dat |>
    tidyr::pivot_longer(cols = -Weight, names_to = "variable", values_to = "value") |>
    filter(!is.na(value)) |>
    group_by(variable, value) |>
    summarise(
      weighted_count = sum(Weight), .groups = "drop") |>
    mutate(
      weighted_proportion = weighted_count / sum(res_dat$Weight, na.rm = TRUE)
    ) |>
    arrange(variable, desc(weighted_proportion))

  # grid of joint probabilities
  resident_joint <- res_dat |>
    filter(if_all(everything(), ~ !is.na(.))) |>
    group_by(age, sex, ethnicity, workingstatus, own_home) |>
    summarise(
      weighted_count = sum(Weight), .groups = "drop") |>
    mutate(
      p_age_sex_eth_work_home = weighted_count / sum(weighted_count)
    ) |>
    arrange(desc(p_age_sex_eth_work_home))

  resident_joint
}
