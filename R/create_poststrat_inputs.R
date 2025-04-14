
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

#' @title Create target population data
#' Marginal Resident Survey data taken from tables in report
#'
#' @param covariate_data Covariate data
#' @return dataframe of levels and joint probability
#'
create_target_marginal_pop_data <- function(covariate_data) {

  # IMD is at LSOA level
  ward_lookup <- read.csv(here::here("raw_data/Ward - neighbourhood - quadrant 2024.csv"))

  imd_dat <- read.csv(here::here("raw_data/localincomedeprivationdata_Newham.csv")) |>
    rename(LSOA11CD = "LSOA.code..2011.",
           imd = "Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.",
           pop = "Total.population..mid.2015..excluding.prisoners.") |>
    select(LSOA11CD, imd, pop)

  LSOA_lookup <-
    read.csv(here::here("raw_data/Lower_Layer_Super_Output_Area_(2011)_to_Ward_(2015)_Lookup_in_England_and_Wales.csv")) |>
    filter(LAD15NM == "Newham")

  imd_lookup <-
    LSOA_lookup |>
    merge(ward_lookup, by.x = "WD15NM", by.y = "Ward") |>
    merge(imd_dat) |>
    group_by(imd) |>
    summarize(pop = sum(pop)) |>
    mutate(p_imd = pop / sum(pop)) |>
    select(-pop)

  # from resident survey report summary tables (weighted)
  # unless otherwise indicated

  res <-
    covariate_data |>
    merge(
      tribble(~age, ~p_age,
              "16-44", 0.15 + 0.27 + 0.22,
              ">=45", 0.15 + 0.11 + 0.1)) |>
    merge(
      tribble(~sex, ~p_sex,
              "Male", 0.54,
              "Female", 0.46)) |>
    merge(
      tribble(~ethnicity, ~p_ethn,
              "White", 0.30,
              "BME", 0.70)) |>
    merge(
      tribble(~workingstatus, ~p_workstatus,
              "Yes", 0.65,
              "No", 0.35)) |>
    merge(
      tribble(~own_home, ~p_own_home,
              "Yes", 0.35,
              "No", 0.65)) |>
    # ONS census 2021 Highest level of qualification
    merge(
      tribble(~qualification, ~p_qual,
              ">=level 2", 0.57,
              "<=Level 1", 0.43)) |>
    # Q61: household gross income before tax
    # Q70: Are you the main or joint householder? e.g.responsible for bills such as rent, mortgage and utilities
    # this would be good but its mostly 'not answered'!
    # Q54	What is your average monthly pay?
    #
    ##TODO: break down by LSOA and map to CNA
    ##  read from Newham tab in saiefy1920finalqaddownload280923.xlsx
    merge(
      tribble(~gross_income, ~p_income,
              ">=10000", 0.9,
              "<10000", 0.1)) |>
    # census 2021 usual resident population
    merge(
      tribble(~uk_born, ~p_uk,
              "Yes", 0.455 + 0.001 + 0.004 + 0.003,
              "No", 0.553)) |>
    # Q77	How well can you speak English?
    # 1	Very well
    # 2	Well
    # 3	Not well
    #
    # ONS census 2021 English as main language
    merge(
      tribble(~english_lang, ~p_english,
              "Yes", 0.6537,
              "No", 0.3463)) |>
    # AB: higher and intermediate managerial, administrative and professional occupations
    # C1: supervisory, clerical and junior managerial, administrative and professional occupations
    # C2: skilled manual occupations
    # DE: semi-skilled and unskilled manual and lowest grade occupations
    #
    # tribble(~job_status_ASG, ~job_status, ~prop,
    #         "AB", "higher", 0.167,
    #         "C1", "intermediate", 0.276,
    #         "C2", "lower", 0.234,
    #         "DE", "lower", 0.323)
    merge(
      tribble(~job_status, ~p_job,
              "higher", 0.167,
              "intermediate", 0.276,
              "lower", 0.234 + 0.323)) |>
    merge(imd_lookup) |>
    #####################
  # calculate product of probabilities, assuming independence
  rowwise() |>
    mutate(product_p = prod(c_across(starts_with("p_")))) |>
    ungroup()

  # write.csv(res, here::here("data/total_dat.csv"))

  res
}


#' @title Create target population data
#' Use individual level Resident survey data
#' So can estimate full joint distribution
#'
#' @param covariate_data Covariate data
#' @return dataframe of levels and joint probability
#'
create_target_pop_data <- function(covariate_data) {

  # IMD is at LSOA level
  ward_lookup <- read.csv(here::here("raw_data/Ward - neighbourhood - quadrant 2024.csv"))

  imd_dat <- read.csv(here::here("raw_data/localincomedeprivationdata_Newham.csv")) |>
    rename(LSOA11CD = "LSOA.code..2011.",
           imd = "Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.",
           pop = "Total.population..mid.2015..excluding.prisoners.") |>
    select(LSOA11CD, imd, pop)

  LSOA_lookup <-
    read.csv(here::here("raw_data/Lower_Layer_Super_Output_Area_(2011)_to_Ward_(2015)_Lookup_in_England_and_Wales.csv")) |>
    filter(LAD15NM == "Newham")

  imd_lookup <-
    LSOA_lookup |>
    merge(ward_lookup, by.x = "WD15NM", by.y = "Ward") |>
    merge(imd_dat) |>
    group_by(imd) |>
    summarize(pop = sum(pop)) |>
    mutate(p_imd = pop / sum(pop)) |>
    select(-pop)

  # from resident survey individual level data
  # unless otherwise indicated

  file_loc <- "C:/Users/n8tha/Documents/Newham Council Fellowship/data/residents survey 2023/London Borough of Newham - Residents Survey - 2023 - Dataset v3.xlsx"

  resident_survey <- readxl::read_xlsx(file_loc, sheet = "Labels")
  variable_labels <- readxl::read_xlsx(file_loc, sheet = "Variable Labels")

  # select columns
  res_dat <- resident_survey |>
    select(Q73, Q71, Q82, Q47GRP, Q69, Weight) |>
    # transform variables
    mutate(age = ifelse(Q73 %in% c("16-24", "25-34", "35-44"), "16-44", ">=45"),
           sex = ifelse(!Q71 %in% c("Male", "Female"), NA, Q71),
           ethnicity = ifelse(Q82 %in% c("White - British",
                                         "White - Irish",
                                         "White - Any other White background"), "White", "BME"),
           workingstatus = ifelse(Q47GRP %in% c("Employed"), "Yes", "No"),
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

  res <-
    covariate_data |>
    merge(resident_joint)

  res <- res |>
    # ONS census 2021 Highest level of qualification
    merge(
      tribble(~qualification, ~p_qual,
              ">=level 2", 0.57,
              "<=Level 1", 0.43)) |>
    # Q61: household gross income before tax
    # Q70: Are you the main or joint householder? e.g.responsible for bills such as rent, mortgage and utilities
    # Q54	What is your average monthly pay?
    # this would be good but its mostly 'not answered/prefer not to say'!
    #
    ##TODO: break down by LSOA and map to CNA
    ##  read from Newham tab in saiefy1920finalqaddownload280923.xlsx
    merge(
      tribble(~gross_income, ~p_income,
              ">=10000", 0.9,
              "<10000", 0.1)) |>
    # census 2021 usual resident population
    merge(
      tribble(~uk_born, ~p_uk,
              "Yes", 0.455 + 0.001 + 0.004 + 0.003,
              "No", 0.553)) |>
    # Q77	How well can you speak English?
    # 1	Very well
    # 2	Well
    # 3	Not well
    #
    # ONS census 2021 English as main language
    merge(
      tribble(~english_lang, ~p_english,
              "Yes", 0.6537,
              "No", 0.3463)) |>
    # AB: higher and intermediate managerial, administrative and professional occupations
    # C1: supervisory, clerical and junior managerial, administrative and professional occupations
    # C2: skilled manual occupations
    # DE: semi-skilled and unskilled manual and lowest grade occupations
    #
    # tribble(~job_status_ASG, ~job_status, ~prop,
    #         "AB", "higher", 0.167,
    #         "C1", "intermediate", 0.276,
    #         "C2", "lower", 0.234,
    #         "DE", "lower", 0.323)
    merge(
      tribble(~job_status, ~p_job,
              "higher", 0.167,
              "intermediate", 0.276,
              "lower", 0.234 + 0.323)) |>
    merge(imd_lookup) |>
    #####################
  # calculate product of probabilities, assuming independence
  rowwise() |>
    mutate(product_p = prod(c_across(starts_with("p_")))) |>
    ungroup()

  # write.csv(res, here::here("data/total_dat.csv"))

  res
}

