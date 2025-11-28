
#' Proportion marginal tables from resident survey
#' report summary tables (weighted)
#' @export
res_survey_prop_tables <- function() {
  list(
    age = tribble(~age, ~p_age,
                  "16-44", 0.64, # 0.15 + 0.27 + 0.22
                  ">=45", 0.36), # 0.15 + 0.11 + 0.1

    sex = tribble(~sex, ~p_sex,
                  "Male", 0.54,
                  "Female", 0.46),

    ethnicity = tribble(~ethnicity, ~p_ethn,
                        "White", 0.30,
                        "BME", 0.70),

    workingstatus = tribble(~workingstatus, ~p_workstatus,
                            "Yes", 0.65,
                            "No", 0.35),

    own_home = tribble(~own_home, ~p_own_home,
                       "Yes", 0.35,
                       "No", 0.65)
  )
}


#' Proportion marginal tables other sources
#' from Census
#'
#' @export
get_newham_census_props <- function(equivalise_income = FALSE) {
  list(
    # ONS census 2021 Highest level of qualification
    qualification = tribble(~qualification, ~p_qual,
                            ">=Level 2", 0.57,
                            "<=Level 1", 0.43),

    # Q61: household gross income before tax
    # Q70: Are you the main or joint householder? e.g.responsible for bills such as rent, mortgage and utilities
    # this would be good but its mostly 'not answered'!
    # Q54	What is your average monthly pay?

    gross_income = if (equivalise_income) {
      # from equivalised income
      # resident_survey <- create_equivalised_income(resident_survey)  # equivalise_income.R
      tribble(~gross_income, ~p_income,
              ">=10000", 0.67,
              "<10000", 0.33)
              # "other", 0)
    } else {
      ##TODO: break down by LSOA and map to CNA
      ##  read from Newham tab in saiefy1920finalqaddownload280923.xlsx
      tribble(~gross_income, ~p_income,
              ">=10000", 0.9,
              "<10000", 0.1)
              # "other", 0)
    },

    # census 2021 usual resident population
    uk_born = tribble(~uk_born, ~p_uk,
                      "Yes", 0.463, # 0.455 + 0.001 + 0.004 + 0.003
                      "No", 0.553),

    # Q77	How well can you speak English?
    # 1	Very well
    # 2	Well
    # 3	Not well
    #
    # ONS census 2021 English as main language
    english_lang = tribble(~english_lang, ~p_english,
                           "Yes", 0.6537,
                           "No", 0.3463),

    # L1, L2 and L3 Higher managerial, administrative and professional occupations	29,613	10.7
    # L4, L5 and L6 Lower managerial, administrative and professional occupations	42,336	15.3
    # L7 Intermediate occupations	23,023	8.3
    # L8 and L9 Small employers and own account workers	33,619	12.2
    # L10 and L11 Lower supervisory and technical occupations	12,009	4.3
    # L12 Semi-routine occupations	28,898	10.5
    # L13 Routine occupations	29,868	10.8
    # L14.1 and L14.2 Never worked and long-term unemployed	41,839	15.1
    # L15 Full-time students	34,967	12.7

    job_status = tribble(~job_status, ~p_job,
                         "higher", 0.107 + 0.153,         # L1 to L6
                         "intermediate", 0.083 + 0.122,   # L7, L8, L9
                         "lower", 0.043 + 0.105 + 0.108,  # L10 to L13
                         "other", 0.151 + 0.127           # L14.1, L14.2, L15
    )
  )
}

#' Proportion marginal tables other sources
#' from 2021 Census (see nomis)
#'
#' @export
get_england_census_props <- function() {
  list(
    qualification = tribble(~qualification, ~p_qual,
                            ">=Level 2", 0.133 + 0.169 + 0.339,
                            "<=Level 1", 0.181 + 0.097 + 0.028 + 0.053),

    # Annual Survey of Hours and Earnings (ASHE)
    # Publisher: Office for National Statistics (ONS)
    # Latest Release: Employee earnings in the UK: 2024 (Released October 2024)
    # assume "Earnings" only (wages from a job)
    gross_income = tribble(~gross_income, ~p_income,
                           ">=10000", 0.56,
                           "<10000", 0.44),
                           # "other", 0),

    # census 2021 usual resident population
    uk_born = tribble(~uk_born, ~p_uk,
                      "Yes", 0.826,
                      "No", 0.174),

    # ONS census 2021 English as main language
    english_lang = tribble(~english_lang, ~p_english,
                           "Yes", 0.911,
                           "No", 0.089),

    # L1, L2 and L3 Higher managerial, administrative and professional occupations
    # L4, L5 and L6 Lower managerial, administrative and professional occupations
    # L7 Intermediate occupations
    # L8 and L9 Small employers and own account workers
    # L10 and L11 Lower supervisory and technical occupations
    # L12 Semi-routine occupations
    # L13 Routine occupations
    # L14.1 and L14.2 Never worked and long-term unemployed
    # L15 Full-time students

    job_status = tribble(~job_status, ~p_job,
                         "higher", 0.132 + 0.199,         # L1 to L6
                         "intermediate", 0.114 + 0.106,   # L7, L8, L9
                         "lower", 0.053 + 0.113 + 0.12,   # L10 to L13
                         "other", 0.085 + 0.077           # L14.1, L14.2, L15
    ),

    workingstatus = tribble(~workingstatus, ~p_workingstatus,
                            "No", 0.443,
                            "Yes", 0.557),

    sex = tribble(~sex, ~p_sex,
                  "Male", 0.49,
                  "Female", 0.51),

    age = tribble(~age, ~p_age,
                  "16-44", 0.457,
                  ">=45", 0.543),

    ethnicity = tribble(~ethnicity, ~p_ethnicity,
                        "White", 0.81,
                        "BME", 0.19),

    # tenure
    own_home = tribble(~own_home, ~p_own_home,
                       "Yes", 0.325 + 0.298,
                       "No", 1 - 0.623),
    # by definition
    imd = tribble(~imd, ~p_imd,
                  "1", 0.2,
                  "2", 0.2,
                  "3", 0.2,
                  "4", 0.2,
                  "5", 0.2)
  )
}

