
#' Marginal proportion tables
#'
#' data from resident survey report summary tables (weighted)
#' unless otherwise indicated
#' @export
proportion_tables <- function()
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
                       "No", 0.65),

    # ONS census 2021 Highest level of qualification
    qualification = tribble(~qualification, ~p_qual,
                            ">=level 2", 0.57,
                            "<=Level 1", 0.43),

    # Q61: household gross income before tax
    # Q70: Are you the main or joint householder? e.g.responsible for bills such as rent, mortgage and utilities
    # this would be good but its mostly 'not answered'!
    # Q54	What is your average monthly pay?
    #
    ##TODO: break down by LSOA and map to CNA
    ##  read from Newham tab in saiefy1920finalqaddownload280923.xlsx
    gross_income = tribble(~gross_income, ~p_income,
                           ">=10000", 0.9,
                           "<10000", 0.1),

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
    job_status = tribble(~job_status, ~p_job,
                         "higher", 0.167,
                         "intermediate", 0.276,
                         "lower", 0.557) # 0.234 + 0.323
  )
