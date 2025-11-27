
#
get_national_marginals <- function() {

  lfs_data <- clean_lfs_data()

  # Define the variables need marginals for
  # names must match what create_synth_data() expects

  make_prop_table <- function(data, var, prob_col_name) {
    data |>
      count(.data[[var]]) |>
      mutate(!!prob_col_name := n / sum(n)) |>
      select(.data[[var]], !!prob_col_name)
  }

  target_marginals <- list(
    qualification = make_prop_table(lfs_data, "qualification", "p_qual"),
    gross_income  = make_prop_table(lfs_data, "gross_income", "p_income"),
    uk_born       = make_prop_table(lfs_data, "uk_born", "p_uk"),
    english_lang  = make_prop_table(lfs_data, "english_lang", "p_english"),
    job_status    = make_prop_table(lfs_data, "job_status", "p_job"),
    sex    = make_prop_table(lfs_data, "sex", "p_sex"),
    age    = make_prop_table(lfs_data, "age", "p_age"),
    ethnicity    = make_prop_table(lfs_data, "ethnicity", "p_ethnicity"),
    workingstatus    = make_prop_table(lfs_data, "workingstatus", "p_workingstatus"),
    own_home    = make_prop_table(lfs_data, "own_home", "p_own_home"),
    imd    = make_prop_table(lfs_data, "imd", "p_imd")
  )

  return(target_marginals)
}

#
get_national_joint <- function() {

  lfs_data <- clean_lfs_data()

  all_vars <- c("qualification", "gross_income", "uk_born", "english_lang",
                "job_status", "sex", "age", "ethnicity",
                "workingstatus", "own_home", "imd")

  joint_dist <- lfs_data |>
    # Count unique combinations of all variables at once
    count(across(all_of(all_vars))) |>
    mutate(p_joint = n / sum(n)) |>
    select(-n)

  return(joint_dist)
}
