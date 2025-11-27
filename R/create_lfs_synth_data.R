
#' Create labour force survey synthetic data
#'
#' Using {simPop} package to perform iterative proportional fitting (IPF).
#'
#' @param target_marginals_props Target marginal proportions as a list.
#' @import dplyr
#' @import simPop
#'
#' @examples
#' # Verification: Check Marginals
#' \dontrun{
#' result_list <-
#'   lapply(vars_to_simulate,
#'          \(x) {
#'            prop_table <- prop.table(table(synthetic_data_final[[x]]))
#'            df_output <- as.data.frame(prop_table, stringsAsFactors = FALSE)
#'
#'            colnames(df_output) <- names(target_marginals_props[[x]])
#'            return(df_output)
#'          }) |>
#'   setNames(vars_to_simulate)
#' }
create_lfs_synth_data <- function(target_marginals_props = NULL) {

  lfs_data <- clean_lfs_data()

  if (is.null(target_marginals_props)) {
    # Default: Newham Analysis (Mix of sources)
    target_marginals_props <- get_newham_census_props()
  }

  N_small_area <- 10000  # number of synthetic individuals

  # Convert proportions to counts for simPop
  # counts are integers and sum exactly to N_small_area
  # Dynamic list creation based on input names
  # matches "p_xxx" column in marginal tables to variable name
  ##TODO fix mismatch
  target_counts_list <- lapply(names(target_marginals_props), function(var) {

    df <- target_marginals_props[[var]]

    # Identify proportion column (assumes it starts with "p_")
    prop_col <- grep("^p_", names(df), value = TRUE)

    counts <- setNames(
      round(df[[prop_col]] * N_small_area),
      df[[var]]
    )
    return(counts)
  })

  names(target_counts_list) <- names(target_marginals_props)

  # adjust for rounding errors to ensure sum to N_small_area
  for (i in seq_along(target_counts_list)) {
    diff <- N_small_area - sum(target_counts_list[[i]])

    if (diff != 0) {
      # Add/subtract the difference from the largest category
      largest_cat_idx <- which.max(target_counts_list[[i]])

      target_counts_list[[i]][largest_cat_idx] <-
        target_counts_list[[i]][largest_cat_idx] + diff
    }
  }

  # --- simPop ---

  # 1. Create a simPop input object
  lfs_data$person_id <- 1:nrow(lfs_data)
  lfs_data$sim_weight <- 1
  lfs_data$strata_col <- factor("default_strata")

  inp <- simPop::specifyInput(
    data = as.data.frame(lfs_data),
    hhid = "person_id",
    # hhsize = NULL,
    # pid = "person_id",
    weight = "sim_weight",
    strata = "strata_col",
  )

  sim_struc <- simPop::simStructure(
    data = inp,
    method = "direct",
    basicHHvars = names(target_counts_list))

  sim_pop_obj <- simPop::simCategorical(
    simPopObj = sim_struc,
    additional = NULL,  #names(target_counts_list), # Variables to simulate
    method = "multinom",
    by = "strata_col"
  )

  # --- calibration

  vars_to_simulate <- names(target_counts_list)

  # list format for calibPop() input
  pers_tables_for_calibPop <-
    lapply(vars_to_simulate, function(var_name) {

      current_levels <- levels(lfs_data[[var_name]])

      ##TODO: replace below with this?
      # # Extract counts from our list
      # counts_vec <- target_counts_list[[var_name]]
      #
      # df <- data.frame(
      #   Var = names(counts_vec),
      #   Freq = as.numeric(counts_vec),
      #   stringsAsFactors = FALSE
      # )
      # # Rename first col to match variable name
      # colnames(df)[1] <- var_name
      #
      # df$strata_col <- factor("default_strata", levels = levels(lfs_data$strata_col))
      # df[[var_name]] <- factor(df[[var_name]], levels = current_levels)


      dummy_strata_level <- levels(lfs_data$strata_col)[1]

      df <- data.frame(
        Category = names(target_counts_list[[var_name]]),
        Freq = as.numeric(target_counts_list[[var_name]]),
        stringsAsFactors = FALSE
      )

      colnames(df)[1] <- var_name

      # add strata_col to each marginal table
      df$strata_col <- dummy_strata_level # All entries get same dummy strata value

      df[[var_name]] <- factor(df[[var_name]],
                               levels = current_levels)

      df[["strata_col"]] <- factor(df[["strata_col"]],
                                   levels = levels(lfs_data$strata_col))

      return(df)
    })

  names(pers_tables_for_calibPop) <- vars_to_simulate

  # Step 3: Calibrate the Simulated Population to External Marginals
  final_sim_pop_obj <- calibPop(
    inp = sim_pop_obj,
    persTables = pers_tables_for_calibPop,
    split = "strata_col",
    # verbose = TRUE,
    maxiter = 200
  )

  synth_data <- simPop::popData(final_sim_pop_obj)

  # frequencies per category
  synth_data <-
    synth_data |>
    as_tibble() |>
    dplyr::count(!!!syms(vars_to_simulate), name = "frequency") |>
    ungroup() |>
    mutate(p_synth = round(frequency/sum(frequency), 3)) |>
    select(-frequency)

  # fill in missing categories with 0
  load(here::here("data/skills_for_life_2011_data.RData"))
  survey_data <- clean_sfl_data_2011(data2011)

  synth_data <- survey_data[[1]] |>
    create_covariate_data() |>
    select(vars_to_simulate) |>
    distinct() |>
    mutate(p_default = 0) |>
    left_join(synth_data) |>
    mutate(p_synth = coalesce(p_synth, p_default)) |>
    select(-p_default) |>
    mutate(across(where(is.factor), as.character))

  invisible(synth_data)
}


#' clean labour force survey data
#'
clean_lfs_data <- function() {
  lfs_data <-
    read.delim(here::here("../../data/LFS/UKDA-9323-tab/tab/lfsp_js24_eul_pwt24.tab"))

  res <- lfs_data |>
    as_tibble() |>
    select(
      LEVQUL22,  # highest qualification
      BANDG,     # Expected gross earnings, <10,000 = < 1.17 code
      CRYOX7_EUL_Sub,   # Country of birth, 1 UNITED KINGDOM
      CRYOX7_EUL_Main,
      CRY12,     # (921) England, (924) Wales, (923) Scotland, (922) Northern Ireland, (926) UK, Britain (don’t know country)
      LANG,      # First language at home, (1) English
      NSECMJ20,   # NS-SEC major group (SOC2020 based)

      SEX,        # Gender
      AGE,        # Age (continuous) or AGEBAND
      ETHUKEUL,   # Ethnicity (EUL simplified)
      ILODEFR,    # Economic Activity (for Working Status)
      TEN1        # Tenure (for Own Home)
    ) |>
    mutate(
      english_lang = ifelse(LANG == 1, "Yes", "No"),

      uk_born = ifelse(CRYOX7_EUL_Sub == 1, "Yes" ,"No"),

      gross_income = ifelse(as.numeric(sub("^.*\\.", "", BANDG)) %in% 1:16,
                            "<10000",
                            ">=10000"),

      qualification = ifelse(LEVQUL22 %in% c(1,2,3,4,5,6,7),
                             ">=level 2", "<=Level 1"),

      job_status = ifelse(NSECMJ20 %in% c(1,2), "higher",  # managerial
                          ifelse(NSECMJ20 %in% c(3,4), "intermediate",
                                 ifelse(NSECMJ20 %in% c(5,6,7), "lower", "other"))),

      # in Newham resident survey ---

      sex = ifelse(SEX == 1, "Male", "Female"),

      age = ifelse(AGE >= 16 & AGE <= 44, "16-44",
                   ifelse(AGE >= 45, ">=45", "other")),

      ethnicity = ifelse(ETHUKEUL == 1, "White", "BME"),

      workingstatus = ifelse(ILODEFR == 1, "Yes", "No"),

      own_home = ifelse(TEN1 %in% c(1, 2), "Yes", "No"),

      # LFS usually doesn't provide IMD
      # --- TODO --- (approx uniform across quintiles)
      imd = sample(c("1", "2", "3", "4", "5"), size = n(), replace = TRUE)
    ) |>

    select(qualification, gross_income, uk_born, english_lang, job_status,
           sex, age, ethnicity, workingstatus, own_home, imd) |>
    mutate(across(everything(), as.factor))

  res
}
