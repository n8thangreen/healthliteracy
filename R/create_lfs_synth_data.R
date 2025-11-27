
#' Create labour force survey synthetic data
#'
#' Using {simPop} package to perform iterative proportional fitting (IPF).
#'
#' @param target_marginals_props Target marginal proportions as a list.
#' @import dplyr
#' @import simPop
#' @import ipfp
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
    target_marginals_props <- get_newham_census_props()
  }

  N_small_area <- 10000

  # Convert proportions to counts for simPop
  # counts are integers and sum exactly to N_small_area
  # Dynamic list creation based on input names
  # matches "p_xxx" column in marginal tables to variable name
  target_counts_list <- lapply(names(target_marginals_props), function(var) {

    df <- target_marginals_props[[var]]

    # Identify proportion column (assumes it starts with "p_")
    prop_col <- grep("^p_", names(df), value = TRUE)

    counts <- setNames(
      round(df[[prop_col]] * N_small_area), df[[var]])

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

  lfs_data$person_id <- 1:nrow(lfs_data)
  lfs_data$sim_weight <- 1
  lfs_data$strata_col <- factor("default_strata")

  inp <- simPop::specifyInput(
    data = as.data.frame(lfs_data),
    hhid = "person_id",
    weight = "sim_weight",
    strata = "strata_col",
  )

  # NOTE: generating all vars in simStructure limits you to combinations
  # present in the input LFS data.
  sim_struc <- simPop::simStructure(
    data = inp,
    method = "direct",
    basicHHvars = names(target_counts_list))

  # Note: No simCategorical needed if you aren't modeling NEW variables,
  # but relying on the structure of the input data.
  sim_pop_obj <- simPop::simCategorical(
    simPopObj = sim_struc,
    additional = NULL,
    method = "multinom",
    by = "strata_col"
  )

  vars_to_simulate <- names(target_counts_list)

  if (FALSE) {
    # --- calibration: simulated annealing


    pers_tables_for_calibPop <-
      lapply(vars_to_simulate, function(var_name) {

        current_levels <- levels(lfs_data[[var_name]])

        dummy_strata_level <- levels(lfs_data$strata_col)[1]

        df <- data.frame(
          Category = names(target_counts_list[[var_name]]),
          Freq = as.numeric(target_counts_list[[var_name]]),
          stringsAsFactors = FALSE
        )

        colnames(df)[1] <- var_name

        # add strata_col to each marginal table
        df$strata_col <- dummy_strata_level # All entries get same dummy strata value

        df[[var_name]] <- factor(df[[var_name]], levels = current_levels)

        df[["strata_col"]] <- factor(df[["strata_col"]],
                                     levels = levels(lfs_data$strata_col))
        return(df)
      })

    names(pers_tables_for_calibPop) <- vars_to_simulate

    # Calibrate the Simulated Population to External Marginals
    final_sim_pop_obj <- simPop::calibPop(
      inp = sim_pop_obj,
      persTables = pers_tables_for_calibPop,
      split = "strata_col",
      epsP.factor = 0.5,
      temp = 100,
      verbose = TRUE,
      maxiter = 500
    )

    # --- Extract and Calculate Probabilities

    synth_data_raw <- simPop::popData(final_sim_pop_obj)

    # Ensure we identify the correct weight column
    # (calibPop usually names it 'calibWeight')
    weight_col <- if ("calibWeight" %in% names(synth_data_raw)) {
      "calibWeight"
    } else {
      "weight"
    }

    synth_counts <- synth_data_raw |>
      as_tibble() |>
      # Group by the specific combinations of variables
      group_by(!!!syms(vars_to_simulate)) |>
      # SUM THE WEIGHTS, do not just count the rows
      summarise(weighted_n = sum(.data[[weight_col]]), .groups = "drop") |>
      mutate(p_synth = round(weighted_n / sum(weighted_n), 5)) |>
      select(-weighted_n)
  }


  # --- ALTERNATIVE CALIBRATION: IPU (Raking) ---

  synth_raw <- simPop::popData(sim_pop_obj)
  vars_to_calibrate <- names(target_counts_list)

  # --- 2. Create the "Target Vector" and "Model Matrix" ---
  # IPFP needs:
  # A: A matrix where rows = constraints, columns = people (0 or 1)
  # y: A vector of the target counts

  target_vector <- numeric()
  constraint_rows <- list()

  row_counter <- 0

  for (var in vars_to_calibrate) {
    # Get the specific targets for this variable
    targets <- target_counts_list[[var]]
    levels_needed <- names(targets)

    # Add to the master target vector
    target_vector <- c(target_vector, as.numeric(targets))

    # Create a binary row for each level
    # If person has var="Female", the "Female" row gets a 1, otherwise 0
    for (lvl in levels_needed) {
      row_counter <- row_counter + 1

      # Create binary indicator (safe against factors/chars)
      binary_indicator <- as.numeric(as.character(synth_raw[[var]]) == lvl)

      # Safety check: if constraint level doesn't exist in data, this is 0s
      if (sum(binary_indicator) == 0) {
        warning(paste("Constraint category missing in data:", var, "-", lvl))
      }

      constraint_rows[[row_counter]] <- binary_indicator
    }
  }

  # Combine list into the matrix 'A' (Constraints x Individuals)
  A <- do.call(rbind, constraint_rows)

  # --- 3. Run Raking (IPFP) ---

  # Initial weights (1 for everyone)
  w0 <- rep(1, ncol(A))

  # Run the solver
  # tol = tolerance (1e-4 is good standard)
  final_weights <-
    ipfp::ipfp(y = target_vector, A = A,
               x0 = w0, tol = 1e-4, maxit = 1000)

  synth_counts <- as_tibble(synth_raw) |>
    mutate(calibWeight = final_weights) |> # Attach new weights
    group_by(!!!syms(vars_to_simulate)) |>
    summarise(weighted_n = sum(calibWeight), .groups = "drop") |>
    mutate(p_synth = round(weighted_n / sum(weighted_n), 5)) |>
    select(-weighted_n)

  # --- join to grid

  load(here::here("data/skills_for_life_2011_data.RData"))
  survey_data <- clean_sfl_data_2011(data2011)

  # Create the full grid from Survey Data
  full_grid <- survey_data[[1]] |>
    create_covariate_data() |>
    select(all_of(vars_to_simulate)) |>
    distinct() |>
    mutate(across(where(is.factor), as.character)) # Convert to character to avoid factor level mismatches

  # Convert synth data to character for safe joining
  synth_counts_safe <- synth_counts |>
    mutate(across(all_of(vars_to_simulate), as.character))

  final_data <- full_grid |>
    left_join(synth_counts_safe, by = vars_to_simulate) |>
    mutate(p_synth = coalesce(p_synth, 0)) # Fill non-matches with 0

  return(final_data)
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
                             ">=Level 2", "<=Level 1"),

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
    filter(age != "other") |>
    mutate(across(where(is.character), as.factor))

  res
}
