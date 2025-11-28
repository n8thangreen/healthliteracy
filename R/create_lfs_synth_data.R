
#' Create labour force survey synthetic data
#'
#' Using {simPop} package to perform iterative proportional fitting (IPF).
#'
#' @param smooth_alpha
#' @param add_missing
#' @param target_marginals_props Target marginal proportions as a list.
#'
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
create_lfs_synth_data <- function(target_marginals_props = NULL,
                                  smooth_alpha = 0.5,
                                  add_missing = FALSE) {

  lfs_data <- clean_lfs_data()

  # -- include 'ghost' rows for missing people
  if (add_missing) {
    # 1. Create the full grid
    vars_to_grid <- names(target_marginals_props)
    full_grid <- expand.grid(lapply(lfs_data[, vars_to_grid], unique))

    # 2. Find missing rows
    missing_rows <- dplyr::anti_join(full_grid, lfs_data, by = vars_to_grid)

    ##TODO:
    # # --- SAFETY FILTER START ---
    # # Remove logical impossibilities before adding them to data
    # if (nrow(missing_rows) > 0) {
    #   missing_rows <- missing_rows |>
    #     filter(
    #       !(age_band == "16-19" & qualification == "Level 4+ (Degree/PhD)"),
    #       !(age_band == "16-19" & economic_status == "Retired"),
    #       !(economic_status == "Unemployed" & occupation_group == "Managerial")
    #       # Add other "Impossible" rules here
    #     )
    # }
    # # --- SAFETY FILTER END ---

    # 3. Add to data (with small weight)
    if (nrow(missing_rows) > 0) {
      # missing_rows$person_id <- 9000000 + 1:nrow(missing_rows)
      # missing_rows$sim_weight <- 0.01  # "Ghost" weight
      lfs_data <- bind_rows(lfs_data, missing_rows)
    }
  }

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

  # --- CALIBRATION: IPU (Raking) ---

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

  # --- 4. Process Results & Apply Laplace Smoothing ---

  # A. Calculate RAW weighted counts from the synthetic data
  # Do NOT calculate proportions (p_synth) yet. Just get the sum of weights.
  synth_counts_raw <- as_tibble(synth_raw) |>
    mutate(calibWeight = final_weights) |>
    group_by(!!!syms(vars_to_simulate)) |>
    summarise(weighted_n = sum(calibWeight), .groups = "drop") |>
    mutate(across(all_of(vars_to_simulate), as.character)) # Ensure char for joining

  # --- 5. Join to Grid and Smooth ---

  load(here::here("data/skills_for_life_2011_data.RData"))
  survey_data <- clean_sfl_data_2011(data2011)

  # B. Create the full grid (All possible combinations)
  full_grid <- survey_data[[1]] |>
    create_covariate_data() |>
    select(all_of(vars_to_simulate)) |>
    distinct() |>
    mutate(across(where(is.factor), as.character))

  # C. Join, Fill Zeros, Apply Smoothing, Calc Probabilities
  final_data <- full_grid |>
    left_join(synth_counts_raw, by = vars_to_simulate) |>

    # 1. Fill missing combos with 0 (Real zeros)
    mutate(weighted_n = coalesce(weighted_n, 0)) |>

    # 2. LAPLACE SMOOTHING: Add a small constant (alpha) to EVERY row
    # alpha = 0.5 is standard (Jeffrey's prior). You can use 1.0 for stronger smoothing.
    mutate(weighted_n_smoothed = weighted_n + smooth_alpha) |>

    # 3. Calculate Probability using the SMOOTHED sum
    # This ensures p_synth is never exactly 0.
    mutate(p_synth = round(weighted_n_smoothed / sum(weighted_n_smoothed), 8)) |>

    select(-weighted_n, -weighted_n_smoothed)

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

      # also in Newham resident survey ---

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
