
#' Create labour force survey synthetic data
#'
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
create_lfs_synth_data <- function() {

  lfs_data <-
    read.delim(here::here("../../data/LFS/UKDA-9323-tab/tab/lfsp_js24_eul_pwt24.tab"))

  demo_data <- lfs_data |>
    as_tibble() |>
    select(LEVQUL22,  # highest qualification
           BANDG,    # Expected gross earnings, <10,000 = < 1.17 code
           CRYOX7_EUL_Sub,   # Country of birth, 1 UNITED KINGDOM
           CRYOX7_EUL_Main,
           CRY12,    # (921) England, (924) Wales, (923) Scotland, (922) Northern Ireland, (926) UK, Britain (don’t know country)
           LANG,     # First language at home, (1) English
           NSECMJ20  # NS-SEC major group (SOC2020 based)
    ) |>
    mutate(
      english_lang = ifelse(LANG == 1, "Yes", "No"),
      uk_born = ifelse(CRYOX7_EUL_Sub == 1, "Yes" ,"No"),
      gross_income = ifelse(as.numeric(sub("^.*\\.", "", BANDG)) %in% 1:16,
                            "<10000",
                            ">=10000"),
      qualification = ifelse(LEVQUL22 %in% c(1,2,3,4,5,6,7),
                             ">=level 2", "<=Level 1"),
      job_status = ifelse(NSECMJ20 %in% c(1,2),
                          "higher", ifelse(NSECMJ20 %in% c(3,4),
                                           "intermediate", "lower"))
    ) |>
    select(qualification, gross_income, uk_born, english_lang, job_status) |>
    mutate(across(everything(), as.factor))

  # table(
  #   demo_data$job_status,
  #   demo_data$english_lang,
  #   demo_data$uk_born,
  #   demo_data$gross_income,
  #   demo_data$qualification
  # ) |>
  #   prop.table()

  #########
  # simpop

  target_marginals_props <- demo_prop_tables()

  N_small_area <- 1000 # Let's say you want 1000 synthetic individuals

  # Convert proportions to counts for simPop
  # counts are integers and sum exactly to N_small_area
  target_counts_list <- list(
    qualification = setNames(
      round(
        target_marginals_props$qualification$p_qual * N_small_area
      ),
      target_marginals_props$qualification$qualification
    ),
    gross_income = setNames(
      round(
        target_marginals_props$gross_income$p_income * N_small_area
      ),
      target_marginals_props$gross_income$gross_income
    ),
    uk_born = setNames(
      round(target_marginals_props$uk_born$p_uk * N_small_area),
      target_marginals_props$uk_born$uk_born
    ),
    english_lang = setNames(
      round(
        target_marginals_props$english_lang$p_english * N_small_area
      ),
      target_marginals_props$english_lang$english_lang
    ),
    job_status = setNames(
      round(target_marginals_props$job_status$p_job * N_small_area),
      target_marginals_props$job_status$job_status
    )
  )

  # Adjust for rounding errors to ensure they sum to N_small_area
  for (i in seq_along(target_counts_list)) {
    diff <- N_small_area - sum(target_counts_list[[i]])

    if (diff != 0) {
      # Add/subtract the difference from the largest category
      largest_cat_idx <- which.max(target_counts_list[[i]])
      target_counts_list[[i]][largest_cat_idx] <-
        target_counts_list[[i]][largest_cat_idx] + diff
    }
  }

  # 1. Create a simPop input object
  demo_data$person_id <- 1:nrow(demo_data)
  demo_data$sim_weight <- 1
  demo_data$strata_col <- factor("default_strata")

  inp <- simPop::specifyInput(
    data = as.data.frame(demo_data),
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

  vars_to_simulate <- names(target_counts_list)

  pers_tables_for_calibPop <- lapply(vars_to_simulate, function(var_name) {
    current_demo_data_levels <- levels(demo_data[[var_name]])
    dummy_strata_level <- levels(demo_data$strata_col)[1]

    df <- data.frame(
      Category = names(target_counts_list[[var_name]]),
      Freq = as.numeric(target_counts_list[[var_name]]),
      stringsAsFactors = FALSE
    )
    colnames(df)[1] <- var_name

    # Add the strata_col to each marginal table
    df$strata_col <- dummy_strata_level # All entries get the same dummy strata value

    df[[var_name]] <- factor(df[[var_name]], levels = current_demo_data_levels)
    df[["strata_col"]] <- factor(df[["strata_col"]], levels = levels(demo_data$strata_col))

    return(df)
  })

  names(pers_tables_for_calibPop) <- vars_to_simulate

  # Step 3: Calibrate the Simulated Population to External Marginals
  # This is the step where `persTables` is used, matching YOUR calibPop signature.
  final_sim_pop_obj <- calibPop(
    inp = sim_pop_obj,
    persTables = pers_tables_for_calibPop,
    split = "strata_col",
    verbose = TRUE,
    maxiter = 200                    # Using 'maxiter' from your calibPop signature
  )

  synth_data <- simPop::popData(final_sim_pop_obj)

  synth_data <-
    synth_data |>
    as_tibble() |>
    dplyr::count(!!!syms(vars_to_simulate), name = "frequency") |>
    ungroup() |>
    mutate(p_synth = round(frequency/sum(frequency), 3))

  save(synth_data, file = here::here("data/synth_data.rda"))

  synth_data
}
