
#' Prepares a combined and imputed 'mids' object for a specific skill domain
#'
#' @param domain_name The name of the skill domain (e.g., "lit", "num") as a string.
#' @param piaac_data The cleaned PIAAC_survey_data (a list of data frames).
#' @param sfl_data The cleaned SfL2011_survey_data (a list of data frames).
#' @param all_covariates A character vector of all covariate names.
#' @param m_imputations The number of imputations (e.g., 5).
#' @param noise_sd Proxy noise standard deviation
#' @return A 'mids' object from mice package
#'
prepare_imputed_domain_par <- function(domain_name, piaac_data, sfl_data,
                                       all_covariates, m_imputations,
                                       n_maxit = 20, noise_sd = 0,
                                       include_outcome = FALSE) {

  # 1. Combine the two surveys for the *specific domain*
  combined_data <- dplyr::bind_rows(
    list(piaac = piaac_data[[domain_name]],
         sfl2011 = sfl_data[[domain_name]]),
    .id = "survey"
  )

  # 2. Ensure all covariates from the master list are present
  missing_covs <- setdiff(all_covariates, names(combined_data))

  if (length(missing_covs) > 0) {
    combined_data[missing_covs] <- NA
  }

  # identify outcome variable to create the proxy
  outcome_var <- if (domain_name == "lit") "lit_thresholdL2_bin" else "num_thresholdL1_bin"

  # 3. Separate covariate data (for imputation)
  # also include weight
  if (include_outcome) {
    vars_for_imputation <- unique(c(all_covariates, "weights", outcome_var))
  } else {
    vars_for_imputation <- unique(c(all_covariates, "weights"))
  }

  covariate_data <- select(combined_data,
                           all_of(vars_for_imputation))

  # generate noise once so all workers see the exact same "observed" data

  noisy_name <- NULL

  if (include_outcome && noise_sd > 0) {
    noisy_name <- paste0(outcome_var, "_noisy")

    # Generate noise
    original_outcome <- as.numeric(covariate_data[[outcome_var]])
    noise_vec <- rnorm(nrow(covariate_data), mean = 0, sd = noise_sd)

    covariate_data[[noisy_name]] <- original_outcome + noise_vec
  }

  # 4. Separate "other" data (to be added back)
  other_col_names <- setdiff(names(combined_data), names(covariate_data))

  other_data <- combined_data %>%
    select(all_of(other_col_names))

  # 5. Set up the parallel cluster
  # use one less than the total to leave one for system processes
  n_cores <- parallel::detectCores() - 1
  n_cores <- min(n_cores, m_imputations)

  cl <- makeCluster(n_cores)

  # 6. Export the data and load 'mice' on all cores
  clusterExport(cl, varlist = c("covariate_data", "n_maxit",
                                "outcome_var", "noise_sd"),
                envir = environment())

  clusterEvalQ(cl, library(mice))

  # 7. Run the imputation in parallel
  # parLapply to run one job for each imputation (from 1 to m_imputations)

  imp_list <- parLapply(cl, 1:m_imputations, function(i) {

    data_to_impute <- covariate_data

    # 1. Define Matrix & Method defaults
    pred_matrix <- make.predictorMatrix(data_to_impute)
    method_vec  <- make.method(data_to_impute)

    # --- Matrix Logic for Noisy Proxy ---
    if (!is.null(noisy_name)) {

      # 1. Do not impute the proxy itself
      method_vec[noisy_name] <- ""
      pred_matrix[noisy_name, ] <- 0

      # 2. Covariates use Proxy, NOT Real Outcome
      predictors <- rownames(pred_matrix)
      predictors <- predictors[!predictors %in% c(outcome_var, noisy_name)]

      pred_matrix[predictors, noisy_name]  <- 1
      pred_matrix[predictors, outcome_var] <- 0

      # 3. Real outcome (if imputed) ignores proxy to avoid circularity
      pred_matrix[outcome_var, noisy_name] <- 0
    }

    # Each core does ONE imputation
    mice(data_to_impute,
         m = 1,
         maxit = n_maxit,
         predictorMatrix = pred_matrix, # custom matrix
         method = method_vec,           # custom method
         seed = i,
         print = FALSE)
  })

  stopCluster(cl)

  # 9. Combine list of 'mids' objects into one
  imp <- Reduce(ibind, imp_list)

  # 10. Add back the "other" data (outcome, id, survey)
  full_imp_object <- mice::cbind(imp, other_data)

  # 11. PIAAC only
  imp_object_piaac <- full_imp_object %>%
    filter(survey == "piaac")

  return(imp_object_piaac)
}

#' Serial Version of Prepares a combined and imputed 'mids' object for a specific skill domain.
#'
#' @param domain_name The name of the skill domain (e.g., "lit", "num") as a string.
#' @param piaac_data The cleaned PIAAC_survey_data (a list of data frames).
#' @param sfl_data The cleaned SfL2011_survey_data (a list of data frames).
#' @param all_covariates A character vector of all covariate names.
#' @param m_imputations The number of imputations.
#' @return A 'mids' object.
#'
prepare_imputed_domain <- function(domain_name, piaac_data, sfl_data,
                                   all_covariates, m_imputations, n_maxit = 20) {
  # 1. Combine the two surveys for the *specific domain*
  combined_data <- dplyr::bind_rows(
    list(piaac = piaac_data[[domain_name]],
         sfl2011 = sfl_data[[domain_name]]),
    .id = "survey"
  )

  # 2. Ensure all covariates from the master list are present
  # (Adds any missing ones as columns full of NA)
  missing_covs <- setdiff(all_covariates, names(combined_data))

  if (length(missing_covs) > 0) {
    combined_data[missing_covs] <- NA
  }

  # 3. Separate covariate data (for imputation)
  # also include weight
  covariate_data <- combined_data %>%
    select(all_of(all_covariates), weights)

  # 4. Separate "other" data (to be added back)
  other_col_names <- setdiff(names(combined_data), names(covariate_data))

  other_data <- combined_data %>%
    select(all_of(other_col_names))

  # --- Imputation

  imp <- mice(covariate_data, m = m_imputations, print = FALSE, maxit = n_maxit)

  # 6. Add back the "other" data (outcome, id, survey)
  full_imp_object <- mice::cbind(imp, other_data)

  # PIAAC only
  imp_object_piaac <- full_imp_object %>%
    filter(survey == "piaac")

  return(imp_object_piaac)
}

