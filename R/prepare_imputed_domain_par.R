#' Prepares a combined and imputed 'mids' object for a specific skill domain.
#'
#' @param domain_name The name of the skill domain (e.g., "lit", "num") as a string.
#' @param piaac_data The cleaned PIAAC_survey_data (a list of data frames).
#' @param sfl_data The cleaned SfL2011_survey_data (a list of data frames).
#' @param all_covariates A character vector of all covariate names.
#' @param m_imputations The number of imputations (e.g., 5).
#' @return A 'mids' object.
#'
prepare_imputed_domain_par <- function(domain_name, piaac_data, sfl_data,
                                       all_covariates, m_imputations, n_maxit = 20) {

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

  # 3. Separate covariate data (for imputation)
  # also include weight
  covariate_data <- combined_data %>%
    select(all_of(all_covariates), weights)

  # 4. Separate "other" data (to be added back)
  other_col_names <- setdiff(names(combined_data), names(covariate_data))

  other_data <- combined_data %>%
    select(all_of(other_col_names))

  # 5. Set up the parallel cluster
  # We use one less than the total to leave one for system processes
  n_cores <- parallel::detectCores() - 1

  # Or, to be safe, just use the number of imputations if it's smaller
  n_cores <- min(n_cores, m_imputations)

  cl <- makeCluster(n_cores)

  # 6. Export the data and load 'mice' on all cores
  clusterExport(cl, varlist = c("covariate_data", "n_maxit"), envir = environment())
  clusterEvalQ(cl, library(mice))

  # 7. Run the imputation in parallel
  # We use parLapply to run one job for each imputation (from 1 to m_imputations)
  imp_list <- parLapply(cl, 1:m_imputations, function(i) {

    # Each core does ONE imputation (m = 1)
    mice(covariate_data,
         m = 1,
         maxit = n_maxit,
         seed = i, # Set a unique seed for each job
         print = FALSE)
  })

  # 8. Stop the cluster
  stopCluster(cl)

  # 9. Combine the list of 'mids' objects into one
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
#' @param m_imputations The number of imputations (e.g., 5).
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

