
#' Calculate Bayesian AMEs for a single variable
#'
#' @param fit A fitted stanreg or brmsfit model.
#' @param poststrat_data The main post-stratification data frame.
#' @param var_name The name of the variable (string) to calculate AMEs for.
#' @param baseline_draws The vector of posterior draws for the grand mean.
#' @return A data frame of AME posterior draws, or NULL if var_name is not a factor.
#'
calc_ame_bayesian <- function(fit, poststrat_data, var_name,
                              baseline_draws, ndraws = NULL) {

  fac_levels <- levels(poststrat_data[[var_name]])

  appended_df <-
    purrr::map_dfr(fac_levels,
                   ~poststrat_data %>% dplyr::mutate(!!sym(var_name) := .x))

  # 2. Get posterior draws for the appended data
  if (inherits(fit, "stanreg")) {
    posterior_draws <-
      rstanarm::posterior_epred(fit, newdata = appended_df, draws = ndraws)
  } else { # Must be brms
    posterior_draws <-
      brms::posterior_epred(fit, newdata = appended_df, ndraws = ndraws)
  }

  # 3. Post-stratify for each level separately
  n_cells <- nrow(poststrat_data)
  n_levels <- length(fac_levels)
  draws_by_level_list <- list()

  for (j in 1:n_levels) {
    row_indices <- (1 + (j - 1) * n_cells) : (j * n_cells)
    draws_for_level <- posterior_draws[, row_indices]
    poststrat_draws_for_level <- draws_for_level %*% poststrat_data$product_p
    draws_by_level_list[[j]] <- as.vector(poststrat_draws_for_level)
  }

  # 4. Tidy and calculate AMEs *within* each draw

  names(draws_by_level_list) <- fac_levels

  ame_df <- as.data.frame(draws_by_level_list, check.names = FALSE)

  ame_df$draw_id <- 1:nrow(ame_df)

  ame_df_final <- ame_df %>%
    tidyr::pivot_longer(
      cols = -draw_id,
      names_to = "name",
      values_to = "estimate"
    ) %>%
    dplyr::group_by(draw_id) %>%
    dplyr::mutate(
      ame_base = estimate - first(estimate),    # AME relative to first level
      ame = estimate - baseline_draws[draw_id]  # AME relative to grand mean
    ) %>%
    dplyr::ungroup()

  return(ame_df_final)
}

#' Calculate Average Marginal Effects (AMEs)
#'
#' Works for frequentist (glm/glmer) and Bayesian (stanreg/brmsfit) models.
#'
#' @param fit A fitted model object (can be glm, stanreg, brmsfit, or brmsfit_multiple).
#' @param data The poststratification data frame (e.g., from create_covariate_data).
#' @param save_output Boolean, whether to save the fitted models to an .RData file.
#'
#' @return A named list ('ame_dat') where each element contains a data frame
#'         of AMEs for a specific variable.
#'
average_marginal_effect <- function(fit, data, save_output = FALSE, ndraws = NULL) {

  is_bayesian <- inherits(fit, "stanreg") || inherits(fit, "brmsfit")
  is_brms <- inherits(fit, "brmsfit")

  poststrat_est <- poststratification(fit, data, ndraws = ndraws)

  names_re <- character(0)

  if (inherits(fit, "brmsfit")) { # This works for brmsfit_multiple too
    re_structure <- brms::ranef(fit)
    if (!is.null(re_structure) && length(re_structure) > 0) {
      names_re <- names(re_structure)  # imd
    }
  } else if (inherits(fit, "stanreg")) {
    re_structure <- rstanarm::ranef(fit)
    if (!is.null(re_structure) && length(re_structure) > 0) {
      names_re <- names(re_structure)
    }
  } else if (inherits(fit, "glmerMod") || inherits(fit, "lmerMod")) {
    re_structure <- lme4::ranef(fit)
    if (!is.null(re_structure) && length(re_structure) > 0) {
      names_re <- names(re_structure)
    }
  }

  # get Fixed Effect names (names_fe)
  if (inherits(fit, "brmsfit")) {
    the_formula <- fit$formula$formula

    # Get ALL variables from the formula (RHS and LHS)
    all_form_vars <- all.vars(the_formula)

    # Get response variables (e.g., "outcome", "weights")
    resp_vars <- all.vars(brms::bf(the_formula)$resp)

    # Start with all variables, then remove response AND random effects
    names_fe <- setdiff(all_form_vars, c(resp_vars, names_re))

  } else if (inherits(fit, "stanreg") || inherits(fit, "glm") || inherits(fit, "glmerMod")) {
    # These objects work with the standard functions
    names_fe <- all.vars(delete.response(terms(fit)))
    # We still need to remove the 'names_re' from this list for stanreg
    names_fe <- setdiff(names_fe, names_re)
  } else {
    stop("Input 'fit' is not a recognized model object.", call. = FALSE)
  }


  # --- 4. Combine and Filter Variable List ---
  names_vars <- unique(c(names_fe, names_re))
  # Keep only variables that exist in our poststratification data
  names_vars <- intersect(names_vars, names(data))

  ame_dat <- list()

  if (is_bayesian) {
    for (i in names_vars) {
      ame_dat[[i]] <- calc_ame_bayesian(
        fit = fit,
        poststrat_data = data,
        var_name = i,
        baseline_draws = poststrat_est,
        ndraws = ndraws
      )
    }
    ame_dat <- purrr::compact(ame_dat) # Remove NULLs from skipped non-factors

  } else {
    for (i in names_vars) {
      fac_levels <- levels(data[[i]])

      if (is.null(fac_levels)) {
        message(paste("Skipping non-factor variable:", i))
        next
      }

      appended_df <- purrr::map_dfr(fac_levels,
                                    ~data %>% dplyr::mutate({{i}} := .x))
      appended_df$predicted_prob <-
        predict(fit, newdata = appended_df, type = 'response')

      level_summary <-
        appended_df %>%
        dplyr::group_by(!!sym(i)) %>%
        dplyr::summarize(estimate = weighted.mean(predicted_prob, product_p))

      level_summary$ame <- level_summary$estimate - poststrat_est$estimate
      level_summary <- level_summary |> dplyr::mutate(ame_base = estimate - first(estimate))

      names(level_summary)[1] <- "name"
      ame_dat[[i]] <- level_summary
    }
  }

  if (save_output) {
    if (is_bayesian) {
      if (is_brms) {
        save(ame_dat, file = here::here("data/ame_data_brms.RData"))
      } else {
        save(ame_dat, file = here::here("data/ame_data_bayesian.RData"))
      }
    } else {
      save(ame_dat, file = here::here("data/ame_data_freq.RData"))
    }
  }

  return(ame_dat)
}
