
#' @title Stratified Marginal Effect
#'
#' @param fit glm or stan model output
#' @param survey_dat data frame
#' @param interaction Name of the interaction term
#' @return List
#'
strat_marginal_effect <- function(fit, survey_dat, mrp_dat,
                                  interaction = "workingstatus") {
  ndraws <- 20

  names_vars <- all.vars(terms(fit)[[3]])
  response <- all.vars(terms(fit))[1]

  main_vars <- names_vars[names_vars != interaction]
  main_term <- paste(main_vars, collapse = " + ")

  # formula with interaction term
  rhs <- glue::glue("1 + {interaction} * ({main_term})")
  form <- glue("{response} ~ {rhs}")

  strat_ame_data <- list()
  ps_strat <- list()

  if (inherits(fit, "stanreg")) {
    fit <- rstanarm::stan_glm(form, data = survey_dat,
                              family = binomial(), weights = weights,
                              chains = 1, iter = 1000)
    for (i in names_vars) {
      for (j in levels(mrp_dat[[interaction]])) {
        # stratified data
        strat_dat <- filter(mrp_dat, !!sym(interaction) == j)

        ps_strat[[j]] <-
          calc_ame(fit, strat_dat, i) |>
          mutate(level = j)
      }

      strat_ame_data[[i]] <- bind_rows(ps_strat)
    }

  } else {
    fit <- glm(form, data = survey_dat,
               family = binomial(), weights = weights)

    for (i in names_vars) {
      for (j in levels(mrp_dat[[interaction]])) {
        strat_dat <- filter(mrp_dat, !!sym(interaction) == j)
        strat_dat$predicted_prob <- predict(fit, strat_dat, type = 'response')

        # total
        poststrat_est <-
          strat_dat |>
          summarize(estimate = weighted.mean(predicted_prob, product_p))

        # marginal
        fac_levels <- levels(strat_dat[[i]])

        # assign everyone the same level
        appended_df <- purrr::map_dfr(fac_levels,
                                      ~strat_dat %>% mutate({{i}} := .x))

        appended_df$predicted_prob <-
          predict(fit, newdata = appended_df, type = 'response')

        # post-stratification
        ps_strat[[j]] <-
          appended_df %>%
          group_by(!!sym(i)) %>%
          summarize(estimate = weighted.mean(predicted_prob, product_p)) |>
          mutate(level = j)
      }

      strat_ame_data[[i]] <- bind_rows(ps_strat)

      # average marginal effect
      strat_ame_data[[i]] <-
        strat_ame_data[[i]] |>
        group_by(level) |>
        mutate(ame_base = estimate - first(estimate))

      # common first column name
      names(strat_ame_data[[i]])[1] <- "name"
    }
  }

  strat_ame_data
}

