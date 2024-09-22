
#' @title Stratified Marginal Effect
#'
#' @param fit glm or stan model output
#' @param survey_dat data frame
#' @param interaction Name of the interaction term
#' @return List
#'
strat_marginal_effect <- function(fit, survey_dat, mrp_dat, interaction = "workingstatus") {

  names_vars <- all.vars(terms(fit)[[3]])
  response <- all.vars(terms(fit))[1]

  main_vars <- names_vars[names_vars != interaction]
  main_term <- paste(main_vars, collapse = " + ")

  # formula with interaction term
  rhs <- glue::glue("1 + {interaction} * ({main_term})")

  fit <- glm(glue("{response} ~ {rhs}"), data = survey_dat,
             family = binomial(), weights = weights)

  ame_dat <- list()
  ps_strat <- list()

  for (i in names_vars) {
    for (j in levels(mrp_dat[[interaction]])) {
      strat_dat <- filter(mrp_dat, !!sym(interaction) == j)
      strat_dat$predicted_prob <- predict(fit, strat_dat, type = 'response')

      # total
      poststratified_estimates <-
        strat_dat |>
        summarize(estimate = weighted.mean(predicted_prob, product_p))

      # marginal
      fac_levels <- levels(strat_dat[[i]])
      appended_df <- purrr::map_dfr(fac_levels, ~strat_dat %>% mutate({{i}} := .x))

      appended_df$predicted_prob <-
        predict(fit, newdata = appended_df, type = 'response')

      ps_strat[[j]] <-
        appended_df %>%
        group_by(!!sym(i)) %>%
        summarize(estimate = weighted.mean(predicted_prob, product_p)) |>
        mutate(level = j)
    }
    ame_dat[[i]] <- bind_rows(ps_strat)

    ame_dat[[i]] <-
      ame_dat[[i]] |>
      group_by(level) |>
      mutate(ame_base = estimate - first(estimate))

    # common first column name
    names(ame_dat[[i]])[1] <- "name"
  }

  ame_dat
}

