
#' @title Stratified Marginal Effect
#'
strat_marginal_effect <- function(fit, data) {

  rhs <- "1 + workingstatus * (sex + age + ethnicity + uk_born + english_lang +
              qualification + job_status + gross_income + own_home + imd)"

  fit <- glm(glue("lit_thresholdL2_bin ~ {rhs}"), data = lit_dat,
             family = binomial(), weights = weights)

  names_vars <- all.vars(terms(fit)[[3]])

  ame_dat <- list()
  ps_strat <- list()

  for (i in names_vars) {
    for (j in levels(data$workingstatus)) {

      strat_dat <- filter(data, workingstatus == j)
      strat_dat$predicted_prob <- predict(fit, strat_dat, type = 'response')

      # total
      poststratified_estimates <-
        strat_dat |>
        summarize(estimate = weighted.mean(predicted_prob, product_p))

      # marginal
      fac_levels <- levels(strat_dat[[i]])
      appended_df <- purrr::map_dfr(fac_levels, ~strat_dat %>% mutate({{i}} := .x))

      appended_df$predicted_prob <-
        predict(lit_glm, newdata = appended_df, type = 'response')

      ps_strat[[j]] <-
        appended_df %>%
        group_by(!!sym(i)) %>%
        summarize(estimate = weighted.mean(predicted_prob, product_p)) |>
        mutate(level = j)
    }

    ame_dat[[i]] <- bind_rows(ps_strat)
    ame_dat[[i]]$ame <- ame_dat[[i]]$estimate - poststrat_estimates

    ame_dat[[i]] <-
      ame_dat[[i]] |>
      group_by(level) |>
      mutate(ame_base = estimate - first(estimate))

    # common first column name
    names(ame_dat[[i]])[1] <- "name"
  }
}

