
#' @title Conditional Average Treatment Effect (CATE)
#'
#' @param fit glm or stan model output
#' @param survey_dat data frame
#' @param interaction Name of the interaction term
#' @return List
#'
cond_treatment_effect <- function(fit,
                                  survey_dat,
                                  mrp_dat,
                                  interaction = "workingstatus") {
  ndraws <- 20

  names_vars <- all.vars(terms(fit)[[3]])
  response <- all.vars(terms(fit))[1]

  main_vars <- names_vars[names_vars != interaction]
  main_term <- paste(main_vars, collapse = " + ")

  # formula with interaction term
  rhs <- glue::glue("1 + {interaction} * ({main_term})")
  form <- glue("{response} ~ {rhs}")

  cate_data <- list()
  ps_cate <- list()

  if (inherits(fit, "stanreg")) {
    fit <- rstanarm::stan_glm(form, data = survey_dat,
                              family = binomial(), weights = weights,
                              chains = 1, iter = 1000)
    for (i in names_vars) {
      for (j in levels(mrp_dat[[interaction]])) {
        # stratified / conditional data
        cate_dat <- filter(mrp_dat, !!sym(interaction) == j)

        ps_cate[[j]] <-
          calc_ame(fit, cate_dat, i) |>
          mutate(level = j)
      }

      cate_data[[i]] <- bind_rows(ps_cate)
    }

  } else {
    fit <- glm(form, data = survey_dat,
               family = binomial(), weights = weights)

    for (i in names_vars) {
      for (j in levels(mrp_dat[[interaction]])) {
        cate_dat <- filter(mrp_dat, !!sym(interaction) == j)
        cate_dat$predicted_prob <- predict(fit, cate_dat, type = 'response')

        # total
        poststrat_est <-
          cate_dat |>
          summarize(estimate = weighted.mean(predicted_prob, product_p))

        # marginal
        fac_levels <- levels(cate_dat[[i]])

        # assign everyone the same level
        appended_df <- purrr::map_dfr(fac_levels,
                                      ~cate_dat %>% mutate({{i}} := .x))

        appended_df$predicted_prob <-
          predict(fit, newdata = appended_df, type = 'response')

        # post-stratification
        ps_cate[[j]] <-
          appended_df %>%
          group_by(!!sym(i)) %>%
          summarize(estimate = weighted.mean(predicted_prob, product_p)) |>
          mutate(level = j)
      }

      cate_data[[i]] <- bind_rows(ps_cate)

      # average marginal effect
      cate_data[[i]] <-
        cate_data[[i]] |>
        group_by(level) |>
        mutate(ame_base = estimate - first(estimate))

      # common first column name
      names(cate_data[[i]])[1] <- "name"
    }
  }

  cate_data
}

