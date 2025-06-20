
#' @title Average Marginal Effect For All Variables
#'
average_marginal_effect <- function(fit, data, save = FALSE) {

  is_stan <- inherits(fit, "stanfit")
  ndraws <- 20

  poststrat_est <- poststratification(fit, data)

  names_fe <- all.vars(terms(fit)[[3]])
  names_re <- rstanarm::ranef(fit) |> names()

  names_vars <- c(names_fe, names_re)

  ame_dat <- list()

  if (inherits(fit, "stanreg")) {

    for (i in names_vars) {
      ame_dat[[i]] <- calc_ame(fit, data, i)
    }

    if (save) {
      save(ame_dat, file = here::here("data/ame_data_stan.RData"))
    }

  } else {

    for (i in names_vars) {
      fac_levels <- levels(data[[i]])

      # assign everyone the same level
      appended_df <- purrr::map_dfr(fac_levels,
                                    ~data %>% mutate({{i}} := .x))
      appended_df$predicted_prob <-
        predict(fit, newdata = appended_df, type = 'response')

      # post-stratification
      ame_dat[[i]] <-
        appended_df %>%
        group_by(!!sym(i)) %>%
        summarize(estimate = weighted.mean(predicted_prob, product_p))

      ame_dat[[i]]$ame <- ame_dat[[i]]$estimate - poststrat_est
      ame_dat[[i]] <- ame_dat[[i]] |> mutate(ame_base = estimate - first(estimate))

      # common first column name
      names(ame_dat[[i]])[1] <- "name"
    }

    if (save) {
      save(ame_dat, file = here::here("data/ame_data.RData"))
    }
  }

  ame_dat
}

#' @title Calculate Average Marginal Effect
#'
calc_ame <- function(fit, data, var) {

  if (inherits(fit, "stanreg")) {
    ndraws <- 20

    fac_levels <- levels(data[[var]])

    # assign everyone the same level
    # counterfactual samples
    appended_df <- purrr::map_dfr(fac_levels,
                                  ~data %>% mutate({{var}} := .x))

    if (nrow(appended_df) == 0) return(data.frame())

    posterior_draws <-
      rstanarm::posterior_epred(
        fit,
        newdata = appended_df,
        draws = ndraws)

    post_draws <-
      cbind(t(posterior_draws)) |>
      as_tibble(.name_repair = "universal")

    names(post_draws) <- gsub(pattern = "...",
                              replacement = "draws_",
                              x = names(post_draws))
    # post-stratification
    ame_dat <-
      appended_df %>%
      cbind(post_draws) %>%
      group_by(!!sym(var)) %>%
      summarize_at(vars(starts_with('draws')),
                   list(~ weighted.mean(., w = product_p)))

    # common first column name
    names(ame_dat)[1] <- "name"

    ame_dat <-
      reshape2::melt(ame_dat) |>
      group_by(variable) |>
      mutate(ame_base = value - first(value))  # ame against base level
  } else {
    ##TODO: frequentist version
  }

  ame_dat
}

