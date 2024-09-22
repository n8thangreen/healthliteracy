
#' @title Average Marginal Effect
#'
average_marginal_effect <- function(fit, data) {

  poststrat_est <-
    poststratification(fit, data)

  names_vars <- all.vars(terms(fit)[[3]])

  ame_dat <- list()

  for (i in names_vars) {
    fac_levels <- levels(data[[i]])
    appended_df <- purrr::map_dfr(fac_levels,
                                  ~data %>% mutate({{i}} := .x))
    appended_df$predicted_prob <-
      predict(fit, newdata = appended_df, type = 'response')

    ame_dat[[i]] <-
      appended_df %>%
      group_by(!!sym(i)) %>%
      summarize(estimate = weighted.mean(predicted_prob, product_p))

    ame_dat[[i]]$ame <- ame_dat[[i]]$estimate - poststrat_est
    ame_dat[[i]] <- ame_dat[[i]] |> mutate(ame_base = estimate - first(estimate))

    # common first column name
    names(ame_dat[[i]])[1] <- "name"
  }

  ame_dat
}
