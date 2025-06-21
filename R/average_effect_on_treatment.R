
#' @title Average Treatment Effect on Treatment For All Variables
#'
average_effect_on_treatment <- function(fit, data, save = FALSE) {

  is_stan <- inherits(fit, "stanreg")
  ndraws <- 20

  names_fe <- all.vars(terms(fit)[[3]])
  names_re <- rstanarm::ranef(fit) |> names()

  names_vars <- c(names_fe, names_re)

  att_dat <- list()

  if (is_stan) {

    for (i in names_vars) {
      att_dat[[i]] <- calc_att_stan(fit, data, i)
    }

    if (save)
      save(att_dat, file = here::here("data/att_data_stan.RData"))

  } else {

    for (i in names_vars) {
      att_dat[[i]] <- calc_att_base(fit, data, i)
    }

    if (save)
      save(att_dat, file = here::here("data/att_data.RData"))
  }

  att_dat
}

#' @title Calculate Average Treatment Effect on Treatment for Stan
#'
#' this is essentially just a change in data input
#' i.e. subpopulation profile
#' relative to the ame calculation for the whole population
#'
calc_att_stan <- function(fit, data, var) {
  is_stan <- inherits(fit, "stanreg")

  if (!is_stan) stop()

  ndraws <- 20

  fac_levels <- levels(data[[var]])

  # subset the data to just those 'on treatment'
  # i.e. those who have the covariate value
  # changed in the counterfactual

  level_dat_lst <- list()

  for (i in fac_levels) {
    # subpopulation data at a 'treatment' level only
    # rather than whole sample for ame
    level_dat <- data |> filter(!!sym(var) == i)

    # assign everyone the same level
    # counterfactual samples like regular ate
    level_dat_lst[[i]] <- purrr::map_dfr(fac_levels,
                                         ~level_dat %>% mutate({{var}} := .x))
  }

  # create combined from-to label
  appended_df <-
    level_dat_lst |>
    bind_rows(, .id = "level_from") |>
    mutate(from_to = paste(level_from, !!sym(var), sep = "_")) |>
    select(from_to, everything()) |>    # move to start
    dplyr::filter(level_from != !!sym(var)) #todo need this for comparison. Delete

  ##TODO: following is duplicate code from calc_ame

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

  # combine inputs with posterior samples
  # then post-stratification
  att_dat_wide <-
    appended_df %>%
    cbind(post_draws) %>%
    group_by(from_to) %>%
    filter(sum(product_p, na.rm = TRUE) > 0) %>%    # weight.mean doesnt allow sum(w) == 0
    # group_by(!!sym(var)) %>%
    summarize_at(vars(starts_with('draws')),
                 list(~ weighted.mean(., w = product_p)))

  att_dat <-
    reshape2::melt(att_dat_wide) |>
    group_by(variable, level_from) |>              # variable is the draw number
    mutate(ame_base = value - first(value)) |>  # att calculation. prediction of treated vs base level_to
    rename(pop = level_from, name = from_to)  # to match ame and use same plotting functions

  att_dat
}


##TODO:
#' @title Calculate Average Treatment Effect on Treatment for Frequentist
#'
cal_att_base <- function(fit, data, var) {

  poststrat_est <- poststratification(fit, data)

  fac_levels <- levels(data[[i]])

  # assign everyone the same level
  appended_df <- purrr::map_dfr(fac_levels,
                                ~data %>% mutate({{i}} := .x))
  appended_df$predicted_prob <-
    predict(fit, newdata = appended_df, type = 'response')

  # post-stratification
  att_dat[[i]] <-
    appended_df %>%
    group_by(!!sym(i)) %>%
    summarize(estimate = weighted.mean(predicted_prob, product_p))

  att_dat[[i]]$ame <- att_dat[[i]]$estimate - poststrat_est
  att_dat[[i]] <- att_dat[[i]] |> mutate(ame_base = estimate - first(estimate))

  # common first column name
  names(att_dat[[i]])[1] <- "name"

  att_dat
}

