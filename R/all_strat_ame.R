
#' Stratified Average Marginal Effect
#'
all_strat_ame <- function(fit,
                          survey_data,
                          mrp_data,
                          save = FALSE) {
  if (inherits(fit, "stanreg")) {
    ext <- "_stan"
  } else {
    ext <- ""
  }

  names_vars <- all.vars(terms(fit)[[3]])
  strat_ame_data <- list()

  ##TODO: fix error for gross_income
  for (i in names_vars) {
    strat_ame_data[[i]] <-
      strat_marginal_effect(fit,
                            survey_data,
                            mrp_data,
                            interaction = i)
  }

  if (save)
    save(strat_ame_data,
         file = here::here(glue::glue("data/strat_ame_data{ext}.RData")))

  strat_ame_data
}
