# MRP analysis using functions
# simplified to use in the shiny app


library(purrr)

load(here::here("data/skills_for_life_data.RData"))

survey_data <- clean_data(data)

use_stan <- TRUE
out <- "lit"

fit <- fit_models(survey_data, stan = use_stan)

mrp_data <-
  map(survey_data,
      ~create_covariate_data(.x) |>
        create_target_pop_data())

save(mrp_data, file = here::here("data/mrp_data.RData"))

poststratification(fit[[out]],
                   mrp_data[[out]])

ame_data <-
  average_marginal_effect(fit[[out]],
                          mrp_data[[out]],
                          save = TRUE)

strat_ame_data <-
  all_strat_ame(fit[[out]],
                survey_data[[out]],
                mrp_data[[out]],
                save = TRUE)
