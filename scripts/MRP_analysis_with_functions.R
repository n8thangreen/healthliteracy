# MRP analysis using functions
# simplified to use in the shiny app


library(purrr)

load(here::here("data/skills_for_life_data.RData"))

survey_data <- clean_data(data)

# fit_stan <- fit_models(survey_data, stan = TRUE)
fit_freq <- fit_models(survey_data, stan = FALSE)

mrp_data <-
  map(survey_data,
      ~create_covariate_data(.x) |>
        create_target_pop_data())

save(mrp_data, file = here::here("data/mrp_data.RData"))

poststratification(fit_freq$lit_glm,
                   mrp_data$lit_dat)

average_marginal_effect(fit_freq$lit_glm,
                        mrp_data$lit_dat)

strat_marginal_effect(fit_freq$lit_glm,
                      mrp_data$lit_dat)
