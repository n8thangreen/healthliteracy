# MRP analysis using functions
# simplified to use in the shiny app


library(purrr)

load(here::here("data/skills_for_life_data.RData"))

survey_data <- clean_data(data)

fit_stan <- fit_models(survey_data, stan = TRUE)
fit_freq <- fit_models(survey_data, stan = FALSE)

hl_type_name <- names(survey_data)

mrp_data <-
  map(hl_type_name,
      ~survey_data[[.x]] |>
        create_covariate_data() |>
        create_target_pop_data())

poststratification(fit_freq$lit_dat, mrp_data$lit_dat)
