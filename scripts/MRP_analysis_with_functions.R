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

ame_data <-
  average_marginal_effect(fit_freq$lit_glm,
                          mrp_data$lit_dat)

save(ame_data, file = here::here("data/ame_data.RData"))

# stratified average marginal effect

names_vars <- all.vars(terms(fit_freq[[1]])[[3]])
stat_ame_data <- list()

##TODO: fix error for gross_income
for (i in names_vars) {
  stat_ame_data[[i]] <-
    strat_marginal_effect(fit_freq$lit_glm,
                          survey_data$lit_dat,
                          mrp_data$lit_dat,
                          interaction = i)
}

save(stat_ame_data, file = here::here("data/stat_ame_data.RData"))
