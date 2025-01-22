# MRP analysis using functions
# simplified to use in the shiny app


library(purrr)

load(here::here("data/skills_for_life_data.RData"))

survey_data <- clean_data(data)

use_stan <- TRUE

fit <- fit_models(survey_data, stan = use_stan)

mrp_data <-
  map(survey_data,
      ~create_covariate_data(.x) |>
        create_target_pop_data())

save(mrp_data, file = here::here("data/mrp_data.RData"))

###########
# outcomes

out_name <- c("lit", "num", "ict")

poststrat <- list()
ame_data <- list()
strat_ame_data <- list()

for (i in out_name) {

  poststrat[[i]] <-
    poststratification(fit[[i]],
                       mrp_data[[i]])

  ame_data[[i]] <-
    average_marginal_effect(fit[[i]],
                            mrp_data[[i]],
                            save = TRUE)

  strat_ame_data[[i]] <-
    all_strat_ame(fit[[i]],
                  survey_data[[i]],
                  mrp_data[[i]],
                  save = TRUE)
}

save(poststrat, file = here::here("data/all_poststrat.RData"))
save(ame_data, file = here::here("data/all_ame_data.RData"))
save(strat_ame_data, file = here::here("data/all_strat_ame_data.RData"))

########
# plots
########
