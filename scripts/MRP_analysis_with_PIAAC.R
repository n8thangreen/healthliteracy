# PIAAC data


library(purrr)

if (FALSE) {
  data_PIAAC <-
    read.csv(
      file = here::here(
        "../../../data/PIAAC/prggbrp2.csv"), sep = ";")

  save(data_PIAAC, file = "data/data_PIAAC.RData")
}

refit <- TRUE
use_stan <- FALSE

# raw data
load(here::here("data/data_PIAAC.RData"))

# # labelled, cleaned data
# load(here::here("../../../data/PIAAC/PIAAC_Cycle2.rda"))
# PIAAC_survey_data <- clean_PIAAC_data(PIAAC_Cycle2)

PIAAC_survey_data <- clean_PIAAC_data(data_PIAAC)


if (refit) {
  fit <- fit_models(PIAAC_survey_data, stan = use_stan, file_suffix = "piaac")
} else {
  load(here::here("data/fit_piaac.RData"))
}

mrp_data <-
  map(PIAAC_survey_data,
      ~ create_covariate_data(.x) |>
        create_target_pop_data(additional_prob_data = synth_data)
  )

save(fit, file = here::here("data/fit_piaac.RData"))
save(mrp_data, file = here::here("data/mrp_data_piaac.RData"))
