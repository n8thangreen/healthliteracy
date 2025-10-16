# PIAAC data


library(purrr)

if (FALSE) {
  data_PIAAC <-
    read.csv(
      file = here::here(
        "../../../data/PIAAC/prggbrp2.csv"), sep = ";")

  save(data_PIAAC, file = "data/data_PIAAC.RData")
}

use_stan <- FALSE

# raw data
load(here::here("data/data_PIAAC.RData"))

# labelled data
load(here::here("../../../data/PIAAC/PIAAC_Cycle2.rda"))

PIAAC_survey_data <- clean_PIAAC_data(data_PIAAC)

#
fit_PIAAC <- fit_models(PIAAC_survey_data, stan = use_stan)
