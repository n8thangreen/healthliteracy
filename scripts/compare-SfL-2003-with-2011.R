# compare SfL 2011 with SfL 2003


library(purrr)

refit <- FALSE
use_stan <- TRUE

# SfL 2011 data

load(here::here("data/skills_for_life_data.RData"))

survey_data2011 <- clean_sfl_data(data)

if (refit) {
  fit2011 <- fit_models(survey_data2011, stan = use_stan)
} else {
  load(here::here("data/fit2011.RData"))
}

# SfL 2003 data

# data2003 <-
#   haven::read_dta(
#     file = "../../../Newham Council Fellowship/data/skills for life survey/2003/UKDA-7239-stata9/stata9/2003_skills_for_life_datafile_anonymised.dta")
#
# save(data2003, file = "data/skills_for_life_2003_data.RData")

load(here::here("data/skills_for_life_2003_data.RData"))

survey_data2003 <- clean_sfl_data_2003(data2003)

##TODO: theres no ICT data in SfL 2003. separate fit function?
if (refit) {
  fit20003 <- fit_models(survey_data2003, stan = use_stan)
} else {
  load(here::here("data/fit2003.RData"))
}

