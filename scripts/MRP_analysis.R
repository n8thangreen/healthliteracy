# multilevel regression and post-stratification analysis

library(tidyverse)
library(ggrepel)

# load regression data
data <-
  haven::read_dta(
    "C:/Users/Nathan/Documents/Newham Fellowship/data/Skills for Life Survey 2011/UKDA-7240-tab/stata9/2011_skills_for_life_datafile_anonymised.dta")

save(data, file = "data/skills_for_life_data.RData")


