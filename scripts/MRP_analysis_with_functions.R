# MRP analysis using functions
# simplified to use in the shiny app


library(purrr)

load(here::here("data/skills_for_life_data.RData"))

survey_data <- clean_data(data)

use_stan <- TRUE

fit <- fit_models(survey_data, stan = use_stan)

mrp_data <-
  map(survey_data,
      ~ create_covariate_data(.x) |>
        create_target_pop_data())

save(mrp_data, file = here::here("data/mrp_data.RData"))
save(fit, file = here::here("data/fit.RData"))

###########
# outcomes

out_name <- c("lit", "num", "ict")

poststrat <- list()
ame_data <- list()
att_data <- list()
strat_ame_data <- list()

for (i in out_name) {

  # # poststrat[[i]] <-
  # #   poststratification(fit[[i]],
  # #                      mrp_data[[i]])
  # #
  # ame_data[[i]] <-
  #   average_marginal_effect(fit[[i]],
  #                           mrp_data[[i]],
  #                           save = TRUE)

  att_data[[i]] <-
    average_effect_on_treatment(
      fit[[i]],
      mrp_data[[i]],
      save = TRUE)

  swate_data[[i]] <-
    subpop_weighted_average_effect(
      att_data[[i]],
      mrp_data[[i]],
      save = TRUE)
  #
  #   strat_ame_data[[i]] <-
  #     all_cate(fit[[i]],
  #               survey_data[[i]],
  #               mrp_data[[i]],
  #               save = TRUE)
}

save(poststrat, file = here::here("data/all_poststrat.RData"))
save(ame_data, file = here::here("data/all_ame_data.RData"))
save(att_data, file = here::here("data/all_att_data.RData"))
save(strat_ame_data, file = here::here("data/all_strat_ame_data.RData"))
# save(cate_data, file = here::here("data/all_cate_data.RData"))

########
# plots
########

library(ggplot2)
library(gridExtra)

load(here::here("data/all_poststrat.RData"))
load(here::here("data/all_ame_data.RData"))
load(here::here("data/all_att_data.RData"))
load(here::here("data/all_strat_ame_data.RData"))
# load(here::here("data/all_cate_data.RData"))

# bar plots

out <- list()
for (i in names(ame_data)) {
  out[[i]] <- bar_plot(ame_data[[i]], title = i)
}
gridout <- gridExtra::grid.arrange(grobs = out, ncol = 1)

ggsave(gridout, filename = here::here("plots/all_bar_plots.png"),
       width = 5, height = 6, dpi = 300, bg = "white")

# scatter plots

for (i in names(ame_data)) {
  scatter_plot(ame_data[[i]], title = i, save = TRUE)
}

# AME forest plot

for (i in names(ame_data)) {
  ame_forest_plot(ame_data[[i]], title = i, save = TRUE)
}

ame_forest_group_plot(ame_data, save = F)
ame_forest_group_plot(att_data, save = F, filename = "att_forest_group_plot.png")
ame_forest_group_plot(swate_data, save = F, filename = "swate_forest_group_plot.png")

# rank plot

for (i in names(ame_data)) {
  rank_plot(ps_var = ame_data[[i]], title = i, save = TRUE)
  rank_plot_by_var(ps_var = ame_data[[i]], title = i, save = TRUE)
}

rank_group_plot(ame_data, max_rank = 3, save = TRUE)
rank_group_plot(att_data, max_rank = 3, save = F)

# sucra plot

for (i in names(ame_data)) {
  sucra_plot(ps_var = ame_data[[i]], title = i, save = TRUE)
}

sucra_group_plot(ame_data, max_rank = 3, threshold = 0.2, abs_val = TRUE, save = TRUE)
sucra_group_plot(att_data, max_rank = 3, threshold = 0.2, abs_val = TRUE, save = F)


#########
# tables
#########

library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)

# ame table
tab <- ame_table(ame_data)
write.csv(tab, here::here("tables/ame_table.csv"), row.names = FALSE)

tab %>%
  mutate(across(everything(), ~ replace_na(.x, ""))) |>
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Variable", "Literacy", "Numeracy", "ICT")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  row_spec(0, bold = TRUE)

# sucra table

##TODO: subset
##      add expected rank
##      combine and transpose

tab <- sucra_table(ame_data, max_rank = 3, threshold = 0.2, abs_val = TRUE)

write.csv(tab, here::here("tables/sucra_table.csv"), row.names = FALSE)

