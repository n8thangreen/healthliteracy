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

###########
# outcomes

out_name <- c("lit", "num", "ict")

poststrat <- list()
ame_data <- list()
strat_ame_data <- list()

for (i in out_name) {

  poststat[[i]] <-
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

save(poststat, file = here::here("data/all_poststrat.RData"))
save(ame_data, file = here::here("data/all_ame_data.RData"))
save(strat_ame_data, file = here::here("data/all_strat_ame_data.RData"))

########
# plots
########

library(ggplot2)
library(gridExtra)

load(here::here("data/all_poststrat.RData"))
load(here::here("data/all_ame_data.RData"))
load(here::here("data/all_strat_ame_data.RData"))

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

ame_forest_group_plot(ame_data, save = TRUE)

# rank plot

for (i in names(ame_data)) {
  rank_plot(ps_var = ame_data[[i]], title = i, save = TRUE)
  rank_plot_by_var(ps_var = ame_data[[i]], title = i, save = TRUE)
}

rank_group_plot(ame_data, max_rank = 3, save = TRUE)

# sucra plot

for (i in names(ame_data)) {
  sucra_plot(ps_var = ame_data[[i]], title = i, save = TRUE)
}

sucra_group_plot(ame_data, max_rank = 3, save = TRUE)


#########
# tables
#########

tab <- ame_table(ame_data)
write.csv(tab, here::here("tables/ame_table.csv"), row.names = FALSE)


