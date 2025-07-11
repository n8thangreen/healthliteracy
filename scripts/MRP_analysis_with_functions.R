# MRP analysis using functions
# refactored to use in the shiny app


library(purrr)

refit <- TRUE
use_stan <- TRUE

# create_target_pop_fn <- create_target_marginal_pop_data  # from tables only (marginal)
create_target_pop_fn <- create_target_pop_data             # from individual resident survey responses (joint)

# raw data
load(here::here("data/skills_for_life_data.RData"))

# IPF data
load(here::here("data/synth_data.rda"))  # create_lfs_synth_data()

survey_data <- clean_sfl_data(data)

if (refit) {
  fit <- fit_models(survey_data, stan = use_stan)
} else {
  load(here::here("data/fit.RData"))
}

mrp_data <-
  map(survey_data,
      ~ create_covariate_data(.x) |>
        # create_target_pop_data(additional_prob_data = demo_prop_tables())               # ONS marginals
        create_target_pop_data(additional_prob_data = synth_data)                      # LFS with ONS
)

save(fit, file = here::here("data/fit.RData"))
# save(mrp_data, file = here::here("data/mrp_data_ons.RData"))
save(mrp_data, file = here::here("data/mrp_data_lfs.RData"))

###########
# outcomes

out_name <- c("lit", "num", "ict")

poststrat <- list()
ame_data <- list()
att_data <- list()
swatt_data <- list()
strat_ame_data <- list()

for (i in out_name) {

  # poststrat[[i]] <-
  #   poststratification(
  #   fit[[i]],
  #   mrp_data[[i]])

  ame_data[[i]] <-
    average_marginal_effect(
      fit[[i]],
      mrp_data[[i]],
      save = TRUE)

  # att_data[[i]] <-
  #   average_effect_on_treatment(
  #     fit[[i]],
  #     mrp_data[[i]],
  #     save = TRUE)
  #
  # # change to swatt?
  # swatt_data[[i]] <-
  #   subpop_weighted_average_effect(
  #     att_data[[i]],
  #     mrp_data[[i]])

  # # slow
  # strat_ame_data[[i]] <-
  #   all_cate(
  #     fit[[i]],
  #     survey_data[[i]],
  #     mrp_data[[i]],
  #     save = TRUE)
}

save(poststrat, file = here::here("data/all_poststrat.RData"))
save(ame_data, file = here::here("data/all_ame_data.RData"))
save(att_data, file = here::here("data/all_att_data.RData"))
save(swatt_data, file = here::here("data/all_swatt_data.RData"))
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
load(here::here("data/all_swatt_data.RData"))
load(here::here("data/all_strat_ame_data.RData"))
# load(here::here("data/all_cate_data.RData"))


# bar plots

out <- list()
for (i in names(ame_data)) {
  out[[i]] <- bar_plot(ame_data[[i]], title = i) + ylim(-0.3, 0.3)
}
gridout <- gridExtra::grid.arrange(grobs = out, ncol = 1)

ggsave(gridout, filename = here::here("plots/all_bar_plots.png"),
       width = 5, height = 6, dpi = 300, bg = "white")

# scatter plots

title_text <- c(ict = "ICT", lit = "Literacy", num = "Numeracy")
for (i in names(ame_data)) {
  scatter_plot(ame_data[[i]], title = title_text[i], save = T)
}

# AME forest plot

for (i in names(ame_data)) {
  ame_forest_plot(ame_data[[i]], title = title_text[i], save = F)
}

ame_forest <- ame_forest_group_plot(ame_data, save = F) +
  scale_color_discrete(
    name = "Outcome:",
    labels = c("ict" = "ICT",
               "lit" = "Literacy",
               "num" = "Numeracy"))
ame_forest

ggsave(plot = ame_forest, filename = here::here("plots/ame_forest_group_plot.png"),
       width = 9, height = 7, dpi = 300, bg = "white")

att_forest <-
  ame_forest_group_plot(att_data, save = F) +
  ylab("Average treatment effect on treated") +
  scale_color_discrete(
    name = "Outcome:",
    labels = c("ict" = "ICT",
               "lit" = "Literacy",
               "num" = "Numeracy"))
att_forest

swatt_ <- swatt_data
swatt_$lit$imd <- NULL
swatt_$num$imd <- NULL
swatt_$ict$imd <- NULL

swate_forest <-
  swatt_ |>
  ame_forest_group_plot(save = F) +
  ylab("Subpopulation weighted average treatment effect") +
  scale_color_discrete(
  name = "Outcome:",
  labels = c("ict" = "ICT",
             "lit" = "Literacy",
             "num" = "Numeracy"))
swate_forest

# IMD only

swatt_ <- list()
swatt_$lit <- list()
swatt_$lit$imd <- swatt_data$lit$imd
swatt_$num$imd <- swatt_data$num$imd
swatt_$ict$imd <- swatt_data$ict$imd

swate_forest_imd <-
  swatt_ |>
  ame_forest_group_plot(save = F) +
  ylab("Subpopulation weighted average treatment effect") +
  scale_color_discrete(
    name = "Type",
    labels = c("ict" = "ICT",
               "lit" = "Literacy",
               "num" = "Numeracy"))
swate_forest_imd

# combine plots with a shared legend
gridout <- cowplot::plot_grid(
  cowplot::plot_grid(att_forest, swate_forest, ncol = 2, labels = c("a)", "b)")),
  # legend,
  ncol = 1
  # ncol = 2,
  # rel_widths = c(1, 0.2)
)
gridout

ggsave(plot = ame_forest, filename = "plots/ame_forest_group_plot.png",
       width = 9, height = 7, dpi = 300, bg = "white")

ggsave(plot = att_forest, filename = "plots/att_forest_group_plot.png",
       width = 9, height = 7, dpi = 300, bg = "white")

ggsave(plot = swate_forest, filename = "plots/swate_forest_group_plot.png",
       width = 9, height = 7, dpi = 300, bg = "white")

ggsave(plot = gridout, filename = "plots/forest_group_grid_plot.png",
       width = 11, height = 7, dpi = 300, bg = "white")

## rank bar plot

for (i in names(ame_data)) {
  rank_plot(ps_var = ame_data[[i]], title = i, save = F)
  rank_plot_by_var(ps_var = ame_data[[i]], title = i, save = F)
}

rank_group_plot(ame_data, max_rank = 3, save = F)
rank_group_plot(att_data, max_rank = 3, save = F)  # error

## sucra plot

for (i in names(ame_data)) {
  sucra_plot(ps_var = ame_data[[i]], title = title_text[i], save = F)
}

ame_data <- setNames(ame_data, nm = c("Literacy", "Numeracy", "ICT"))

gg <- list()
gg[[1]] <- sucra_group_plot(ame_data, max_rank = 4, threshold = 0.2, abs_val = TRUE, save = F)
gg[[2]] <- sucra_group_plot(att_data, max_rank = 3, threshold = 0.2, abs_val = TRUE, save = F, filename = "att_sucra_group_plot.png")
gg[[3]] <- sucra_group_plot(swatt_data, max_rank = 3, threshold = 0.2, abs_val = TRUE, save = F, filename = "swate_sucra_group_plot.png")

# extract common legend
legend <- cowplot::get_legend(gg[[1]])

# remove legends
p1_no_legend <- gg[[1]] + theme(legend.position = "none")
p2_no_legend <- gg[[2]] + theme(legend.position = "none")
p3_no_legend <- gg[[3]] + theme(legend.position = "none")

# combine plots with a shared legend
gridout <- cowplot::plot_grid(
  cowplot::plot_grid(p1_no_legend, p2_no_legend, p3_no_legend, ncol = 1, labels = c("a)", "b)", "c)")),
  legend,
  ncol = 2,
  rel_widths = c(1, 0.2)
)
gridout

ggsave(gridout, filename = here::here("plots/all_sucra_group_plot.png"),
       width = 10, height = 12, dpi = 300, bg = "white")

ggsave(gg[[1]], filename = here::here("plots/ame_sucra_group_plot.png"),
       width = 12, height = 6, dpi = 300, bg = "white")



#########
# tables
#########

library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)

# ame table
tab <- ame_data |>
  ame_table() |>
  clean_names("variable")

write.csv(tab, here::here("tables/ame_table.csv"), row.names = FALSE)

indent_rows <- grep("^  ", tab$variable)

tab %>%
  mutate(across(everything(), ~ replace_na(.x, ""))) |>
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Variable", "Literacy", "Numeracy", "ICT"),
        linesep = "") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  row_spec(0, bold = TRUE) |>
  add_indent(positions = indent_rows, level_of_indent = 1, target_cols = 1)


## sucra table

tab_ame <- sucra_table(ame_data, max_rank = 3, threshold = 0.2, abs_val = TRUE)
tab_att <- sucra_table(att_data, max_rank = 3, threshold = 0.2, abs_val = TRUE)
tab_swate <- sucra_table(swatt_data, max_rank = 3, threshold = 0.2, abs_val = TRUE)

tab_ame |>
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
      # align = c("l", "l", "r", "r", "r"),
        caption = "SUCRA and expected rank using the average treatment effect for the health literacy
outcomes ICT, literacy and numeracy. \\label{tab:sucra-ate}",
        col.names = c("Variable", "Category", "ICT", "Literacy", "Numeracy", "ICT", "Literacy", "Numeracy")) |>
  kable_styling(latex_options = c("hold_position")) |>
  add_header_above(c(" " = 2, "SUCRA" = 3, "E[rank]" = 3)) |>
  row_spec(0, bold = TRUE)

tab_att |>
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
      # align = c("l", "l", "r", "r", "r"),
        caption = "SUCRA and expected rank using the average treatment on treated effect for the health literacy
outcomes ICT, literacy and numeracy. \\label{tab:}",
        col.names = c("Variable", "Category", "ICT", "Literacy", "Numeracy", "ICT", "Literacy", "Numeracy")) |>
  kable_styling(latex_options = c("hold_position")) |>
  add_header_above(c(" " = 2, "SUCRA" = 3, "E[rank]" = 3)) |>
  row_spec(0, bold = TRUE)

tab_swate |>
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
      # align = c("l", "l", "r", "r", "r"),
        caption = "SUCRA and expected rank using the subpopulation weighted average treatment effect for the health literacy
outcomes ICT, literacy and numeracy. \\label{tab:}",
        col.names = c("Variable", "Category", "ICT", "Literacy", "Numeracy", "ICT", "Literacy", "Numeracy")) |>
  kable_styling(latex_options = c("hold_position")) |>
  add_header_above(c(" " = 2, "SUCRA" = 3, "E[rank]" = 3)) |>
  row_spec(0, bold = TRUE)


