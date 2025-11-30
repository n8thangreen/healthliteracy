# MRP analysis using Skill for Life survey 2011 data
# includes England comparison

library(purrr)

refit <- TRUE
use_stan <- TRUE
save_output <- FALSE

# create_target_pop_fn <- create_target_marginal_pop_data  # from tables only (marginal)
create_target_pop_fn <- create_target_pop_data             # from individual resident survey responses (joint)

# raw data
load(here::here("data/skills_for_life_2011_data.RData"))

survey_data <- clean_sfl_data_2011(data2011)

if (refit) {
  fit <- fit_all_models(survey_data, stan = use_stan, year_suffix = "2003")
} else {
  load(here::here("data/fit_2011.RData"))
}

if (save_output) {
  save(fit, file = here::here("data/fit_2011.RData"))
}

# --- Newham specific

newham_census_marginals <- get_newham_census_props()
synth_data_census_newham <-
  create_lfs_synth_data(newham_census_marginals,
                        add_missing = TRUE,
                        smooth_alpha = 0.5)

imd_lookup <- create_imd_lookup(quintile = TRUE)
nrs_prob_data <- create_NRS_prob_data()

props_newham_list <-
  list(synth_data_census_newham,
       nrs_prob_data,
       imd_lookup)

mrp_data_newham <-
  map(
    survey_data,
    ~ create_covariate_data(.x) |>
      combine_all_prop_data(props_newham_list) |>
      create_target_pop_data()
  )

if (save_output) {
  save(mrp_data_newham, file = here::here("data/mrp_data_newham_lfs_2011.RData"))
}

# --- England baseline

# england_joint <- get_national_joint()
england_census_marginals <- get_england_census_props()
synth_data_census_england <-
  create_lfs_synth_data(england_census_marginals,
                        add_missing = TRUE,
                        smooth_alpha = 0.5)

mrp_data_england <-
  map(
    survey_data,
    ~ create_covariate_data(.x) |>
      combine_all_prop_data(synth_data_census_england) |>
      create_target_pop_data()
  )


if (save_output) {
  save(mrp_data_england, file = here::here("data/mrp_data_england_lfs_2011.RData"))
}

###########
# outcomes

out_name <- c("lit", "num", "ict")

ame_data <- list()
ame_data_england <- list()

# poststrat <- list()
# att_data <- list()
# swatt_data <- list()
# strat_ame_data <- list()

for (i in out_name) {

  # poststrat[[i]] <-
  #   poststratification(
  #   fit[[i]],
  #   mrp_data_newham[[i]])

  ame_data[[i]] <-
    average_marginal_effect(
      fit[[i]],
      mrp_data_newham[[i]],
      save = save_output)

  ame_data_england[[i]] <-
    average_marginal_effect(
      fit[[i]],
      mrp_data_england[[i]],
      save = save_output)

  # att_data[[i]] <-
  #   average_effect_on_treatment(
  #     fit[[i]],
  #     mrp_data_newham[[i]],
  #     save = TRUE)
  #
  # # change to swatt?
  # swatt_data[[i]] <-
  #   subpop_weighted_average_effect(
  #     att_data[[i]],
  #     mrp_data_newham[[i]])

  # # slow
  # strat_ame_data[[i]] <-
  #   all_cate(
  #     fit[[i]],
  #     survey_data[[i]],
  #     mrp_data_newham[[i]],
  #     save = TRUE)
}

if (save_output) {
  save(ame_data, file = here::here("data/all_ame_data_2011.RData"))
  save(ame_data_england, file = here::here("data/all_ame_data_2011_england.RData"))

  # save(poststrat, file = here::here("data/all_poststrat.RData"))
  # save(att_data, file = here::here("data/all_att_data.RData"))
  # save(swatt_data, file = here::here("data/all_swatt_data.RData"))
  # save(strat_ame_data, file = here::here("data/all_strat_ame_data.RData"))
  # save(cate_data, file = here::here("data/all_cate_data.RData"))
}

########
# plots
########

library(ggplot2)
library(gridExtra)

load(here::here("data/all_ame_data_2011.RData"))
# load(here::here("data/all_poststrat.RData"))
# load(here::here("data/all_att_data.RData"))
# load(here::here("data/all_swatt_data.RData"))
# load(here::here("data/all_strat_ame_data.RData"))
# load(here::here("data/all_cate_data.RData"))


# bar plots

out <- list()
for (i in names(ame_data)) {
  out[[i]] <- bar_plot(ame_data[[i]], title = i) + ylim(-0.3, 0.3)
}
gridout <- gridExtra::grid.arrange(grobs = out, ncol = 1)

if (save_output) {
  ggsave(gridout, filename = here::here("plots/all_bar_plots_2011.png"),
         width = 5, height = 6, dpi = 300, bg = "white")
}

# scatter plots

title_text <- c(ict = "ICT", lit = "Literacy", num = "Numeracy")

for (i in names(ame_data)) {
  scatter_plot(ame_data[[i]], title = title_text[i], save = T)
}

# ggsave(gridout, filename = here::here("plots/scatter_plots_2011.png"),
#        width = 5, height = 6, dpi = 300, bg = "white")

# AME forest plot ---

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

if (save_output) {
  ggsave(plot = ame_forest, filename = here::here("plots/ame_forest_group_plot_2011.png"),
         width = 9, height = 7, dpi = 300, bg = "white")
}

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

if (save_output) {
  ggsave(plot = att_forest, filename = "plots/att_forest_group_plot_2011.png",
         width = 9, height = 7, dpi = 300, bg = "white")

  ggsave(plot = swate_forest, filename = "plots/swate_forest_group_plot_2011.png",
         width = 9, height = 7, dpi = 300, bg = "white")

  ggsave(plot = gridout, filename = "plots/forest_group_grid_plot_2011.png",
         width = 11, height = 7, dpi = 300, bg = "white")
}

## rank bar plot

for (i in names(ame_data)) {
  rank_plot(ps_var = ame_data[[i]], title = i, save = F)
  rank_plot_by_var(ps_var = ame_data[[i]], title = i, save = F)
}

rank_group_plot(ame_data, max_rank = 3, save = F)
rank_group_plot(att_data, max_rank = 3, save = F)  # error

## cumulative rank plots

for (i in names(ame_data)) {
  cumrank_plot(ps_var = ame_data[[i]], title = title_text[i], save = F)
}

ame_data <- setNames(ame_data, nm = c("Literacy", "Numeracy", "ICT"))

gg <- list()
gg[[2]] <- cumrank_group_plot(att_data, max_rank = 3, threshold = 0.2, abs_val = TRUE, save = F, filename = "att_cumrank_group_plot_2011.png")
gg[[3]] <- cumrank_group_plot(swatt_data, max_rank = 3, threshold = 0.2, abs_val = TRUE, save = F, filename = "swate_cumrank_group_plot_2011.png")

gg[[1]] <- cumrank_group_plot(ame_data, max_rank = 4, threshold = 0.2, abs_val = TRUE, save = F)
gg[[1]]

gg_cumrank_complete <- cumrank_group_plot(ame_data, abs_val = TRUE, save = F)

if (save_output) {
  ggsave(gg_cumrank_complete, filename = here::here("plots/gg_cumrank_complete_2011.png"),
         width = 18, height = 12, dpi = 300, bg = "white")
}

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

if (save_output) {
  ggsave(gridout, filename = here::here("plots/all_cumrank_group_plot_2011.png"),
         width = 10, height = 12, dpi = 300, bg = "white")

  # ATE only
  ggsave(gg[[1]], filename = here::here("plots/ame_cumrank_group_plot_2011.png"),
         width = 12, height = 6, dpi = 300, bg = "white")
}

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

write.csv(tab, here::here("tables/ame_table_2011.csv"), row.names = FALSE)

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

tab_ame <- sucra_table(ame_data, abs_val = TRUE)
tab_att <- sucra_table(att_data, abs_val = TRUE)
tab_swate <- sucra_table(swatt_data, abs_val = TRUE)

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


#############################
# England comparison
##TODO

ame_forest <- ame_forest_group_plot(ame_data, save = F) +
  scale_color_discrete(
    name = "Outcome:",
    labels = c("ict" = "ICT",
               "lit" = "Literacy",
               "num" = "Numeracy")) + ylim(-0.4, 0.3)
ame_forest

combined_engl_newham_forest <-
  create_comparison_forest_plots(ame_data, ame_data_england)

combined_engl_newham_forest

if (save_output) {
  ggsave(combined_engl_newham_forest$lit, filename = here::here("plots/combined_engl_newham_forest_lit.png"),
         width = 9, height = 7, dpi = 300, bg = "white")

  ggsave(combined_engl_newham_forest$num, filename = here::here("plots/combined_engl_newham_forest_num.png"),
         width = 9, height = 7, dpi = 300, bg = "white")

  ggsave(combined_engl_newham_forest$ict, filename = here::here("plots/combined_engl_newham_forest_ict.png"),
         width = 9, height = 7, dpi = 300, bg = "white")
}
