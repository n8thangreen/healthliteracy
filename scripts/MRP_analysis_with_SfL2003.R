# MRP analysis using Skill for Life survey 2003 data

library(purrr)

refit <- T
use_stan <- TRUE

create_target_pop_fn <- create_target_pop_data             # from individual resident survey responses (joint)

# raw data
load(here::here("data/skills_for_life_2003_data.RData"))

# IPF data
load(here::here("data/synth_data.rda"))  # create_lfs_synth_data()

survey_data <- clean_sfl_data_2003(data2003)

if (refit) {
  fit <- fit_models(survey_data, stan = use_stan, file_suffix = "2003")
} else {
  load(here::here("data/fit_2003.RData"))
}

mrp_data <-
  map(survey_data,
      ~ create_covariate_data(.x) |>
        create_target_pop_data(additional_prob_data = synth_data)        # LFS with ONS
)

save(fit, file = here::here("data/fit_2003.RData"))
save(mrp_data, file = here::here("data/mrp_data_lfs_2003.RData"))

###########
# outcomes

out_name <- c("lit", "num", "ict")

ame_data <- list()

for (i in out_name) {
  ame_data[[i]] <-
    average_marginal_effect(
      fit[[i]],
      mrp_data[[i]],
      save = TRUE)
}

save(ame_data, file = here::here("data/all_ame_data_2003.RData"))

########
# plots
########

library(ggplot2)
library(gridExtra)

load(here::here("data/all_ame_data_2003.RData"))

# bar plots

out <- list()

for (i in names(ame_data)) {
  out[[i]] <- bar_plot(ame_data[[i]], title = i) + ylim(-0.3, 0.3)
}
gridout <- gridExtra::grid.arrange(grobs = out, ncol = 1)

ggsave(gridout, filename = here::here("plots/all_bar_plots_2003.png"),
       width = 5, height = 6, dpi = 300, bg = "white")

# scatter plots

title_text <- c(ict = "ICT", lit = "Literacy", num = "Numeracy")

for (i in names(ame_data)) {
  scatter_plot(ame_data[[i]], title = title_text[i], save = T)
}

# ggsave(gridout, filename = here::here("plots/scatter_plots_2003.png"),
#        width = 5, height = 6, dpi = 300, bg = "white")

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

ggsave(plot = ame_forest,
       filename = here::here("plots/ame_forest_group_plot_2003.png"),
       width = 9, height = 7, dpi = 300, bg = "white")

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

gg_cumrank <- cumrank_group_plot(ame_data, max_rank = 4,
                              threshold = 0.2,
                              abs_val = TRUE, save = F)
gg_cumrank

gg_cumrank_complete <- cumrank_group_plot(ame_data, abs_val = TRUE, save = F)

gg_cumrank_complete

ggsave(gg_cumrank_complete,
       filename = here::here("plots/gg_cumrank_complete_2003.png"),
       width = 18, height = 12, dpi = 300, bg = "white")

ggsave(gg_cumrank,
       filename = here::here("plots/ame_cumrank_group_plot_2003.png"),
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

write.csv(tab, file = here::here("tables/ame_table_2003.csv"), row.names = FALSE)

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

tab_ame |>
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
      # align = c("l", "l", "r", "r", "r"),
        caption =
        "SUCRA and expected rank using the average treatment effect for the health literacy
outcomes ICT, literacy and numeracy. \\label{tab:sucra-ate}",
      col.names = c("Variable", "Category",
                    "ICT", "Literacy", "Numeracy",
                    "ICT", "Literacy", "Numeracy")) |>
  kable_styling(latex_options = c("hold_position")) |>
  add_header_above(c(" " = 2, "SUCRA" = 3, "E[rank]" = 3)) |>
  row_spec(0, bold = TRUE)

