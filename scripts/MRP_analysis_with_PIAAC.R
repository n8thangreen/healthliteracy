# MRP analysis with PIAAC data


library(purrr)

if (FALSE) {
  data_PIAAC <-
    read.csv(
      file = here::here(
        "../../../data/PIAAC/prggbrp2.csv"), sep = ";")

  save(data_PIAAC, file = "data/data_PIAAC.RData")
}

refit <- TRUE
use_stan <- TRUE

# raw data
load(here::here("data/data_PIAAC.RData"))

# # labelled, cleaned data
# load(here::here("../../../data/PIAAC/PIAAC_Cycle2.rda"))
# PIAAC_survey_data <- clean_PIAAC_data(PIAAC_Cycle2)

PIAAC_survey_data <- clean_PIAAC_data(data_PIAAC)

if (refit) {
  fit <- fit_all_models(PIAAC_survey_data, stan = use_stan, year_suffix = "piaac")
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

# --- outcomes

out_name <- c("lit", "num")

ame_data <- list()

for (i in out_name) {
  ame_data[[i]] <-
    average_marginal_effect(
      fit[[i]],
      mrp_data[[i]],
      save = TRUE)
}

save(ame_data, file = here::here("data/all_ame_data_piaas.RData"))

########
# plots
########

library(ggplot2)
library(gridExtra)

load(here::here("data/all_ame_data_piaas.RData"))

# scatter plots

title_text <- c(lit = "Literacy", num = "Numeracy")

for (i in names(ame_data)) {
  scatter_plot(ame_data[[i]], title = title_text[i], save = F)
}

# ggsave(gridout, filename = here::here("plots/scatter_plots_piass.png"),
#        width = 5, height = 6, dpi = 300, bg = "white")

# AME forest plot

gg <- list()
for (i in names(ame_data)) {
  gg[[i]] <- ame_forest_plot(ame_data[[i]], title = title_text[i], save = F)
}
gg[[1]]
gg[[2]]

ame_forest <- ame_forest_group_plot(ame_data, save = F) +
  scale_fill_discrete( # Use scale_fill_discrete instead
    name = "Outcome:",
    labels = c("lit" = "Literacy",
               "num" = "Numeracy")
  )
ame_forest

ggsave(plot = ame_forest,
       filename = here::here("plots/ame_forest_group_plot_piaas.png"),
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

ame_data <- setNames(ame_data, nm = c("Literacy", "Numeracy"))

gg <- list()

gg_cumrank <- cumrank_group_plot(ame_data, max_rank = 4,
                                 threshold = 0.2,
                                 abs_val = TRUE, save = F)
gg_cumrank

gg_cumrank_complete <- cumrank_group_plot(ame_data, abs_val = TRUE, save = F)

gg_cumrank_complete

ggsave(gg_cumrank_complete,
       filename = here::here("plots/gg_cumrank_complete_piaas.png"),
       width = 18, height = 12, dpi = 300, bg = "white")

ggsave(gg_cumrank,
       filename = here::here("plots/ame_cumrank_group_plot_piass.png"),
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

write.csv(tab, file = here::here("tables/ame_table_piaas.csv"), row.names = FALSE)

indent_rows <- grep("^  ", tab$variable)

tab %>%
  mutate(across(everything(), ~ replace_na(.x, ""))) |>
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Variable", "Literacy", "Numeracy"),
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
outcomes literacy and numeracy. \\label{tab:sucra-ate}",
        col.names = c("Variable", "Category",
                      "Literacy", "Numeracy",
                      "Literacy", "Numeracy")) |>
  kable_styling(latex_options = c("hold_position")) |>
  add_header_above(c(" " = 2, "SUCRA" = 3, "E[rank]" = 3)) |>
  row_spec(0, bold = TRUE)


