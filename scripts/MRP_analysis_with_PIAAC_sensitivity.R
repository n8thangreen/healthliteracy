# MRP analysis with PIAAC data using imputed data

library(dplyr)
library(mice)
library(here)
library(parallel)
library(purrr)

# save compile brms models
rstan::rstan_options(auto_write = TRUE)
# options(brms.backend = "cmdstanr")
options(mc.cores = parallel::detectCores() - 1)

save_output <- FALSE

noise_sd <- 1
include_outcome <- TRUE

run_name <- if (!include_outcome) {
  "no_outcome"
} else if (noise_sd == 0) {
  "outcome"
} else {
  glue::glue("diluted_{noise_sd}")
}

covariate_names <- c(
  "workingstatus", "gross_income", "uk_born", "sex", "own_home",
  "age", "english_lang", "ethnicity", "qualification", "imd", "job_status"
)

# Number of imputations
m <- 20

# Load raw data
load(here::here("data/data_PIAAC.RData"))
load(here::here("data/skills_for_life_2011_data.RData"))

PIAAC_survey_data <- clean_PIAAC_data(data_PIAAC)
SfL2011_survey_data <- clean_sfl_data_2011(data2011)

domains_to_process <- c("lit", "num")

imputed_piaac_data_list <-
  lapply(domains_to_process, function(domain) {
    prepare_imputed_domain_par(
      domain_name = domain,
      piaac_data = PIAAC_survey_data,
      sfl_data = SfL2011_survey_data,
      all_covariates = covariate_names,
      m_imputations = m,
      include_outcome = include_outcome,
      noise_sd = noise_sd
    )
  })

names(imputed_piaac_data_list) <- domains_to_process

if (save_output) {
  save(imputed_piaac_data_list, file = here::here(
    glue::glue("data/imputed_piaac_data_list_{run_name}.RData")))
}

# model fit

load(here::here(
  glue::glue("data/imputed_piaac_data_list_{run_name}.RData")))

# options(mc.cores = 1)  # fix sink() invalid connection error

refit <- TRUE

if (refit) {
  fit <- fit_all_models(imputed_piaac_data_list,
                        stan = TRUE,             # over-ridden by mice mids
                        year_suffix = "2011",    # determines set of covariates
                        algorithm = "meanfield")
} else {
  load(here::here(glue::glue("data/fit_piaac_imp_{run_name}.RData")))
}

if (save_output) {
  save(fit, file = here::here(glue::glue("data/fit_piaac_imp_{run_name}.RData")))
}

mrp_data <-
  map(imputed_piaac_data_list,
      ~ create_covariate_data(.x) |>
        create_target_pop_data(additional_prob_data = synth_data)
  )

if (save_output) {
  save(mrp_data,
       file = here::here(glue::glue("data/mrp_data_piaac_imp_{run_name}.RData")))
}

# --- outcomes

out_name <- c("lit", "num")

ame_data <- list()

for (i in out_name) {
  ame_data[[i]] <-
    average_marginal_effect(
      fit[[i]],
      mrp_data[[i]],
      save = FALSE,
      ndraws = 20)
}

if (save_output) {
  save(ame_data,
       file = here::here(glue::glue(
         "data/all_ame_data_piaac_imp_{run_name}.RData")))
}

########
# plots
########

library(ggplot2)
library(gridExtra)

load(here::here(glue::glue("data/all_ame_data_piaac_imp_{run_name}.RData")))

# --- scatter plots

# title_text <- c(lit = "Literacy", num = "Numeracy")

for (i in names(ame_data)) {
  scatter_plot(ame_data[[i]],
               save = save_output,
               title = glue::glue("{i}_piaac_imp"))
}


# --- AME forest plot

gg <- list()
for (i in names(ame_data)) {
  gg[[i]] <- ame_forest_plot(ame_data[[i]], title = i, save = save_output)
}

gg[[1]] #+ ylim(-.2,.2)
gg[[2]] #+ ylim(-.2,.2)

ame_forest <-
  ame_forest_group_plot(ame_data, save = save_output) +
  scale_fill_discrete( # Use scale_fill_discrete instead
    name = "Outcome:",
    labels = c("lit" = "Literacy",
               "num" = "Numeracy")
  )
ame_forest

ggsave(plot = ame_forest,
       filename = here::here(glue::glue(
         "plots/ame_forest_group_plot_piaac_imp_{run_name}.png")),
       width = 9, height = 7, dpi = 300, bg = "white")

## --- rank bar plot

for (i in names(ame_data)) {
  rank_plot(ps_var = ame_data[[i]], title = i, save = save_output)
  rank_plot_by_var(ps_var = ame_data[[i]], title = i, save = save_output)
}

rank_group_plot(ame_data, max_rank = 3, save = save_output)

## --- cumulative rank plots

for (i in names(ame_data)) {
  cumrank_plot(ps_var = ame_data[[i]], title = i, save = save_output)
}

ame_data <- setNames(ame_data, nm = c("Literacy", "Numeracy"))

gg <- list()

gg_cumrank <- cumrank_group_plot(ame_data, max_rank = 4,
                                 threshold = 0.2,
                                 abs_val = TRUE,
                                 save = save_output)
gg_cumrank

gg_cumrank_complete <-
  cumrank_group_plot(ame_data, abs_val = TRUE, save = save_output)

gg_cumrank_complete

ggsave(gg_cumrank_complete,
       filename = here::here(glue::glue(
         "plots/gg_cumrank_complete_piaac_imp_{run_name}.png")),
       width = 18, height = 12, dpi = 300, bg = "white")

ggsave(gg_cumrank,
       filename = here::here(glue::glue(
         "plots/ame_cumrank_group_plot_piaac_imp_{run_name}.png")),
       width = 12, height = 6, dpi = 300, bg = "white")
