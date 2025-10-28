# MRP analysis with PIAAC data using imputed data

library(dplyr)
library(mice)
library(here)

# ave compile brms models
rstan::rstan_options(auto_write = TRUE)

options(mc.cores = parallel::detectCores())


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

#' Prepares a combined and imputed 'mids' object for a specific skill domain.
#'
#' @param domain_name The name of the skill domain (e.g., "lit", "num") as a string.
#' @param piaac_data The cleaned PIAAC_survey_data (a list of data frames).
#' @param sfl_data The cleaned SfL2011_survey_data (a list of data frames).
#' @param all_covariates A character vector of all covariate names.
#' @param m_imputations The number of imputations (e.g., 5).
#' @return A 'mids' object.
#'
prepare_imputed_domain <- function(domain_name, piaac_data, sfl_data,
                                   all_covariates, m_imputations) {

  # 1. Combine the two surveys for the *specific domain*
  combined_data <- dplyr::bind_rows(
    list(piaac = piaac_data[[domain_name]],
         sfl2011 = sfl_data[[domain_name]]),
    .id = "survey"
  )

  # 2. Ensure all covariates from the master list are present
  # (Adds any missing ones as columns full of NA)
  missing_covs <- setdiff(all_covariates, names(combined_data))

  if (length(missing_covs) > 0) {
    combined_data[missing_covs] <- NA
  }

  # 3. Separate covariate data (for imputation)
  covariate_data <- combined_data %>%
    select(all_of(all_covariates))

  # 4. Separate "other" data (to be added back)
  other_col_names <- setdiff(names(combined_data), all_covariates)

  other_data <- combined_data %>%
    select(all_of(other_col_names))

  # --- Imputation ---
  imp <- mice(covariate_data, m = m_imputations, print = FALSE)

  # 6. Add back the "other" data (outcome, id, survey)
  full_imp_object <- mice::cbind(imp, other_data)

  # PIAAC only
  imp_object_piaac <- full_imp_object %>%
    filter(survey == "piaac")

  return(imp_object_piaac)
}

domains_to_process <- c("lit", "num")

imputed_piaac_data_list <-
  lapply(domains_to_process, function(domain) {
    prepare_imputed_domain(
      domain_name = domain,
      piaac_data = PIAAC_survey_data,
      sfl_data = SfL2011_survey_data,
      all_covariates = covariate_names,
      m_imputations = m
    )
  })

names(imputed_piaac_data_list) <- domains_to_process

if (refit) {
  fit <- fit_all_models(imputed_piaac_data_list,
                        stan = use_stan,         # over-ridden by mice mids
                        year_suffix = "2011")    # determines set of covariates
  # backend = "cmdstanr" ##TODO
} else {
  load(here::here("data/fit_piaac_imp.RData"))
}

mrp_data <-
  map(imputed_piaac_data_list,
      ~ create_covariate_data(.x) |>
        create_target_pop_data(additional_prob_data = synth_data)
  )

save(imputed_piaac_data_list, file = here::here("data/imputed_piaac_data_list.RData"))
save(fit, file = here::here("data/fit_piaac_imp.RData"))
save(mrp_data, file = here::here("data/mrp_data_piaac_imp.RData"))

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

save(ame_data, file = here::here("data/all_ame_data_piaas_imp.RData"))


########
# plots
########

library(ggplot2)
library(gridExtra)

load(here::here("data/all_ame_data_piaas_imp.RData"))

# scatter plots

title_text <- c(lit = "Literacy", num = "Numeracy")

for (i in names(ame_data)) {
  scatter_plot(ame_data[[i]], title = title_text[i], save = F)
}

# ggsave(gridout, filename = here::here("plots/scatter_plots_piass.png"),
#        width = 5, height = 6, dpi = 300, bg = "white")

# --- AME forest plot

gg <- list()
for (i in names(ame_data)) {
  gg[[i]] <- ame_forest_plot(ame_data[[i]], title = title_text[i], save = F)
}
gg[[1]] + ylim(-.2,.2)
gg[[2]] + ylim(-.2,.2)

ame_forest <- ame_forest_group_plot(ame_data, save = F) +
  scale_fill_discrete( # Use scale_fill_discrete instead
    name = "Outcome:",
    labels = c("lit" = "Literacy",
               "num" = "Numeracy")
  )
ame_forest

ggsave(plot = ame_forest,
       filename = here::here("plots/ame_forest_group_plot_piaas_imp.png"),
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
       filename = here::here("plots/gg_cumrank_complete_piaas_imp.png"),
       width = 18, height = 12, dpi = 300, bg = "white")

ggsave(gg_cumrank,
       filename = here::here("plots/ame_cumrank_group_plot_piass_imp.png"),
       width = 12, height = 6, dpi = 300, bg = "white")
