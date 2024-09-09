1# multilevel regression and post-stratification analysis
# with skills for life survey data and
# Newham resident survey data

library(dplyr)
library(glue)
library(gtsummary)

# # load regression data
# {
#   data <-
#     haven::read_dta(
#       file = "C:/Users/Nathan/Documents/Newham Fellowship/data/Skills for Life Survey 2011/UKDA-7240-tab/stata9/2011_skills_for_life_datafile_anonymised.dta")
#
#   save(data, file = "data/skills_for_life_data.RData")
# }


load(here::here("data/skills_for_life_data.RData"))


################
# data cleaning

# select variables

data <-
  data |>
  select(
    WORKINGSTATUS2,
    GROSS_ANNUAL_INCOME_OLDBANDS,
    BUK,
    QxTenu1,
    Sex1,
    AGE1NET,
    Sesol,              # is English first language
    ETHNICSIMPLE,
    HIQUAL,
    CLITSPEAK,          # ENFL everyday English skills (literacy and speaking)
    IMDSCOREB4,         # Index of Multiple Deprivation banded into deciles
    NSSEC7,
    # outcomes
    SUMMARYCOMP,        # self-assessed computer skills (summary)
    TSKILLA,            # self-assessed computer skills (summary 2)
    COMBLIT,            # self-assessed reading a writing (summary)
    LiteracyScoreA_1,                  # literacy level
    starts_with("LiteracyThreshold"),  # literacy threshold
    NumeracyScoreA_1,                  # numeracy level
    starts_with("NumeracyThreshold"),  # numeracy threshold,
    MultipleChoiceLevelA_1,            # ICT level
    MultipleChoiceLevelA_1Thres,       # ICT threshold
    # weights
    rimweight2003,
    rimweightLIT2003,
    rimweightNUM2003,
    rimweightICT2003,
    rimweightNUMICT2003,
    rimweightLITICT2003,
    rimweightLITNUM2003
  )


# these are the S4L variables used in
# Rowlands (2015) British Journal of General Practice
#
# job status: National Statistics Socioeconomic Classification 3 bands
# (Managerial/professional, Intermediate, Routine/manual/ students/unemployed)
# employment status: employed, not employed
# gross income: >=10000, <10000
# place of birth: UK, non UK
# home ownership: Owns or part-owns home, does not own home
# sex: male, female
# age: 16-44, >=45
# first language: English, other
# ethnicity: white, black and minority ethnic
# qualification level: NQF >= level at age 16 (level 2), below level 2
# area deprivation: IMD quintiles

# matching with original survey

# NSSEC7: 1	Higher managerial and professional
#         2	Lower managerial and professional
#         3	Intermediate
#         4	Small employers and own account workers
#         5	Lower supervisory and technical
#         6	Semi-routine occupations
#         7	Routine occupations
#         8	Never worked/ long term unemployed
#         9	Full-time student
#         10 Not classifiable
# WORKINGSTATUS2: 0-No, 1-Yes
# GROSS_ANNUAL_INCOME_OLDBANDS: {<£5,000, £5,000 - £9,999},
# {£10,000 - £14,999, £15,000 - £19,999, £20,000 - £29,999}
# BUK: 1-Yes, 2-No
# QxTenu1: 1-Own home outright or with a mortgage or loan
# Sex1: 1-Male, 2-Female
# AGE1NET: {16-24, 25-44}, 45-65
# Sesol: 1-Yes, 2-No
# ETHNICSIMPLE: 1-White, 2-BME
# HIQUAL: {1-4), {5-Level 1 qualification or below}
# IMDSCOREB4: 1,...,9

model_dat <-
  data |>
  # remove class
  mutate(
    WORKINGSTATUS2 = unclass(WORKINGSTATUS2),
    GROSS_ANNUAL_INCOME_OLDBANDS = unclass(GROSS_ANNUAL_INCOME_OLDBANDS),
    BUK = unclass(BUK),
    QxTenu1 = unclass(QxTenu1),
    Sex1 = unclass(Sex1),
    AGE1NET = unclass(AGE1NET),
    Sesol = unclass(Sesol),
    ETHNICSIMPLE = unclass(ETHNICSIMPLE),
    HIQUAL = unclass(HIQUAL),
    IMDSCOREB4 = unclass(IMDSCOREB4),
    NSSEC7 = unclass(NSSEC7),
    LiteracyThresholdA_1 = unclass(LiteracyThresholdA_1),
    NumeracyThresholdA_1 = unclass(NumeracyThresholdA_1),
    MultipleChoiceLevelA_1Thres = unclass(MultipleChoiceLevelA_1Thres),
    LiteracyScoreA_1 = unclass(LiteracyScoreA_1),
    NumeracyScoreA_1 = unclass(NumeracyScoreA_1)) |>
  # relabel and order levels
  transmute(
    workingstatus = factor(WORKINGSTATUS2, levels = 1:0, labels = c("Yes","No")),
    gross_income =
      ifelse(GROSS_ANNUAL_INCOME_OLDBANDS %in% 1:2,
             "<10000",
             ifelse(GROSS_ANNUAL_INCOME_OLDBANDS %in% 3:6,
                    ">=10000", "other")) |>
      factor(levels = c(">=10000", "<10000", "other")),
    uk_born = factor(BUK, levels = 1:2, labels = c("Yes", "No")),
    sex = factor(Sex1, levels = c(2,1), c("Female", "Male")),
    own_home = ifelse(QxTenu1 == 1, "Yes", "No") |>
      factor(levels = c("Yes", "No")),
    age = ifelse(AGE1NET %in% 1:2, "16-44",
                 ifelse(AGE1NET == 3, ">=45", "other")) |>
      factor(levels = c("16-44", ">=45")),
    english_lang = factor(Sesol, levels = 1:2, labels = c("Yes", "No")),
    ethnicity = factor(ETHNICSIMPLE, levels = 1:2, labels = c("White", "BME")),
    qualification = ifelse(HIQUAL %in% 1:4, ">=level 2", "<=Level 1") |>
      factor(levels = c(">=level 2", "<=Level 1")),
    imd = factor(IMDSCOREB4),
    job_status = ifelse(NSSEC7 %in% 1:2, "higher",
                        ifelse(NSSEC7 == 3, "intermediate",
                               ifelse(NSSEC7 %in% 4:10, "lower", "other"))) |>
      factor(levels = c("intermediate", "lower", "higher")),
    lit_thresholdL1 =
      ifelse(LiteracyThresholdA_1 == 1, "below",
             ifelse(LiteracyThresholdA_1 == 2, "above", "other")),
    lit_thresholdL2 = ifelse(LiteracyScoreA_1 == 5, "above",
                             ifelse(LiteracyScoreA_1 %in% 1:4, "below", "other")),   # >= L2
    num_thresholdEL3 =
      ifelse(NumeracyThresholdA_1 == 1, "below",
             ifelse(NumeracyThresholdA_1 == 2, "above", "other")),
    num_thresholdL1 = ifelse(NumeracyScoreA_1 == 4:5, "above",
                             ifelse(NumeracyScoreA_1 %in% 1:3, "below", "other")),  # >= L1
    ict_thresholdEL3 =
      ifelse(MultipleChoiceLevelA_1Thres == 1, "below",
             ifelse(MultipleChoiceLevelA_1Thres == 2, "above", "other")),
    weights = unclass(rimweight2003),
    lit_weightsL1 = unclass(rimweightLIT2003),
    num_weightsEL3 = unclass(rimweightNUM2003),
    ict_weightsEL3 = unclass(rimweightICT2003)
  ) |>
  filter(!is.na(age),
         !is.na(ethnicity))

# test specific data sets
# where have answered question

lit_dat <- model_dat |>
  filter(lit_thresholdL2 %in% c("above", "below")) |>
  mutate(lit_thresholdL2 = as.factor(lit_thresholdL2),
         lit_thresholdL2_bin = as.integer(lit_thresholdL2) - 1L) |>
  select(-lit_weightsL1, -num_weightsEL3, -ict_weightsEL3, -num_thresholdEL3, -num_thresholdL1, -ict_thresholdEL3)

num_dat <- model_dat |>
  filter(num_thresholdL1 %in% c("above", "below")) |>
  mutate(num_thresholdL1 = as.factor(num_thresholdL1),
         num_thresholdL1_bin = as.integer(num_thresholdL1) - 1L) |>
  select(-lit_weightsL1, -num_weightsEL3, -ict_weightsEL3, -num_thresholdEL3, -lit_thresholdL2, -ict_thresholdEL3)

ict_dat <- model_dat |>
  filter(ict_thresholdEL3 %in% c("above", "below")) |>
  mutate(ict_thresholdEL3 = as.factor(ict_thresholdEL3),
         ict_thresholdEL3_bin = as.integer(ict_thresholdEL3) - 1L) |>
  select(-lit_weightsL1, -num_weightsEL3, -ict_weightsEL3, -num_thresholdEL3, -num_thresholdL1, -lit_thresholdL2)

################
# summary stats

lit_dat$lit_thresholdL2 |> table() |> prop.table()

#######################
# logistic regressions

rhs <- "1 + sex + age + ethnicity + uk_born + english_lang + qualification + workingstatus + job_status + gross_income + own_home + imd"

# unweighted
lit_glm <- glm(glue("lit_thresholdL2_bin ~ {rhs}"), data = lit_dat, family = binomial(), weights = weights)
lit_glm_stan <- rstanarm::stan_glm(glue("lit_thresholdL2_bin ~ {rhs}"), data = lit_dat, family = binomial(),
                                   weights = weights, chains = 2, iter = 2000)

# lit_glm
suppressWarnings({
  tbl_regression(lit_glm, exponentiate = TRUE)
})
# see Table 3 in Rowlands (2015)

num_glm <- glm(glue("num_thresholdL1_bin ~ {rhs}"), data = num_dat, family = binomial(), weights = weights)
# num_glm
suppressWarnings({
  tbl_regression(num_glm, exponentiate = TRUE)
})

ict_glm <- glm(glue("ict_thresholdEL3_bin ~ {rhs}"), data = ict_dat, family = binomial(), weights = weights)
# ict_glm
suppressWarnings({
  tbl_regression(num_glm, exponentiate = TRUE)
})

# partial pooling?


######################
# post-stratification

# CRAN package, including newer methods from ML:
#   https://cran.r-project.org/web/packages/autoMrP/vignettes/autoMrP_vignette.pdf
#
# This is the Stan vignette:
#   https://mc-stan.org/rstanarm/articles/mrp.html
#
# Some interesting extensions:
#   https://bookdown.org/jl5522/MRP-case-studies/
#
# Stacked Regression and Poststratification (SRP)?
# (Breiman, 1996)


## prediction

unique_workingstatus <- unique(lit_dat$workingstatus)
unique_gross_income <- unique(lit_dat$gross_income)
unique_uk_born <- unique(lit_dat$uk_born)
unique_sex <- unique(lit_dat$sex)
unique_own_home <- unique(lit_dat$own_home)
unique_age <- unique(lit_dat$age)
unique_english_lang <- unique(lit_dat$english_lang)
unique_ethnicity <- unique(lit_dat$ethnicity)
unique_qualification <- unique(lit_dat$qualification)
unique_imd <- unique(lit_dat$imd)
unique_job_status <- unique(lit_dat$job_status)

# generate all combinations
combs_df <- expand.grid(
  workingstatus = unique_workingstatus,
  gross_income = unique_gross_income,
  uk_born = unique_uk_born,
  sex = unique_sex,
  own_home = unique_own_home,
  age = unique_age,
  english_lang = unique_english_lang,
  ethnicity = unique_ethnicity,
  qualification = unique_qualification,
  imd = unique_imd,
  job_status = unique_job_status
) |> as_tibble()

names_vars <- names(combs_df)
combs_df$predicted_prob <- predict(lit_glm, combs_df, type = 'response')


###################################
# join with target population data

# IMD is at LSOA level
ward_lookup <- read.csv(here::here("raw_data/Ward - neighbourhood - quadrant 2024.csv"))

imd_dat <- read.csv(here::here("raw_data/localincomedeprivationdata_Newham.csv")) |>
  rename(LSOA11CD = "LSOA.code..2011.",
         imd = "Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.",
         pop = "Total.population..mid.2015..excluding.prisoners.") |>
  select(LSOA11CD, imd, pop)

LSOA_lookup <-
  read.csv(here::here("raw_data/Lower_Layer_Super_Output_Area_(2011)_to_Ward_(2015)_Lookup_in_England_and_Wales.csv")) |>
  filter(LAD15NM == "Newham")

imd_lookup <-
  LSOA_lookup |>
  merge(ward_lookup, by.x = "WD15NM", by.y = "Ward") |>
  merge(imd_dat) |>
  group_by(imd) |>
  summarize(pop = sum(pop)) |>
  mutate(p_imd = pop / sum(pop)) |>
  select(-pop)

# from resident survey report summary tables
# unless otherwise indicated

total_dat <-
  combs_df |>
  merge(
    tribble(~age, ~p_age,
            "16-44", 0.15 + 0.27 + 0.22,
            ">=45", 0.15 + 0.11 + 0.1)) |>
  merge(
    tribble(~sex, ~p_sex,
            "Male", 0.54,
            "Female", 0.46)) |>
  merge(
    tribble(~ethnicity, ~p_ethn,
            "White", 0.30,
            "BME", 0.70)) |>
  merge(
    tribble(~workingstatus, ~p_workstatus,
            "Yes", 0.65,
            "No", 0.35)) |>
  merge(
    tribble(~own_home, ~p_own_home,
            "Yes", 0.35,
            "No", 0.65)) |>
  # ONS census 2021 Highest level of qualification
  merge(
    tribble(~qualification, ~p_qual,
            ">=level 2", 0.57,
            "<=Level 1", 0.43)) |>
  # Q61: household gross income before tax
  # Q70: Are you the main or joint householder? e.g.responsible for bills such as rent, mortgage and utilities
  # this would be good but its mostly 'not answered'!
  # Q54	What is your average monthly pay?
  #
  ##TODO: break down by LSOA and map to CNA
  ##  read from Newham tab in saiefy1920finalqaddownload280923.xlsx
  merge(
    tribble(~gross_income, ~p_income,
            ">=10000", 0.9,
            "<10000", 0.1)) |>
  # census 2021 usual resident population
  merge(
    tribble(~uk_born, ~p_uk,
            "Yes", 0.455 + 0.001 + 0.004 + 0.003,
            "No", 0.553)) |>
  # Q77	How well can you speak English?
  # 1	Very well
  # 2	Well
  # 3	Not well
  #
  # ONS census 2021 English as main language
  merge(
    tribble(~english_lang, ~p_english,
            "Yes", 0.6537,
            "No", 0.3463)) |>
  # AB: higher and intermediate managerial, administrative and professional occupations
  # C1: supervisory, clerical and junior managerial, administrative and professional occupations
  # C2: skilled manual occupations
  # DE: semi-skilled and unskilled manual and lowest grade occupations
  #
  # tribble(~job_status_ASG, ~job_status, ~prop,
  #         "AB", "higher", 0.167,
  #         "C1", "intermediate", 0.276,
  #         "C2", "lower", 0.234,
  #         "DE", "lower", 0.323)
  merge(
    tribble(~job_status, ~p_job,
            "higher", 0.167,
            "intermediate", 0.276,
            "lower", 0.234 + 0.323)) |>
  merge(imd_lookup) |>
  #####################
# calculate product of probabilities, assuming independence
rowwise() |>
  mutate(product_p = prod(c_across(starts_with("p_")))) |>
  ungroup()

write.csv(total_dat, here::here("data/total_dat.csv"))

##TODO:
# merge(
#   tribble(~area_cna, ~p_cna,
#           "beckton", 0.05,
#           "custom_house_and_canning_town", 0.14,
#           "east_ham", 0.1,
#           "forest_gate", 0.12,
#           "green_street", 0.13,
#           "manor_park", 0.12,
#           "plaistow", 0.14,
#           "royal_docks", 0.07,
#           "stratford_and_west_ham", 0.13))

#################
# stratification

poststratified_estimates <-
  total_dat |>
  summarize(estimate = weighted.mean(predicted_prob, product_p))

poststratified_estimates

# Bayesian

posterior_draws <- rstanarm::posterior_epred(lit_glm_stan, newdata = total_dat)
poststrat_estimates_stan <- posterior_draws %*% total_dat$product_p

hist(poststrat_estimates_stan, breaks = 20, main = "")
abline(v = poststratified_estimates, col = "red", lwd = 2)


################################
# average marginal effect (AME)

# within levels
conditional_effects <-
  lapply(names(combs_df), function(x) {
    total_dat %>%
      group_by(!!sym(x)) %>%
      summarize(estimate = weighted.mean(predicted_prob, product_p))
  })

# set everyone to same level
fac_levels <- levels(total_dat$workingstatus)
appended_df <- purrr::map_dfr(fac_levels, ~total_dat %>% mutate(workingstatus = .x))
appended_df$predicted_prob <- predict(lit_glm, appended_df, type = 'response')

ps_workingstatus <-
  appended_df %>%
  group_by(workingstatus) %>%
  summarize(estimate = weighted.mean(predicted_prob, product_p))

# average marginal effect vs current profile
ps_workingstatus$ame <- ps_workingstatus$estimate - poststratified_estimates$estimate


# for _all_ variables

# frequentist

ps_var <- list()

for (i in names_vars) {
  fac_levels <- levels(total_dat[[i]])
  appended_df <- purrr::map_dfr(fac_levels, ~total_dat %>% mutate(i = .x))
  appended_df$predicted_prob <- predict(lit_glm, newdata = appended_df, type = 'response')

  ps_var[[i]] <-
    appended_df %>%
    group_by(!!sym(i)) %>%
    summarize(estimate = weighted.mean(predicted_prob, product_p))

  ps_var[[i]]$ame <- ps_var[[i]]$estimate - poststratified_estimates$estimate
  ps_var[[i]] <- ps_var[[i]] |> mutate(ame_base = estimate - first(estimate))

  # common first column name
  names(ps_var[[i]])[1] <- "name"
}

# Bayesian

library(tidyr)

ps_var <- list()

for (i in names_vars) {
  fac_levels <- levels(total_dat[[i]])
  appended_df <- purrr::map_dfr(fac_levels, ~total_dat %>% mutate(i = .x))
  posterior_draws <-
    rstanarm::posterior_epred(
      lit_glm_stan,
      newdata = appended_df,
      draws = 20)

  post_draws <-
    cbind(t(posterior_draws)) |>
    as_tibble(.name_repair = "universal")

  names(post_draws) <- gsub(pattern = "...",
                            replacement = "draws_",
                            x = names(post_draws))
  ps_var[[i]] <-
    appended_df %>%
    cbind(post_draws) %>%
    group_by(!!sym(i)) %>%
    summarize_at(vars(starts_with('draws')),
                 list(~ weighted.mean(., w = product_p)))

  # common first column name
  names(ps_var[[i]])[1] <- "name"

  ps_var[[i]] <-
    reshape2::melt(ps_var[[i]]) |>
    group_by(variable) |>
    mutate(ame_base = value - first(value))
}

########
# plots

library(ggplot2)

plot_dat <- bind_rows(ps_var, .id = "vars")

plot_dat |>
  ggplot(aes(x = vars, y = ame_base, fill = name)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Marginal Effect",
       x = "Variable",
       y = "AME") +
  theme(legend.position = "none") +
  coord_flip()

# scatter plot by levels

plot_ls <- list()

for (i in names_vars) {
  # Calculate means for each level of 'name'
  means_df <- ps_var[[i]] %>%
    group_by(name) %>%
    summarise(mean_value = mean(value, na.rm = TRUE)) |>
    mutate(lead_name = lead(name),
           lead_mean_value = lead(mean_value)) %>%
    filter(!is.na(lead_name) & !is.na(lead_mean_value))

  plot_ls[[i]] <-
    ps_var[[i]] |>
    ggplot(aes(x = name, y = value)) +
    # add jitter to points
    geom_jitter(width = 0.1, height = 0) +
    # draw gradient line connecting the means
    geom_segment(data = means_df,
                 aes(x = name, xend = lead_name,
                     y = mean_value, yend = lead_mean_value),
                 col = "red") +
    ylab("P(not health literate)") +
    xlab(tools::toTitleCase(stringr::str_replace_all(i, "_", " "))) +
    ylim(0.4, 0.75) +
    theme_minimal()
}

gridExtra::grid.arrange(grobs = plot_ls, ncol = 3)

# AME forest plot
ame_dat_ls <- list()

for (i in names_vars) {
  ame_dat_ls[[i]] <-
    ps_var[[i]] |>
    group_by(name) |>
    summarise(mean_value = mean(ame_base, na.rm = TRUE),
              upper = quantile(ame_base, 0.975),
              lower = quantile(ame_base, 0.025)) |>
    mutate(variable = i,
           var_name = paste0(variable, "_", name)) |>
    filter(mean_value != 0)
}

ame_plot_dat <- do.call(rbind, ame_dat_ls)

ggplot(ame_plot_dat, aes(x = var_name, y = mean_value, colour = variable)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  coord_flip() +
  ylab("Average marginal effect") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()

ggasve(filename = here::here("plots/ame_plot.png")))

# regression-type table



