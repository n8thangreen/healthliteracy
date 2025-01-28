# descriptive data analysis
# for health literacy analysis


library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(stringr)


##################
# skills for life

load(here::here("data/skills_for_life_data.RData"))

survey_data <- clean_data(data)

summarize_data <- function(data) {
  data %>%
    summarise(
      workingstatus = list(table(workingstatus)),
      gross_income = list(table(gross_income)),
      uk_born = list(table(uk_born)),
      sex = list(table(sex)),
      own_home = list(table(own_home)),
      age = list(table(age)),
      english_lang = list(table(english_lang)),
      ethnicity = list(table(ethnicity)),
      qualification = list(table(qualification)),
      imd = list(table(imd)),
      job_status = list(table(job_status))) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Counts") %>%
    unnest(Counts) %>%
    mutate(
      Category = as.character(names(Counts)),
      n = as.numeric(Counts)) |>
    mutate(.by = Variable,
           `\\%` = round(n / sum(n), 2)*100) %>%
    select(Variable, Category, n, `\\%`) |>
    # replace underscores with spaces
    mutate(Category = gsub("_", " ", Category),
           Variable = gsub("_", " ", Variable),
           # start with capital letter
           Variable = str_to_title(Variable),
           Category = str_to_title(Category),
           Variable = gsub("Bme", "BME", Variable),
           Variable = gsub("Uk", "UK", Variable),
           Variable = gsub("Imd", "IMD", Variable))
}

summary_results <- summarize_data(survey_data$lit)
summary_results

summary_results <- lapply(survey_data, summarize_data) |>
  plyr::join_all(by = c("Variable", "Category"))

# Replace special characters with LaTeX equivalents (e.g., <= with \leq)
summary_results$Category <- gsub("<=", "$\\\\leq$", summary_results$Category)
summary_results$Category <- gsub(">=", "$\\\\geq$", summary_results$Category)
summary_results$Category <- gsub("<", "$<$", summary_results$Category)
summary_results$Category <- gsub(">", "$>$", summary_results$Category)

# latex table

kable(summary_results,
      format = "latex",
      align = c("l", "l", "r", "r"), escape = FALSE,
      booktabs = TRUE) |>
  kable_styling(latex_options = c("hold_position")) %>%  # Keep table in place
  add_header_above(c(" " = 2, "Lit" = 2, "Num" = 2, "ICT" = 2))


#########
# Newham

# ONS census 2011 for Newham
imd_dat <- read.csv(here::here("raw_data/localincomedeprivationdata_Newham.csv")) |>
  rename(LSOA11CD = "LSOA.code..2011.",
         imd = "Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.",
         pop = "Total.population..mid.2015..excluding.prisoners.") |>
  select(LSOA11CD, imd, pop)
##NOTE: there aren't any 7 or 9

# 2011 IMD
imd_lookup <-
  imd_dat |>
  group_by(imd) |>
  summarize(pop = sum(pop)) |>
  mutate(p_imd = pop / sum(pop)) |>
  select(-pop)

# check 2011 IMD against 2019 data
imd_dat_2019 <- read.csv(here::here("raw_data/File_1_-_IMD2019_Index_of_Multiple_Deprivation_Newham.csv"))
names(imd_dat_2019)[names(imd_dat_2019) == "LSOA.code..2011."] <- "LSOA11CD"
xx <- merge(imd_dat, imd_dat_2019, by = "LSOA11CD")

# check 2011 pop against 2021 data
lsoa_lookup <- read.csv(here::here("raw_data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Best_Fit_Lookup_for_EW_(V2).csv"))
pop_dat_2021 <- read.csv(here::here("raw_data/sapelsoasyoa20192022_Newham.csv"))

names(pop_dat_2021)[names(pop_dat_2021) == "LSOA.2021.Code"] <- "LSOA21CD"

pop_merged <- merge(imd_dat, lsoa_lookup, by = "LSOA11CD", all.x = TRUE, all.y = FALSE)

imd_dat_2021 <-
  merge(pop_merged, pop_dat_2021, by = "LSOA21CD", allx = TRUE) |>
  rename(pop_2011 = pop, pop_2021 = Total) |>
  select(LSOA21CD, LSOA11CD, imd, pop_2011, pop_2021)

# 2021 IMD
imd_lookup <-
  imd_dat_2021 |>
  group_by(imd) |>
  summarize(pop = sum(pop_2021)) |>
  mutate(p_imd = pop / sum(pop)) |>
  select(-pop)

newham_props <- tribble(
  ~Variable, ~Category, ~ Newham,
  "age", "16-44", 0.15 + 0.27 + 0.22,
  "age", "$\\geq$45", 0.15 + 0.11 + 0.1,
  "sex", "Male", 0.54,
  "sex", "Female", 0.46,
  "ethnicity", "White", 0.30,
  "ethnicity", "BME", 0.70,
  "workingstatus", "Yes", 0.65,
  "workingstatus", "No", 0.35,
  "own_home", "Yes", 0.35,
  "own_home", "No", 0.65,
  "qualification", "$\\geq$level 2", 0.57,
  "qualification", "$\\leq$Level 1", 0.43,
  "gross_income", "$\\geq$10000", 0.9,
  "gross_income", "$<$10000", 0.1,
  "uk_born", "Yes", 0.455 + 0.001 + 0.004 + 0.003,
  "uk_born", "No", 0.553,
  "english_lang", "Yes", 0.6537,
  "english_lang", "No", 0.3463,
  "job_status", "higher", 0.167,
  "job_status", "intermediate", 0.276,
  "job_status", "lower", 0.234 + 0.323
) |>
  rbind(
    imd_lookup |>
      mutate(Variable = "imd") |>
      rename(Newham = p_imd) |>
      mutate(Category = as.character(imd)) |>
      select(Variable, Category, Newham)
  ) |>
  mutate(Newham = round(Newham, 2)*100) |>
  rename(`\\%` = Newham) |>
  # replace underscores with spaces
  mutate(Category = gsub("_", " ", Category),
         Variable = gsub("_", " ", Variable),
         # start with capital letter
         Variable = ifelse(str_starts(Variable, "\\$"), Variable, str_to_title(Variable)),
         Category = ifelse(str_starts(Category, "\\$"), Category, str_to_title(Category)),
         Variable = gsub("Uk", "UK", Variable),
         Variable = gsub("Bme", "BME", Variable),
         Variable = gsub("Imd", "IMD", Variable))

full_table <- plyr::join(summary_results, newham_props,
                         by = c("Variable", "Category"))

# latex table

full_table |>
  kable(format = "latex",
        align = c("l", "l", "r", "r", "r"),
        escape = FALSE,
        booktabs = TRUE) |>
  kable_styling(latex_options = c()) |> # drop addlinespace?
  add_header_above(c(" " = 2, "Lit" = 2, "Num" = 2, "ICT" = 2, "Newham" = 1))
