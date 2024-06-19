# multilevel regression and post-stratification analysis

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
# job status: National Statistics Socioeconomic Classification 3 bands (Managerial/professional, Intermediate, Routine/manual/ students/unemployed)
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

# matching with survey

# WORKINGSTATUS2: 0-No, 1-Yes
# GROSS_ANNUAL_INCOME_OLDBANDS: {<£5,000, £5,000 - £9,999}, {£10,000 - £14,999, £15,000 - £19,999, £20,000 - £29,999}
# BUK: 1-Yes, 2-No
# QxTenu1: 1-Own home outright or with a mortgage or loan
# Sex1: 1-Male, 2-Female
# AGE1NET: {16-24, 25-44}, 45-65
# Sesol: 1-Yes, 2-No
# ETHNICSIMPLE: 1-White, 2-BME
# HIQUAL: {1-4), {5-Level 1 qualification or below}
# IMDSCOREB4: 1,...,9
# NSSEC7: 1	Higher managerial and professional
# 2	Lower managerial and professional
# 3	Intermediate
# 4	Small employers and own account workers
# 5	Lower supervisory and technical
# 6	Semi-routine occupations
# 7	Routine occupations
# 8	Never worked/ long term unemployed
# 9	Full-time student
# 10	Not classifiable

################
# data cleaning

model_dat <-
  data |>
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
  transmute(
    workingstatus = factor(WORKINGSTATUS2, levels = 0:1, labels = c("No", "Yes")),
    gross_income =
      ifelse(GROSS_ANNUAL_INCOME_OLDBANDS %in% 1:2,
             "<10000",
             ifelse(GROSS_ANNUAL_INCOME_OLDBANDS %in% 3:6,
                    ">=10000", "other")) |>
      as.factor(),
    uk_born = factor(BUK, levels = 1:2, labels = c("Yes", "No")),
    sex = factor(Sex1, levels = c(2,1), c("Female", "Male")),
    own_home = ifelse(QxTenu1 == 1, "Yes", "No") |> as.factor(),
    age = ifelse(AGE1NET %in% 1:2, "16-44",
                 ifelse(AGE1NET == 3, ">=45", "other")) |>
      as.factor(),
    english_lang = factor(Sesol, levels = 1:2, labels = c("Yes", "No")),
    ethnicity = factor(ETHNICSIMPLE, levels = 1:2, labels = c("White", "BME")),
    qualification = ifelse(HIQUAL %in% 1:4, ">=level 2", "<=Level 1") |>
      as.factor(),
    imd = factor(IMDSCOREB4),
    job_status = ifelse(NSSEC7 %in% 1:2, "higher",
                        ifelse(NSSEC7 == 3, "intermediate",
                               ifelse(NSSEC7 %in% 4:10, "lower", "other"))) |>
      as.factor(),

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
  )

summary(model_dat)

# test specific data sets
lit_dat <- model_dat |>
  filter(lit_thresholdL2 %in% c("above", "below")) |>
  mutate(lit_thresholdL2 = as.factor(lit_thresholdL2))

num_dat <- model_dat |>
  filter(num_thresholdL1 %in% c("above", "below")) |>
  mutate(num_thresholdL1 = as.factor(num_thresholdL1))

ict_dat <- model_dat |>
  filter(ict_thresholdEL3 %in% c("above", "below")) |>
  mutate(ict_thresholdEL3 = as.factor(ict_thresholdEL3))

################
# summary stats

lit_dat$lit_thresholdL2 |> table() |> prop.table()

#######################
# logistic regressions

rhs <- "1 + workingstatus + gross_income + uk_born + sex + own_home + age + english_lang + ethnicity + qualification + imd + job_status"

# unweighted
lit_glm <- glm(glue("lit_thresholdL2 ~ {rhs}"), data = lit_dat, family = binomial(), weights = weights)
# lit_glm
suppressWarnings({
  tbl_regression(lit_glm, exponentiate = TRUE)
})

num_glm <- glm(glue("num_thresholdL1 ~ {rhs}"), data = num_dat, family = binomial(), weights = weights)
# num_glm
suppressWarnings({
  tbl_regression(num_glm, exponentiate = TRUE)
})

ict_glm <- glm(glue("ict_thresholdEL3 ~ {rhs}"), data = ict_dat, family = binomial(), weights = weights)
# ict_glm
suppressWarnings({
  tbl_regression(num_glm, exponentiate = TRUE)
})

