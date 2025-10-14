# compare SfL 2011 with SfL 2003


library(purrr)

refit <- TRUE
use_stan <- FALSE

# SfL 2011 data

load(here::here("data/skills_for_life_2011_data.RData"))

survey_data2011 <- clean_sfl_data_2011(data2011)

if (refit) {
  fit2011 <- fit_models(survey_data2011, stan = use_stan)
} else {
  load(here::here("data/fit2011.RData"))
}

# SfL 2003 data

# data2003 <-
#   haven::read_dta(
#     file = "../../../Newham Council Fellowship/data/skills for life survey/2003/UKDA-7239-stata9/stata9/2003_skills_for_life_datafile_anonymised.dta")
#
# save(data2003, file = "data/skills_for_life_2003_data.RData")

load(here::here("data/skills_for_life_2003_data.RData"))

survey_data2003 <- clean_sfl_data_2003(data2003)

if (refit) {
  fit2003 <- fit_models(survey_data2003, stan = use_stan)
} else {
  load(here::here("data/fit2003.RData"))
}

# literacy

pred_obj_2011 <- predict(object = fit2011$lit, newdata = survey_data2011$lit, type = "response", se.fit = TRUE)
pred_obj_2003 <- predict(object = fit2003$lit, newdata = survey_data2011$lit, type = "response", se.fit = TRUE)

pred2011 <- pred_obj_2011$fit
pred2003 <- pred_obj_2003$fit

ci_upper_2011 <- pred2011 + 1.96 * pred_obj_2011$se.fit
ci_lower_2011 <- pred2011 - 1.96 * pred_obj_2011$se.fit

ci_upper_2003 <- pred2003 + 1.96 * pred_obj_2003$se.fit
ci_lower_2003 <- pred2003 - 1.96 * pred_obj_2003$se.fit

# # colour points
# colors_by_var <- survey_data2003$lit$job_status

plot(pred2003, pred2011,
     xlim = c(0, 1), ylim = c(0, 1),
     type = "n",
     xlab = "Health literacy prediction from 2003 Model",
     ylab = "Health literacy prediction from 2011 Model")

arrows(x0 = ci_lower_2003, y0 = pred2011,
       x1 = ci_upper_2003, y1 = pred2011,
       length = 0.02, angle = 90, code = 3, col = "gray")

arrows(x0 = pred2003, y0 = ci_lower_2011,
       x1 = pred2003, y1 = ci_upper_2011,
       length = 0.02, angle = 90, code = 3, col = "gray")

points(pred2003, pred2011,
       # col = colors_by_var,
       pch = 19, cex = 1)

abline(a = 0, b = 1, col = "red", lwd = 2)


# numeracy

pred_obj_2011 <- predict(object = fit2011$num, newdata = survey_data2011$num, type = "response", se.fit = TRUE)
pred_obj_2003 <- predict(object = fit2003$num, newdata = survey_data2011$num, type = "response", se.fit = TRUE)

pred2011 <- pred_obj_2011$fit
pred2003 <- pred_obj_2003$fit

ci_upper_2011 <- pred2011 + 1.96 * pred_obj_2011$se.fit
ci_lower_2011 <- pred2011 - 1.96 * pred_obj_2011$se.fit

ci_upper_2003 <- pred2003 + 1.96 * pred_obj_2003$se.fit
ci_lower_2003 <- pred2003 - 1.96 * pred_obj_2003$se.fit

plot(pred2003, pred2011,
     xlim = c(0, 1), ylim = c(0, 1),
     type = "n",
     xlab = "Health numeracy prediction from 2003 Model",
     ylab = "Health numeracy prediction from 2011 Model")

arrows(x0 = ci_lower_2003, y0 = pred2011,
       x1 = ci_upper_2003, y1 = pred2011,
       length = 0.02, angle = 90, code = 3, col = "gray")

arrows(x0 = pred2003, y0 = ci_lower_2011,
       x1 = pred2003, y1 = ci_upper_2011,
       length = 0.02, angle = 90, code = 3, col = "gray")

points(pred2003, pred2011, pch = 19, cex = 0.6) # pch=19 is a solid circle

abline(a = 0, b = 1, col = "red", lwd = 2)

# pred2011 <- predict(object = fit2011$ict, newdata = survey_data2011$ict, type = "response")
# pred2003 <- predict(object = fit2003$ict, newdata = survey_data2011$ict, type = "response")
#
# plot(pred2003, pred2011, xlim = c(0,1), ylim = c(0,1))
# abline(a = 0, b = 1, col = "red")


# --- interaction model ---

survey_data2003$lit$year <- "2003"
survey_data2011$lit$year <- "2011"
combined_data_lit <- rbind(survey_data2003$lit, survey_data2011$lit)

fe_names <- c("sex", "age", "ethnicity", "english_lang", "qualification",
              "workingstatus", "job_status", "gross_income", "own_home", "imd")
fe_form <- paste(fe_names, collapse = " + ")
fe_form <- glue::glue("year * ({fe_form})")

fit_interaction_lit <-
  stats::glm(glue("lit_thresholdL2_bin ~ {fe_form}"),
             data = combined_data_lit, family = binomial(),
             weights = weights)

summary(fit_interaction_lit)

survey_data2003$num$year <- "2003"
survey_data2011$num$year <- "2011"
combined_data_num <- rbind(survey_data2003$num, survey_data2011$num)

fit_interaction_num <-
  stats::glm(glue("num_thresholdL1_bin ~ {fe_form}"),
             data = combined_data_num, family = binomial(),
             weights = weights)

summary(fit_interaction_num)

