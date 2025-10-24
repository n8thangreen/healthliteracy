# compare SfL 2011 with PIAAC


library(purrr)

refit <- TRUE
use_stan <- FALSE

# SfL 2011 data

load(here::here("data/skills_for_life_2011_data.RData"))

survey_data2011 <- clean_sfl_data_2011(data2011)

if (refit) {
  fit2011 <- fit_all_models(survey_data2011, stan = use_stan, year_suffix = "piaac")
} else {
  load(here::here("data/fit2011.RData"))
}

# PIAAC

load(here::here("data/data_PIAAC.RData"))

PIAAC_survey_data <- clean_PIAAC_data(data_PIAAC)

if (refit) {
  fit_piaac <- fit_all_models(PIAAC_survey_data, stan = use_stan, year_suffix = "piaac")
} else {
  load(here::here("data/fit_piaac.RData"))
}


# literacy

pred_obj_2011 <- predict(object = fit2011$lit, newdata = survey_data2011$lit, type = "response", se.fit = TRUE)
pred_obj_piaac <- predict(object = fit_piaac$lit, newdata = survey_data2011$lit, type = "response", se.fit = TRUE)

pred2011 <- pred_obj_2011$fit
pred_piaac <- pred_obj_piaac$fit

ci_upper_2011 <- pred2011 + 1.96 * pred_obj_2011$se.fit
ci_lower_2011 <- pred2011 - 1.96 * pred_obj_2011$se.fit

ci_upper_piaac <- pred_piaac + 1.96 * pred_obj_piaac$se.fit
ci_lower_piaac <- pred_piaac - 1.96 * pred_obj_piaac$se.fit

# colour points
colors_by_var <- survey_data2011$lit$qualification

plot(pred_piaac, pred2011,
     xlim = c(0, 1), ylim = c(0, 1),
     type = "n",
     xlab = "Health literacy prediction from PIAAC Model",
     ylab = "Health literacy prediction from 2011 Model")

arrows(x0 = ci_lower_piaac, y0 = pred2011,
       x1 = ci_upper_piaac, y1 = pred2011,
       length = 0.02, angle = 90, code = 3, col = "gray")

arrows(x0 = pred_piaac, y0 = ci_lower_2011,
       x1 = pred_piaac, y1 = ci_upper_2011,
       length = 0.02, angle = 90, code = 3, col = "gray")

points(pred_piaac, pred2011,
       # col = colors_by_var,
       pch = 19, cex = 1)

abline(a = 0, b = 1, col = "red", lwd = 2)


# numeracy

pred_obj_2011 <- predict(object = fit2011$num, newdata = survey_data2011$num, type = "response", se.fit = TRUE)
pred_obj_piaac <- predict(object = fit_piaac$num, newdata = survey_data2011$num, type = "response", se.fit = TRUE)

pred2011 <- pred_obj_2011$fit
pred_piaac <- pred_obj_piaac$fit

ci_upper_2011 <- pred2011 + 1.96 * pred_obj_2011$se.fit
ci_lower_2011 <- pred2011 - 1.96 * pred_obj_2011$se.fit

ci_upper_piaac <- pred_piaac + 1.96 * pred_obj_piaac$se.fit
ci_lower_piaac <- pred_piaac - 1.96 * pred_obj_piaac$se.fit

plot(pred_piaac, pred2011,
     xlim = c(0, 1), ylim = c(0, 1),
     type = "n",
     xlab = "Health numeracy prediction from 2003 Model",
     ylab = "Health numeracy prediction from 2011 Model")

arrows(x0 = ci_lower_piaac, y0 = pred2011,
       x1 = ci_upper_piaac, y1 = pred2011,
       length = 0.02, angle = 90, code = 3, col = "gray")

arrows(x0 = pred_piaac, y0 = ci_lower_2011,
       x1 = pred_piaac, y1 = ci_upper_2011,
       length = 0.02, angle = 90, code = 3, col = "gray")

points(pred_piaac, pred2011, pch = 19, cex = 0.6) # pch=19 is a solid circle

abline(a = 0, b = 1, col = "red", lwd = 2)


# --- interaction model ---

PIAAC_survey_data$lit$year <- "piaac"
survey_data2011$lit$year <- "2011"

fe_names <- c("sex", "age", "uk_born", "english_lang", "qualification", "workingstatus")

keep_cols <- c(fe_names, "year", "lit_thresholdL2_bin", "weights")

combined_data_lit <- rbind(PIAAC_survey_data$lit[, keep_cols],
                           survey_data2011$lit[, keep_cols])

fe_form <- paste(fe_names, collapse = " + ")
fe_form <- glue::glue("year * ({fe_form})")

# literacy

fit_interaction_lit <-
  stats::glm(glue("lit_thresholdL2_bin ~ {fe_form}"),
             data = combined_data_lit, family = binomial(),
             weights = weights)

summary(fit_interaction_lit)

# numeracy

survey_data2003$num$year <- "2003"
survey_data2011$num$year <- "2011"
combined_data_num <- rbind(survey_data2003$num, survey_data2011$num)

fit_interaction_num <-
  stats::glm(glue("num_thresholdL1_bin ~ {fe_form}"),
             data = combined_data_num, family = binomial(),
             weights = weights)

summary(fit_interaction_num)

