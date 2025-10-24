#' Fit a single model
#'
#' Helper function to fit one model using the specified function and arguments.
#' It constructs the formula and uses do.call to flexibly pass arguments.
#'
#' @param dat The data.frame to use for modelling.
#' @param outcome_var A string, the name of the outcome variable.
#' @param formula_rhs A string, the right-hand side of the model formula.
#' @param model_fun Modelling function to call (e.g., stats::glm, lme4::glmer).
#' @param common_args A list of common arguments for the model function.
#' @param ... Additional arguments passed to the modelling function (e.g., from fit_models).
#' @return A fitted model object.
#'
fit_model <- function(dat, outcome_var, formula_rhs, model_fun, common_args, ...) {

  # 1. Create the full formula
  full_formula_str <- glue::glue("{outcome_var} ~ {formula_rhs}")
  full_formula <- stats::as.formula(full_formula_str)

  # 2. Combine all arguments
  # We pass 'weights = quote(weights)' to tell glm/glmer to look for a column
  # named 'weights' inside the 'data' argument.
  model_args <- list(
    formula = full_formula,
    data = dat,
    family = binomial(),
    weights = quote(weights)
  )

  all_args <- c(model_args, common_args, list(...))

  do.call(model_fun, all_args)
}


#' Fit models to all outcomes
#'
#' Dynamically handles the presence of 'ict' data.
#'
#' @param survey_data A list containing data.frames named 'lit', 'num',
#'        and optionally 'ict'.
#' @param stan Boolean, whether to use rstanarm (TRUE) or stats/lme4 (FALSE).
#' @param save Boolean, whether to save the fitted models to an .RData file.
#' @param year_suffix A string suffix for the saved file (e.g., "2003").
#' @param ... Additional arguments passed to the modelling functions
#'        (e.g., control, chains, iter).
#' @return A named list containing the fitted models ('lit', 'num', 'ict').
#'
fit_all_models <- function(survey_data, stan = TRUE, save = FALSE, year_suffix = "", ...) {

  lit_dat <- survey_data$lit
  num_dat <- survey_data$num
  ict_dat <- survey_data$ict  # NULL if survey_data$ict doesn't exist

  has_ict <- !is.null(ict_dat)
  model_type <- if (stan) "stan" else "freq"

  # --- 2. Construct formula object ---
  if (year_suffix == "2003") {
    fe_names <- c("sex", "age", "ethnicity", "english_lang", "qualification",
                  "workingstatus", "job_status", "gross_income", "own_home")
    re_names <- "imd"
  } else if (year_suffix %in% c("", "2011")) {
    fe_names <- c("sex", "age", "ethnicity", "uk_born", "english_lang", "qualification",
                  "workingstatus", "job_status", "gross_income", "own_home")
    re_names <- "imd"
  } else if (year_suffix == "piaac") {
    fe_names <- c("sex", "age", "uk_born", "english_lang", "qualification",
                  "workingstatus", "gross_income")
    re_names <- NULL
  } else {
    stop("unrecognised year_suffix")
  }

  fe_form <- paste(fe_names, collapse = " + ")
  has_re <- length(re_names) > 0

  if (has_re) {
    re_form <- paste0("(1|", re_names, ")", collapse = " + ")
    re_form <- paste("+", re_form)
  } else {
    re_form <- NULL
  }

  # This is the complete RHS of our formula
  rhs <- paste("1 +", fe_form, re_form)

  # --- 3. Define model tasks ---
  # Create a list of all models we need to run
  tasks <- list(
    lit = list(data = lit_dat, outcome = "lit_thresholdL2_bin"),
    num = list(data = num_dat, outcome = "num_thresholdL1_bin")
  )

  # Conditionally add the ICT task if the data exists
  if (has_ict) {
    tasks$ict <- list(data = ict_dat, outcome = "ict_thresholdEL3_bin")
  }

  # --- 4. Select model function and arguments ---

  # Select the correct function based on 'stan' and 'has_re'
  model_fun <- if (stan) {
    if (has_re) rstanarm::stan_glmer else rstanarm::stan_glm
  } else {
    if (has_re) lme4::glmer else stats::glm
  }

  # Define common arguments (e.g., for stan)
  common_args <- if (stan) {
    list(chains = 2, iter = 2000)
  } else {
    list()
  }

  # --- 5. Run all models ---
  models_list <- lapply(tasks, function(task) {
    fit_model(
      dat = task$data,
      outcome_var = task$outcome,
      formula_rhs = rhs,
      model_fun = model_fun,
      common_args = common_args, ...
    )
  })

  if (save) {
    if (nchar(year_suffix) > 0) {
      year_suffix <- paste0("_", year_suffix)
    }

    file_path <- here::here(glue::glue(
      "data/{model_type}{year_suffix}_fits.RData"
    ))

    # This is the safest way to save.
    # It mimics the original 'save(lit, num, ict, ...)' by
    # 1. Creating variables named 'lit', 'num', (and 'ict') in this environment
    list2env(models_list, envir = environment())

    # 2. Getting the names of the objects we just created
    objects_to_save <- names(models_list)

    # 3. Telling save() to save the objects with those names
    save(list = objects_to_save, file = file_path)
  }

  models_list
}


#' #' @title Fit health literacy, numeracy and ICT regression models
#' #'
#' #' @param survey_data List of data frames containing the cleaned survey data
#' #' @param stan Logical indicating whether to use Stan or not
#' #' @param save Logical indicating whether to save the fitted models
#' #' @param ... Additional arguments to pass to the Stan model
#' #' @return List of fitted models
#' #'
#' #' @importFrom glue glue
#' #' @importFrom here here
#' #' @importFrom rstanarm stan_glm
#' #' @importFrom tibble lst
#' #' @seealso [clean_data()]
#' #'
#' fit_models <- function(survey_data, stan = TRUE, save = FALSE, year_suffix = "", ...) {
#'
#'   lit_dat <- survey_data$lit
#'   num_dat <- survey_data$num
#'   ict_dat <- survey_data$ict
#'
#'   model_type <- if (stan) "stan" else "freq"
#'
#'   # --- construct formula object
#'
#'   # # all FE
#'   # if (year_suffix == "2003") {
#'   #   # remove uk_born which is not available in 2003
#'   #   fe_names <- c("sex", "age", "ethnicity", "english_lang", "qualification",
#'   #                 "workingstatus", "job_status", "gross_income", "own_home", "imd")
#'   # } else {
#'   #   fe_names <- c("sex", "age", "ethnicity", "uk_born", "english_lang", "qualification",
#'   #                 "workingstatus", "job_status", "gross_income", "own_home", "imd")
#'   # }
#'   #
#'   # re_names <- NULL
#'
#'   # with RE for IMD
#'   if (year_suffix == "2003") {
#'     # remove uk_born which is not available in 2003
#'     fe_names <- c("sex", "age", "ethnicity", "english_lang", "qualification",
#'                   "workingstatus", "job_status", "gross_income", "own_home")
#'     re_names <- "imd"
#'   } else if (year_suffix %in% c("", "2011")) {
#'     fe_names <- c("sex", "age", "ethnicity", "uk_born", "english_lang", "qualification",
#'                   "workingstatus", "job_status", "gross_income", "own_home")
#'     re_names <- "imd"
#'   } else if (year_suffix == "piaac") {
#'     fe_names <- c("sex", "age", "uk_born", "english_lang", "qualification",
#'                   "workingstatus", "gross_income")
#'     re_names <- NULL
#'   } else {
#'     stop("unrecognised year_suffix")
#'   }
#'
#'   fe_form <- paste(fe_names, collapse = " + ")
#'
#'   # fixed effect only?
#'   has_re <- length(re_names) > 0
#'
#'   if (has_re) {
#'     re_form <- paste0("(1|", re_names, ")", collapse = " + ")
#'     re_form <- paste("+", re_form)
#'   } else {
#'     re_form <- NULL
#'   }
#'
#'   rhs <- paste("1 +", fe_form, re_form)
#'
#'   if (!stan) {
#'     if (has_re) {
#'       lit <- lme4::glmer(glue("lit_thresholdL2_bin ~ {rhs}"),
#'                          data = lit_dat, family = binomial(),
#'                          weights = weights, ...)
#'
#'       num <- lme4::glmer(glue("num_thresholdL1_bin ~ {rhs}"),
#'                          data = num_dat, family = binomial(),
#'                          weights = weights, ...)
#'
#'       ict <- lme4::glmer(glue("ict_thresholdEL3_bin ~ {rhs}"),
#'                          data = ict_dat, family = binomial(),
#'                          weights = weights, ...)
#'     } else {
#'       lit <- stats::glm(glue("lit_thresholdL2_bin ~ {rhs}"),
#'                         data = lit_dat, family = binomial(),
#'                         weights = weights, ...)
#'
#'       num <- stats::glm(glue("num_thresholdL1_bin ~ {rhs}"),
#'                         data = num_dat, family = binomial(),
#'                         weights = weights, ...)
#'
#'       ict <- stats::glm(glue("ict_thresholdEL3_bin ~ {rhs}"),
#'                         data = ict_dat, family = binomial(),
#'                         weights = weights, ...)
#'     }
#'   } else {
#'
#'     if (has_re) {
#'       lit <- rstanarm::stan_glmer(
#'         glue("lit_thresholdL2_bin ~ {rhs}"),
#'         data = lit_dat,
#'         family = binomial(),
#'         weights = weights,
#'         chains = 2, iter = 2000, ...)
#'
#'       num <- rstanarm::stan_glmer(
#'         glue("num_thresholdL1_bin ~ {rhs}"),
#'         data = num_dat,
#'         family = binomial(),
#'         weights = weights,
#'         chains = 2, iter = 2000, ...)
#'
#'       ict <- rstanarm::stan_glmer(
#'         glue("ict_thresholdEL3_bin ~ {rhs}"),
#'         data = ict_dat,
#'         family = binomial(),
#'         weights = weights,
#'         chains = 2, iter = 2000, ...)
#'     } else {
#'       lit <- rstanarm::stan_glm(
#'         glue("lit_thresholdL2_bin ~ {rhs}"),
#'         data = lit_dat,
#'         family = binomial(),
#'         weights = weights,
#'         chains = 2, iter = 2000, ...)
#'
#'       num <- rstanarm::stan_glm(
#'         glue("num_thresholdL1_bin ~ {rhs}"),
#'         data = num_dat,
#'         family = binomial(),
#'         weights = weights,
#'         chains = 2, iter = 2000, ...)
#'
#'       ict <- rstanarm::stan_glm(
#'         glue("ict_thresholdEL3_bin ~ {rhs}"),
#'         data = ict_dat,
#'         family = binomial(),
#'         weights = weights,
#'         chains = 2, iter = 2000, ...)
#'     }
#'   }
#'
#'   if (save) {
#'
#'     if (nchar(year_suffix) > 0) {
#'       year_suffix <- paste0("_", year_suffix)
#'     }
#'
#'     save(lit, num, ict,
#'          file = here::here(glue::glue(
#'            "data/{model_type}{year_suffix}_fits.RData")))
#'   }
#'
#'   tibble::lst(lit, num, ict)
#' }
