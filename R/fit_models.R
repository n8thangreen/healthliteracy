#' Fit a single model
#'
#' Helper function to fit one model. It detects if 'dat' is a 'mids' object
#' and calls brms::brm_multiple, or if 'dat' is a data.frame and calls
#' the specified 'model_fun'.
#'
#' @param dat The data to use for modelling (either a data.frame or a mids object).
#' @param outcome_var A string, the name of the outcome variable.
#' @param formula_rhs A string, the right-hand side of the model formula.
#' @param model_fun Modelling function to call (e.g., stan_glmer).
#'                  Ignored if 'dat' is a 'mids' object.
#' @param common_args A list of common arguments for the model function.
#' @param ... Additional arguments passed to the modelling function.
#' @return A fitted model object.
#'
fit_model <- function(dat, outcome_var, formula_rhs, model_fun, common_args, ...) {

  model_args <- list(data = dat)

  all_args <- c(model_args, common_args, list(...))

  if (inherits(dat, "mids")) {

    full_formula_str <- glue::glue("{outcome_var} | weights(weights) ~ {formula_rhs}")
    all_args$formula <- stats::as.formula(full_formula_str)
    all_args$family <- brms::bernoulli()

    all_args$combine <- TRUE        # pool models
    all_args$backend <- "cmdstanr"  # default is rstan

    if (is.null(all_args$chains)) all_args$chains <- 2
    if (is.null(all_args$iter)) all_args$iter <- 2000

    do.call(brms::brm_multiple, all_args)

  } else if (inherits(dat, "data.frame")) {

    full_formula_str <- glue::glue("{outcome_var} ~ {formula_rhs}")
    all_args$formula <- stats::as.formula(full_formula_str)

    all_args$weights <- quote(weights)

    all_args$family <- binomial()

    # Data is a single data.frame, use the provided 'model_fun'
    do.call(model_fun, all_args)

  } else {
    stop("Data for 'fit_model' must be a 'data.frame' or 'mids' object.", call. = FALSE)
  }
}


#' Fit models to all outcomes
#'
#' Dynamically handles 'survey_data' list containing data.frames or 'mids' objects.
#'
#' @param survey_data A list containing data.frames or mids objects named 'lit',
#'                    'num', and optionally 'ict'.
#' @param stan Boolean, whether to use rstanarm (TRUE) or stats/lme4 (FALSE).
#'             Ignored if 'survey_data' elements are 'mids' objects.
#' @param save Boolean, whether to save the fitted models to an .RData file.
#' @param year_suffix A string suffix for the saved file (e.g., "2003"). This is used to determine the list of covariates.
#' @param algorithm A string specifying the algorithm to use (e.g. "meanfield", "fullrank"). Defaults to "sampling".
#' @param ... Additional arguments passed to the modelling functions
#'            (e.g., control, chains, iter).
#' @return A named list containing the fitted models ('lit', 'num', 'ict').
#'
fit_all_models <- function(survey_data, stan = TRUE, save = FALSE, year_suffix = "", algorithm = "sampling", ...) {

  lit_dat <- survey_data$lit
  num_dat <- survey_data$num
  ict_dat <- survey_data$ict # NULL if survey_data$ict doesn't exist

  if (inherits(lit_dat, "mids")) {
    model_type <- "brms"
  } else if (stan) {
    model_type <- "stan"
  } else {
    model_type <- "freq"
  }

  if (year_suffix == "2003") {
    fe_names <- c("sex", "age", "ethnicity", "english_lang", "qualification",  # missing uk_born
                  "workingstatus", "job_status", "gross_income", "own_home")
    re_names <- "imd"
  } else if (year_suffix %in% c("", "2011")) {
    fe_names <- c("sex", "age", "ethnicity", "uk_born", "english_lang", "qualification",
                  "workingstatus", "job_status", "gross_income", "own_home")
    re_names <- "imd"
  } else if (year_suffix == "piaac") {
    fe_names <- c("sex", "age", "uk_born", "english_lang", "qualification",
                  "workingstatus")
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

  rhs <- paste("1 +", fe_form, re_form)

  tasks <- list(
    lit = list(data = lit_dat, outcome = "lit_thresholdL2_bin"),
    num = list(data = num_dat, outcome = "num_thresholdL1_bin")
  )

  if (!is.null(ict_dat)) {
    tasks$ict <- list(data = ict_dat, outcome = "ict_thresholdEL3_bin")
  }

  # This 'model_fun' will ONLY be used if data is a data.frame.
  # The 'fit_model' function will ignore it if data is 'mids'.
  model_fun <- if (stan) {
    if (has_re) rstanarm::stan_glmer else rstanarm::stan_glm
  } else {
    if (has_re) lme4::glmer else stats::glm
  }

  # passed to stan_glmer OR brm_multiple
  common_args <- if (stan || model_type == "brms") {
    list(chains = 2,
         iter = 2000,
         algorithm = algorithm)
  } else {
    list()
  }

  # --- run models

  models_list <- lapply(tasks, function(task) {
    fit_model(
      dat = task$data,
      outcome_var = task$outcome,
      formula_rhs = rhs,
      model_fun = model_fun,       # Ignored if dat is 'mids'
      common_args = common_args,   # Passed to both
      ...
    )
  })

  if (save) {
    if (nchar(year_suffix) > 0) {
      year_suffix <- paste0("_", year_suffix)
    }

    file_path <- here::here(glue::glue(
      "data/{model_type}{year_suffix}_fits.RData"
    ))

    list2env(models_list, envir = environment())
    objects_to_save <- names(models_list)
    save(list = objects_to_save, file = file_path)
  }

  models_list
}
