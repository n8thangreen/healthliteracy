
#' Conditional Average Treatment Effect (CATE)
#'
all_cate <- function(fit,
                     survey_data,
                     mrp_data,
                     save = FALSE) {
  if (inherits(fit, "stanreg")) {
    ext <- "_stan"
  } else {
    ext <- ""
  }

  names_vars <- all.vars(terms(fit)[[3]])
  cate_data <- list()

  ##TODO: fix error for gross_income
  for (i in names_vars) {
    cate_data[[i]] <-
      cond_marginal_effect(fit,
                           survey_data,
                           mrp_data,
                           interaction = i)
  }

  if (save)
    save(cate_data,
         file = here::here(glue::glue("data/cate_data{ext}.RData")))

  cate_data
}
