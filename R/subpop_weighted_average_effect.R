
#' Weighted average treatment effect of treated by size of subpopulation
#'
subpop_weighted_average_effect <- function(att_data, mrp_data) {

  names_vars <- names(att_data)

  swate <- att_data

  for (i in names_vars) {
    # population proportion of each level
    prop_levels <- tapply(mrp_data$product_p, mrp_data[[i]], sum)

    # weighted average treatment effect
    swate[[i]]$ame_base <-
      prop_levels[att_data[[i]]$pop] * att_data[[i]]$ame_base
  }

  swate
}
