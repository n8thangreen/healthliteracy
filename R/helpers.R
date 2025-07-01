# helper functions

#' Clean names
#' @export
#'
clean_names <- function(x, col_name = "var_name") {
  x |>
    mutate(
      var_tmp = .data[[col_name]],
      var_tmp = gsub(pattern = "_", replacement = " ", var_tmp),
      # Option 1: Capture and uppercase LITERAL SPACES + first letter
      var_tmp = gsub("^(  *.)", "\\U\\1", tolower(var_tmp), perl = TRUE),
      var_tmp = gsub("^(.)", "\\U\\1", var_tmp, perl = TRUE),
      # Option 2: Capture and uppercase ANY WHITESPACE + first letter (more robust)
      # var_tmp = gsub("^([[:space:]]*.)", "\\U\\1", tolower(var_tmp), perl = TRUE),
      var_tmp = gsub(pattern = "Imd", replacement = "IMD", var_tmp),
      var_tmp = gsub(pattern = "IMD", replacement = "IMD (decile)", var_tmp),
      var_tmp = gsub(pattern = "English lang", replacement = "English language", var_tmp),
      var_tmp = gsub(pattern = "Gross income", replacement = "Gross income (£)", var_tmp),
      var_tmp = gsub(pattern = "Age", replacement = "Age (years)", var_tmp),
      var_tmp = gsub(pattern = "Workingstatus", replacement = "Working status", var_tmp),
      # var_tmp = gsub(pattern = ">=", replacement = "$\\\\geq$", var_tmp),
      # var_tmp = gsub(pattern = "<=", replacement = "$\\\\leq$", var_tmp),
      var_tmp = gsub(pattern = "Uk", replacement = "UK", var_tmp)
    ) |>
    select(-!!sym(col_name)) |>
    rename(!!sym(col_name) := var_tmp) |>
    select(!!sym(col_name), everything())
}
