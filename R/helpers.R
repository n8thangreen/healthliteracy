# helper functions

#
clean_names <- function(x, col_name = "var_name") {
  x |>
    mutate(
      var_tmp = gsub(.data[[col_name]], pattern = "_", replacement = " "),
      var_tmp = gsub("^(.)", "\\U\\1", tolower(var_tmp), perl = TRUE),
      var_tmp = gsub(var_tmp, pattern = "Imd", replacement = "IMD"),
      var_tmp = gsub(var_tmp, pattern = "Uk", replacement = "UK")) |>
    select(-!!sym(col_name)) |>  # replace the original column
    rename(!!sym(col_name) := var_tmp)
}
