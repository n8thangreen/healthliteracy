# helper functions

#
clean_names <- function(x, col_name = "var_name") {
  x |>
    mutate(
      var_name = gsub(.data[[col_name]], pattern = "_", replacement = " "),
      var_name = gsub("^(.)", "\\U\\1", tolower(var_name), perl = TRUE),
      var_name = gsub(var_name, pattern = "Imd", replacement = "IMD"),
      var_name = gsub(var_name, pattern = "Uk", replacement = "UK")) |>
    select(-!!sym(col_name)) |>  # replace the original column
    rename(!!sym(col_name) := var_name)
}
