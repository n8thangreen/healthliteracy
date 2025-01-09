# tables

#
ame_table <- function(ame_data) {

  # process each dataset
  ame_dat_ls <- list()

  for (plot_name in names(ame_data)) {
    ps_var <- ame_data[[plot_name]]

    for (i in names(ps_var)) {
      ame_dat_ls[[paste0(plot_name, "_", i)]] <-
        ps_var[[i]] |>
        group_by(name) |>
        summarise(mean_value = mean(ame_base, na.rm = TRUE),
                  upper = quantile(ame_base, 0.975),
                  lower = quantile(ame_base, 0.025)) |>
        mutate(variable = i,
               var_name = paste0(variable, "_", name),
               group = plot_name) |>
        filter(mean_value != 0)
    }
  }

  # combine the data into a single data frame
  ame_plot_dat <- do.call(rbind, ame_dat_ls)

  # Create a new column to format the value with range
  data_formatted <- ame_plot_dat %>%
    mutate(
      value_with_range = sprintf("%.3f [%.3f, %.3f]", mean_value, lower, upper)
    ) %>%
    select(variable, name, group, value_with_range)

  # Prepare the rows where we have just the variable names with empty values
  variable_rows <- data_formatted %>%
    distinct(variable) %>%
    mutate(
      name = "",  # Empty name for variable rows
      value_with_range = "",  # Empty values for variable rows
      group = "dummy"  # Empty group for variable rows
    )

  # Prepare the rows for the names (subcategories) under each variable
  name_rows <- data_formatted %>%
    mutate(
      name = paste0("  ", name)  # Indent the names
    )

  # Combine variable rows and name rows
  table <- bind_rows(variable_rows, name_rows) %>%
    arrange(variable, name)  # Ensure the order is variable first, then names

  # Pivot the table so that 'group' becomes columns and each value has the corresponding data
  table_wide <- table %>%
    tidyr::pivot_wider(
      names_from = group,
      values_from = value_with_range
    )

  table_wide <- table_wide %>%
    mutate(
      variable = if_else(duplicated(variable), "", variable), # Blank out repeated variable
      name = if_else(duplicated(variable), paste0("  ", name), name) # Indent subcategory names
    )

  table_wide$variable <- ifelse(table_wide$variable == "", table_wide$name, table_wide$variable)
  table_wide <- select(table_wide, -dummy, -name)

  table_wide
}

