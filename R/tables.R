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

#
sucra_table <- function(ame_data) {

  ### duplicated from rank_group_plot
  rank_dat_ls <- list()

  for (plot_name in names(ame_data)) {
    ps_var <- ame_data[[plot_name]]

    xx <-
      bind_rows(ps_var, .id = "vars") |>
      filter(ame_base != 0) |>
      select(vars, name, variable, ame_base) |>
      group_by(vars, name) |>
      reshape2::dcast(variable ~ vars + name,
                      value.var = "ame_base")

    row_ranks <-
      xx[, -1] |>
      apply(1, rank) |>
      t() |>
      apply(2, \(x) table(factor(x, levels = 1:(ncol(xx) - 1))))

    rank_dat <-
      row_ranks |>
      as_tibble() |>
      mutate(rank = 1:n()) |>
      tidyr::gather(key = "name", value = "count", -rank) |>
      mutate(rank = as.integer(rank),
             group = plot_name) # Add group identifier

    rank_dat_ls[[plot_name]] <- rank_dat
  }

  # combine all datasets into a single data frame
  combined_rank_dat <- bind_rows(rank_dat_ls)
  ###

  sucra <-
    combined_rank_dat |>
    group_by(name, group) |>
    mutate(sucra = cumsum(count),
           sucra = sucra / max(sucra))

  sucra_table <- sucra %>%
    filter(rank != max(rank)) |>
    group_by(group, name) %>%
    summarize(
      SUCRA = round(sum(sucra) / (max(rank) - 1) * 100, 0),
      .groups = "drop") |>
    tidyr::pivot_wider(names_from = name, values_from = SUCRA)

  final_table <- sucra %>%
    mutate(p = count/max(rank)) |>
    select(-count, -sucra) |>
    pivot_wider(names_from = name, values_from = p) %>%
    bind_rows(sucra_table)

  grouped_tables <- sucra %>%
    mutate(p = count / max(rank)) %>%
    select(-count, -sucra) %>%
    group_by(group) %>%
    group_split() |>
    purrr::map(~ pivot_wider(.x, names_from = name, values_from = p) %>%
          bind_rows(sucra_table))
}

