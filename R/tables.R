# tables of model outputs


#' @title Average marginal effect table
#'
ame_table <- function(ame_data) {

  # process each data set
  ame_dat_ls <- list()

  for (plot_name in names(ame_data)) {
    ps_var <- ame_data[[plot_name]]

    for (i in names(ps_var)) {
      ame_dat_ls[[paste0(plot_name, "_", i)]] <-
        ps_var[[i]] |>
        group_by(name) |>
        summarise(mean_value = mean(ame_base, na.rm = TRUE),  # posterior summaries
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
      value_with_range = sprintf("%.3f [%.3f, %.3f]", mean_value, lower, upper)) %>%
    select(variable, name, group, value_with_range)

  # prepare rows where we have just variable names with empty values
  variable_rows <- data_formatted %>%
    distinct(variable) %>%
    mutate(
      name = "",              # empty name for variable rows
      value_with_range = "",  # empty values for variable rows
      group = "dummy"         # empty group for variable rows
    )

  # prepare rows for the names (subcategories) under each variable
  name_rows <- data_formatted %>%
    mutate(
      name = paste0("  ", name)  # indent the names
    )

  # combine variable rows and name rows
  table <- bind_rows(variable_rows, name_rows) %>%
    arrange(variable, name)  # ensure order is variable first then names

  # pivot table so 'group' becomes columns and each value has corresponding data
  table_wide <- table %>%
    tidyr::pivot_wider(
      names_from = group,
      values_from = value_with_range
    )

  table_wide <- table_wide %>%
    mutate(
      variable = if_else(duplicated(variable), "", variable),     # blank out repeated variable
      name = if_else(duplicated(variable), paste0("  ", name), name) # indent subcategory names
    )

  table_wide$variable <- ifelse(table_wide$variable == "",
                                yes = table_wide$name,
                                no = table_wide$variable)

  table_wide <- select(table_wide, -dummy, -name)

  table_wide
}

#' @title Ranks SUCRA summary table
#'
#' Includes cumulative rank, SUCRA and expected rank
#'
#' @param ame_data
#' @param max_rank An integer specifying the maximum rank to display on the x-axis.
#'   Defaults to `NA` for all ranks.
#' @param threshold A numeric value between 0 and 1. Variables that never achieve a
#'   cumulative rank value above this threshold will be filtered out to declutter the plot.
#'   Defaults to `0`.
#' @param abs_val A logical value. If `TRUE`, ranking is based on the absolute magnitude
#'   of `ame_base` (largest absolute value gets the best rank). Defaults to `FALSE`.
#'
#' @return data.frame
#'
sucra_table <- function(ame_data,
                        max_rank = NA,
                        threshold = 0,
                        abs_val = TRUE) {
  ### duplicated from rank_group_plot
  rank_dat_ls <- list()

  for (plot_name in names(ame_data)) {
    ps_var <- ame_data[[plot_name]]

    xx <-
      bind_rows(ps_var, .id = "vars") |>
      mutate(ame_base = if (abs_val) -abs(ame_base) else ame_base) |>  # ranks the largest first
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
             group = plot_name)  # group identifier

    rank_dat_ls[[plot_name]] <- rank_dat
  }

  # combine all datasets into a single data frame
  combined_rank_dat <- bind_rows(rank_dat_ls)
  ###

  # all ranks
  if (is.na(max_rank)) {
    max_rank <- max(combined_rank_dat$rank)
  }

  # calculate cumulative rank
  cumrank_long <-
    combined_rank_dat |>
    group_by(name, group) |>
    mutate(cumrank = cumsum(count),
           cumrank = cumrank / max(cumrank),
           p = count/max(rank)) |>
    select(-count)

  # calculate SUCRA
  sucra_long <- cumrank_long |>
    filter(rank != max(rank)) |>
    group_by(group, name) |>
    summarize(
      SUCRA = round(sum(cumrank) / max(rank) * 100, 0),
      rank_hat = max(rank) - round(sum(cumrank)) + 1,
      .groups = "drop")

  final_table <- sucra_long |>
    pivot_wider(names_from = group, values_from = c(SUCRA, rank_hat))

  # format and clean names
  final_table <- final_table |>
    # split by last underscore
    mutate(name = sub("_(?!.*_)", "@@", name, perl = TRUE)) %>%
    separate(name, into = c("Variable", "Category"), sep = "@@") |>
    select(Variable, Category, everything()) |>
    # replace underscores with spaces
    mutate(Category = gsub("_", " ", Category),
           Variable = gsub("_", " ", Variable),
           # start with capital letter
           Variable = ifelse(stringr::str_starts(Variable, "\\$"), Variable, stringr::str_to_title(Variable)),
           Category = ifelse(stringr::str_starts(Category, "\\$"), Category, stringr::str_to_title(Category)),
           Variable = gsub("Uk", "UK", Variable),
           Category = gsub("Bme", "BME", Category),
           Variable = gsub("Imd", "IMD", Variable),
           Variable = gsub("Workingstatus", "Working Status", Variable),
           Variable = gsub("English Lang", "English Language", Variable),
           # include units
           Variable = gsub("Gross Income", "Gross Income (£)", Variable),
           Variable = gsub("IMD", "IMD (quintile)", Variable),
           Variable = gsub("Age", "Age (years)", Variable),
           Category = gsub("<=", "$\\\\leq$", Category),
           Category = gsub(">=", "$\\\\geq$", Category),
           Category = gsub("<", "$<$", Category),
           Category = gsub(">", "$>$", Category),
           Variable = if_else(duplicated(Variable), "", Variable))  # blank out repeated variable

  final_table
}

