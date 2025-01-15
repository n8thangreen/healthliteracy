# plotting functions


# bar plot for AME
#
bar_plot <- function(ps_var, title = "", save = FALSE) {

  plot_dat <-
    bind_rows(ps_var, .id = "vars") |>
    group_by(vars, name) |>
    summarise(mean = mean(ame_base, na.rm = TRUE))

  res <-
    plot_dat |>
    ggplot(aes(x = vars, y = mean, fill = name)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title,
         x = "Variable",
         y = "Average Marginal Effect") +
    theme(legend.position = "none") +
    ylim(-0.25, 1.1) +
    coord_flip()

  if (save) {
    ggsave(plot = res, filename = here::here("plots/bar_plot.png"),
           width = 10, height = 6, dpi = 300, bg = "white")
  }

  res
}

# scatter plot by levels
#
scatter_plot <- function(ps_var, save = FALSE, title = "") {

  names_vars <- names(ps_var)

  plot_ls <- list()

  for (i in names_vars) {
    # Calculate means for each level of 'name'
    means_df <- ps_var[[i]] %>%
      group_by(name) %>%
      summarise(mean_value = mean(value, na.rm = TRUE)) |>
      mutate(lead_name = lead(name),
             lead_mean_value = lead(mean_value)) %>%
      filter(!is.na(lead_name) & !is.na(lead_mean_value))

    plot_ls[[i]] <-
      ps_var[[i]] |>
      ggplot(aes(x = name, y = value)) +
      # add jitter to points
      geom_jitter(width = 0.1, height = 0) +
      # draw gradient line connecting the means
      geom_segment(data = means_df,
                   aes(x = name, xend = lead_name,
                       y = mean_value, yend = lead_mean_value),
                   col = "red") +
      ylab("P(not health literate)") +
      xlab(tools::toTitleCase(stringr::str_replace_all(i, "_", " "))) +
      # ylim(0.4, 0.75) +
      theme_minimal()
  }

  res <- gridExtra::grid.arrange(grobs = plot_ls, ncol = 3, top = title)

  if (save) {
    ggsave(plot = res, filename = here::here(glue::glue("plots/scatter_plot_{title}.png")),
           width = 8, height = 9, dpi = 300, bg = "white")
  }
}

# AME forest plot
#
ame_forest_plot <- function(ps_var, title = "", save = FALSE) {

  names_vars <- names(ps_var)

  ame_dat_ls <- list()

  #
  for (i in names_vars) {
    ame_dat_ls[[i]] <-
      ps_var[[i]] |>
      group_by(name) |>
      summarise(mean_value = mean(ame_base, na.rm = TRUE),
                upper = quantile(ame_base, 0.975),
                lower = quantile(ame_base, 0.025)) |>
      mutate(variable = i,
             var_name = paste0(variable, "_", name)) |>
      filter(mean_value != 0)
  }

  ame_plot_dat <- do.call(rbind, ame_dat_ls)

  res <-
    ggplot(ame_plot_dat, aes(x = var_name, y = mean_value, colour = variable)) +
    geom_point(size = 4) +
    geom_linerange(aes(ymin = lower, ymax = upper), size = 1.3) +
    # geom_errorbar(aes(ymin = lower, ymax = upper)) +
    coord_flip() +
    ylab("Average marginal effect") +
    xlab("") +
    ggtitle(title) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal()

  if (save) {
    ggsave(plot = res, filename = here::here(glue::glue("plots/ame_forest_plot_{title}.png")),
           width = 10, height = 6, dpi = 300, bg = "white")
  }

  res
}

# combined all outcomes on a single forest plot
#
ame_forest_group_plot <- function(ame_data, title = "", save = FALSE) {

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

  res <-
    ggplot(ame_plot_dat, aes(x = var_name, y = mean_value, colour = group)) +
    geom_point(size = 4, position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = lower, ymax = upper), size = 1.3,
                   position = position_dodge(width = 0.5)) +
    coord_flip() +
    ylab("Average marginal effect") +
    xlab("") +
    # ggtitle(title) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    theme(legend.position = "top")

  if (save) {
    ggsave(plot = res, filename = here::here(glue::glue("plots/ame_forest_group_plot.png")),
           width = 9, height = 7, dpi = 300, bg = "white")
  }
}


# rank plots
#
rank_plot <- function(ps_var,
                      max_rank = 5,
                      title = "",
                      save = FALSE) {
  ame_wide <-
    bind_rows(ps_var, .id = "vars") |>
    filter(ame_base != 0) |>
    select(vars, name, variable, ame_base) |>
    group_by(vars, name) |>
    reshape2::dcast(variable ~ vars + name,
                    value.var = "ame_base")
  row_ranks <-
    ame_wide[, -1] |>
    apply(1, rank) |>
    t() |>
    apply(2, \(x) table(factor(x, levels = 1:(ncol(ame_wide) - 1))))

  rank_dat <-
    row_ranks |>
    as_tibble() |>
    mutate(rank = 1:n()) |>
    tidyr::gather(key = "name", value = "count", -rank) |>
    mutate(rank = as.integer(rank))

  # bar plot
  res <-
    rank_dat |>
    filter(count > 0,
           rank <= max_rank) |>
    ggplot(aes(x = rank, y = count, fill = name)) +
    geom_bar(stat = "identity") +
    xlim(0, max_rank) +
    ggtitle(title) +
    theme_minimal() +
    scale_x_discrete(limits = factor(1:max_rank),
                     labels = 1:max_rank)

  if (save) {
    ggsave(filename = here::here(glue::glue("plots/rank_plot_{title}.png")),
           width = 10, height = 6, dpi = 300, bg = "white")
  }

  res
}

#' all outcomes on a single rank stacked bar plot
#'
#' @param abs logical, if TRUE, use marginal effect absolute values
#'
rank_group_plot <- function(ame_data,
                            max_rank = 3,
                            title = "",
                            abs_val = TRUE,
                            save = FALSE) {

  # process each dataset and add a 'group' column
  rank_dat_ls <- list()

  for (plot_name in names(ame_data)) {
    ps_var <- ame_data[[plot_name]]

    ame_wide <-
      bind_rows(ps_var, .id = "vars") %>%
      mutate(ame_base = if (abs_val) abs(ame_base) else ame_base) |>
      filter(ame_base != 0) |>
      select(vars, name, variable, ame_base) |>
      group_by(vars, name) |>
      reshape2::dcast(variable ~ vars + name,
                      value.var = "ame_base")

    row_ranks <-
      ame_wide[, -1] |>  # remove draw id column
      apply(1, rank) |>
      t() |>
      apply(2, \(x) table(factor(x, levels = 1:(ncol(ame_wide) - 1))))

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

  res <-
    combined_rank_dat |>
    filter(count > 0, rank <= max_rank) |>
    ggplot(aes(x = factor(rank), y = count, fill = name)) +
    geom_bar(stat = "identity") +
    ggtitle(title) +
    theme_minimal() +
    facet_wrap(~ group) +
    scale_x_discrete(limits = factor(1:max_rank),
                     labels = 1:max_rank) +
    xlab("Rank") +
    ylab("Count") +
    theme(legend.position = "top")

  if (save) {
    ggsave(plot = res, filename = here::here(glue::glue("plots/rank_group_plot.png")),
           width = 10, height = 6, dpi = 300, bg = "white")
  }

  res
}

#' stacked bar chart with variable on the x-axis
#'
rank_plot_by_var <- function(ps_var,
                             max_rank = 5,
                             title = "",
                             save = FALSE) {
  # process data
  ame_wide <-
    bind_rows(ps_var, .id = "vars") |>
    filter(ame_base != 0) |>
    select(vars, name, variable, ame_base) |>
    group_by(vars, name) |>
    reshape2::dcast(variable ~ vars + name,
                    value.var = "ame_base")
  row_ranks <-
    ame_wide[, -1] |>
    apply(1, rank) |>
    t() |>
    apply(2, \(x) table(factor(x, levels = 1:(ncol(ame_wide) - 1))))

  rank_dat <-
    row_ranks |>
    as_tibble() |>
    mutate(rank = 1:n()) |>
    tidyr::gather(key = "name", value = "count", -rank) |>
    mutate(rank = as.integer(rank))

  res <-
    rank_dat |>
    filter(count > 0, rank <= max_rank) |>
    ggplot(aes(x = name, y = count, fill = factor(rank))) +
    geom_bar(stat = "identity") +
    ggtitle(title) +
    theme_minimal() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d(name = "Rank", option = "mako") +  # discrete colour scale
    xlab("Variable") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # rotate x-axis labels if needed

  if (save) {
    ggsave(filename = here::here(glue::glue("plots/rank_plot_by_var_{title}.png")),
           width = 10, height = 6, dpi = 300, bg = "white")
  }

  res
}



# cumulative rank (SUCRA)
#
sucra_plot <- function(ps_var,
                       max_rank = 5,
                       title = "",
                       save = FALSE) {
  # this is duplication of rank_plot
  ame_wide <-
    bind_rows(ps_var, .id = "vars") |>
    filter(ame_base != 0) |>
    select(vars, name, variable, ame_base) |>
    group_by(vars, name) |>
    reshape2::dcast(variable ~ vars + name,
                    value.var = "ame_base")
  row_ranks <-
    ame_wide[, -1] |>
    apply(1, rank) |>
    t() |>
    apply(2, \(x) table(factor(x, levels = 1:(ncol(ame_wide) - 1))))

  rank_dat <-
    row_ranks |>
    as_tibble() |>
    mutate(rank = 1:n()) |>
    tidyr::gather(key = "name", value = "count", -rank) |>
    mutate(rank = as.integer(rank))
  ####

  sucra <-
    rank_dat |>
    group_by(name) |>
    mutate(sucra = cumsum(count),
           sucra = sucra / max(sucra))

  res <-
    sucra |>
    group_by(name) |>
    filter(rank <= max_rank) |>
    filter(!all(sucra == 0)) |>
    mutate(name = as.factor(name),
           name = droplevels(name)) |>
    ggplot(aes(x = rank, y = sucra, colour = name)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    ggtitle(title) +
    ylab("Probability ranking or higher") +
    theme_minimal() +
    scale_x_discrete(limits = factor(1:max_rank),
                     labels = 1:max_rank)

  if (save) {
    ggsave(plot = res, filename = here::here(glue::glue("plots/sucra_plot_{title}.png")),
           width = 10, height = 6, dpi = 300, bg = "white")
  }

  res
}

# all outcomes on a single cumulative sucra plot
#
sucra_group_plot <- function(ame_data,
                             max_rank = 3,
                             title = "",
                             threshold = 0.5,
                             neg_ame = FALSE,
                             abs_val = FALSE,
                             save = FALSE) {

  ### duplicated from rank_group_plot
  rank_dat_ls <- list()

  for (plot_name in names(ame_data)) {
    ps_var <- ame_data[[plot_name]]

    ame_wide <-
      bind_rows(ps_var, .id = "vars") %>%
      mutate(ame_base = if (neg_ame) -ame_base else ame_base,
             ame_base = if (abs_val) -abs(ame_base) else ame_base) |>  # ranks the largest first
      filter(ame_base != 0) |>
      select(vars, name, variable, ame_base) |>
      group_by(vars, name) |>
      reshape2::dcast(variable ~ vars + name,
                      value.var = "ame_base")
    row_ranks <-
      ame_wide[, -1] |>
      apply(1, rank) |>
      t() |>
      apply(2, \(x) table(factor(x, levels = 1:(ncol(ame_wide) - 1))))

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

  res <-
    sucra |>
    group_by(name, group) |>
    filter(rank <= max_rank) |>
    filter(!all(sucra <= threshold)) |>
    mutate(name = as.factor(name),
           name = droplevels(name)) |>
    ggplot(aes(x = rank, y = sucra, colour = name)) +
    facet_wrap(~ group) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    ggtitle(title) +
    ylab("Probability ranking or higher") +
    theme_minimal() +
    scale_x_discrete(limits = factor(1:max_rank),
                     labels = 1:max_rank)

  if (save) {
    ggsave(plot = res, filename = here::here(glue::glue("plots/sucra_group_plot.png")),
           width = 10, height = 6, dpi = 300, bg = "white")
  }

  res
}


