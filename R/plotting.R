# plotting functions


# bar plot
#
bar_plot <- function(ps_var) {

  plot_dat <-
    bind_rows(ps_var, .id = "vars") |>
    group_by(vars, name) |>
    summarise(mean = mean(ame_base, na.rm = TRUE))

  plot_dat |>
    ggplot(aes(x = vars, y = mean, fill = name)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Average Marginal Effect",
         x = "Variable",
         y = "AME") +
    theme(legend.position = "none") +
    coord_flip()
}

# scatter plot by levels
#
scatter_plot <- function(ps_var) {

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
      ylim(0.4, 0.75) +
      theme_minimal()
  }

  gridExtra::grid.arrange(grobs = plot_ls, ncol = 3)
}

# AME forest plot
#
ame_forest_plot <- function(ps_var) {

  ame_dat_ls <- list()

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

  ggplot(ame_plot_dat, aes(x = var_name, y = mean_value, colour = variable)) +
    geom_point(size = 4) +
    geom_linerange(aes(ymin = lower, ymax = upper), size = 1.3) +
    # geom_errorbar(aes(ymin = lower, ymax = upper)) +
    coord_flip() +
    ylab("Average marginal effect") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal()

  ggsave(filename = here::here("plots/ame_plot.png"),
         width = 10, height = 6, dpi = 300, bg = "white")

}

# rank plots
#
rank_plot <- function(ps_var) {

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
    gather(key = "name", value = "count", -rank) |>
    mutate(rank = as.integer(rank))

  # bar plot
  rank_dat |>
    filter(count > 0,
           rank <= 10) |>
    ggplot(aes(x = rank, y = count, fill = name)) +
    geom_bar(stat = "identity") +
    xlim(0, 10) +
    theme_minimal() +
    scale_x_discrete(limits = factor(1:10),
                     labels = 1:10)
}

# cumulative rank (SUCRA)
#
sucra_plot <- function(rank_dat) {

  max_rank <- 10

  sucra <-
    rank_dat |>
    group_by(name) |>
    mutate(sucra = cumsum(count),
           sucra = sucra / max(sucra))

  sucra |>
    group_by(name) |>
    filter(rank <= max_rank) |>
    filter(!all(sucra == 0)) |>
    mutate(name = as.factor(name),
           name = droplevels(name)) |>
    ggplot(aes(x = rank, y = sucra, colour = name)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    ylab("Probability ranking or higher") +
    theme_minimal() +
    scale_x_discrete(limits = factor(1:max_rank),
                     labels = 1:max_rank)
}

