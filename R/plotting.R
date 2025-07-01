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
    # calculate means for each level of 'name'
    means_df <- ps_var[[i]] %>%
      mutate(name = factor(name,
                           levels = unique(name))) |>
      group_by(name) %>%
      summarise(mean_value = mean(value, na.rm = TRUE)) |>
      mutate(lead_name = lead(name),
             lead_mean_value = lead(mean_value)) %>%
      filter(!is.na(lead_name) & !is.na(lead_mean_value))

    upp_low <- ps_var[[i]] %>%
      group_by(name) %>%
      summarise(mean_value = mean(value, na.rm = TRUE),
                l95 = quantile(value, probs = 0.025),
                u95 = quantile(value, probs = 0.975),
                l50 = quantile(value, probs = 0.25),
                u50 = quantile(value, probs = 0.75))

    upp_low$name <- factor(upp_low$name, levels = unique(upp_low$name))
    ps_var[[i]]$name <- factor(ps_var[[i]]$name, levels = unique(ps_var[[i]]$name))

    upp_low$name_numeric <- as.numeric(upp_low$name)
    ps_var[[i]]$name_numeric <- as.numeric(ps_var[[i]]$name)

    plot_ls[[i]] <-
      ps_var[[i]] |>
      ggplot() +
      # add jitter to points
      geom_jitter(data = ps_var[[i]], aes(x = name_numeric, y = value),
                  width = 0.1, height = 0) +
      # # draw gradient line connecting the means
      geom_segment(data = means_df,
                   aes(x = as.numeric(name), xend = as.numeric(lead_name),
                       y = mean_value, yend = lead_mean_value),
                   col = "red", inherit.aes = FALSE, linewidth = 1.2) +
      # draw ribbon for the 95% CI
      geom_ribbon(data = upp_low,
                  aes(x = name_numeric, ymin = l95, ymax = u95),
                  fill = "firebrick", alpha = 0.2, inherit.aes = FALSE) +
      geom_ribbon(data = upp_low,
                  aes(x = name_numeric, ymin = l50, ymax = u50),
                  fill = "firebrick4", alpha = 0.2, inherit.aes = FALSE) +
      scale_x_continuous(breaks = seq_along(levels(upp_low$name)), labels = levels(upp_low$name)) +
      ylab("P(not health literate)") +
      xlab(tools::toTitleCase(stringr::str_replace_all(i, "_", " "))) +
      # expand_limits(y = 0) +
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
ame_forest_group_plot <- function(ame_data, title = "", save = FALSE,
                                  filename = "ame_forest_group_plot.png") {

  # process each dataset
  ame_dat_ls <- list()

  for (plot_name in names(ame_data)) {
    ps_var <- ame_data[[plot_name]]

    for (i in names(ps_var)) {
      ame_dat_ls[[paste0(plot_name, "_", i)]] <-
        ps_var[[i]] |>
        group_by(name) |>
        summarise(mean_value = mean(ame_base, na.rm = TRUE),
                  upper = quantile(ame_base, 0.975, na.rm = TRUE),
                  lower = quantile(ame_base, 0.025, na.rm = TRUE)) |>
        mutate(variable = i,
               var_name = paste0(variable, ": ", name),
               # var_name = paste0(""**", variable, "**: ", name),  # bold font
               group = plot_name) |>
        clean_names() |>
        filter(mean_value != 0)
    }
  }

  # combine the data into a single data frame
  ame_plot_dat <- do.call(rbind, ame_dat_ls)

  group_labels <- c("Literacy", "Numeracy", "ICT")

  res <-
    # ggplot(ame_plot_dat, aes(x = name, y = mean_value, colour = group)) +   # when facet dont need variable text
    ggplot(ame_plot_dat, aes(x = var_name, y = mean_value, colour = group, linetype = group)) +
    geom_point(aes(shape = group), size = 4, position = position_dodge(width = 0.8)) +
    geom_linerange(aes(ymin = lower, ymax = upper, colour = group, linetype = group), size = 1.3,
                   position = position_dodge(width = 0.8)) +
    scale_shape_manual(name = "Type", labels = group_labels,
                       values = c(16, 17, 18)) +  # 16=circle, 17=triangle, 18=diamond
    scale_linetype_manual(name = "Type", labels = group_labels,
                          values = c("solid", "dashed", "longdash")) +
    scale_color_manual(
      name = "Type",
      labels = group_labels,
      values = c("red", "blue", "darkgreen")) +
    coord_flip() +
    # facet_grid(variable ~ ., scales = "free", space = "free") +   # grid
    # facet_wrap(~ variable, scales = "free_y", ncol = 2) +         # warp
    ylab("MRP-ATE for probability not health literate") +
    xlab("") +
    # ggtitle(title) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    theme(legend.position = "top") +
    # strip.text = element_markdown(size = 10, face = "plain"),
    scale_x_discrete(expand = expansion(add = 2)) +    # add extra space
    annotate("segment", x = 0, xend = 0, y = -0.1, yend = -0.3,
             arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "black") +
    annotate("text", x = -0.5, y = -0.2, label = "Better", hjust = 0.5, color = "black") +
    annotate("segment", x = 0, xend = 0, y = 0.1, yend = 0.3,
             arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "black") +
    annotate("text", x = -0.5, y = 0.2, label = "Worse", hjust = 0.5, color = "black") +
    guides(
      color = guide_legend(
        override.aes = list(
          shape = c(16, 17, 18),         # Specify shapes for the legend entries
          linetype = c("solid", "dashed", "longdash"), # Specify linetypes for the legend entries
          size = 4 # Optional: Adjust the size of points in the legend
        ),
        order = 1 # Optional: Ensure this combined legend is the first/main one
      ),
      shape = "none", # Hide the separate shape legend
      linetype = "none" # Hide the separate linetype legend
    )

  if (save) {
    ggsave(plot = res, filename = here::here(glue::glue("plots/{filename}")),
           width = 9, height = 7, dpi = 300, bg = "white")
  }

  res
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
    mutate(rank = as.integer(rank)) |>
    clean_names(col_name = "name")

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
             group = plot_name) |>  # Add group identifier
      clean_names(col_name = "name")

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
    mutate(rank = as.integer(rank)) |>
    clean_names(col_name = "name")

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
#' @param ame_data
#' @param max_rank
#' @param title
#' @param threshold
#' @param neg_ame logical
#' @param abs_val logical
#' @param save logical
#' @param filename string
#'
#' @returns
#' @export
#'
#' @examples
sucra_group_plot <- function(ame_data,
                             max_rank = 3,
                             title = "",
                             threshold = 0.5,
                             neg_ame = FALSE,
                             abs_val = FALSE,
                             save = FALSE,
                             filename = "sucra_group_plot.png") {

  ### duplicated from rank_group_plot
  rank_dat_ls <- list()

  for (plot_name in names(ame_data)) {
    ps_var <- ame_data[[plot_name]]

    ##TODO: problem with pop and name column
    ##      for att job status has length 2 why?
    ame_wide <-
      bind_rows(ps_var, .id = "vars") %>%
      mutate(ame_base = if (neg_ame) -ame_base else ame_base,
             ame_base = if (abs_val) -abs(ame_base) else ame_base) |>  # ranks the largest first
      filter(ame_base != 0) |>     # remove comparison with itself
      ungroup() |>
      select(vars, name, variable, ame_base) |>
      # group_by(vars, name) |>
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
             group = plot_name) |> # add group identifier
      clean_names(col_name = "name")

    rank_dat_ls[[plot_name]] <- rank_dat
  }

  # combine all datasets into a single data frame
  combined_rank_dat <- bind_rows(rank_dat_ls)
  ###

  sucra <-
    combined_rank_dat |>
    group_by(name, group) |>
    mutate(sucra = cumsum(count),
           sucra = sucra / max(sucra)) |>
    ungroup() |>
    mutate(
      name = ifelse(
        grepl("Age \\(years\\)", name), "Age (years) >=45", name),
      name = ifelse(
        grepl("Gross income \\(£\\) \\$", name), "Gross income (£) >=10000", name),
      name = ifelse(
        grepl("Qualification", name), "Qualification >=level 2", name))

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
    ylim(0,1) +
    theme_minimal() +
    scale_x_discrete(limits = factor(1:max_rank),
                     labels = 1:max_rank)

  if (save) {
    ggsave(plot = res, filename = here::here(glue::glue("plots/{filename}")),
           width = 10, height = 6, dpi = 300, bg = "white")
  }

  res
}


