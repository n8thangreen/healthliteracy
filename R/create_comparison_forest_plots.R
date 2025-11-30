
#' @import ggplot2
#' @import dplyr
#' @import janitor
#' @import forcats
#'
create_comparison_forest_plots <- function(ame_data, ame_data_england) {

  # --- 1. Internal Helper Function to Process Data ---
  # This ensures both datasets are treated identically
  process_ame_list <- function(data_list, source_label) {
    processed_ls <- list()

    # Loop through domains (lit, num, ict)
    for (domain in names(data_list)) {
      domain_data <- data_list[[domain]]

      # Loop through variables inside the domain
      for (var_i in names(domain_data)) {
        processed_ls[[paste0(domain, "_", var_i)]] <-
          domain_data[[var_i]] |>
          group_by(name) |>
          summarise(mean_value = mean(ame_base, na.rm = TRUE),
                    upper = quantile(ame_base, 0.975, na.rm = TRUE),
                    lower = quantile(ame_base, 0.025, na.rm = TRUE)) |>
          mutate(variable = var_i,
                 # Combine variable and category name for the Y axis label
                 var_name = paste0(variable, ": ", name),
                 domain = domain,
                 dataset_source = source_label) |> # Tag the source (England vs Main)
          clean_names() |>
          filter(mean_value != 0)
      }
    }
    return(do.call(rbind, processed_ls))
  }

  # --- 2. Process and Combine Datasets ---
  # CHANGE 1: Update the label here
  df_main <- process_ame_list(ame_data, "AME Newham")
  df_eng  <- process_ame_list(ame_data_england, "AME England")

  all_plot_data <- bind_rows(df_main, df_eng) |>
    mutate(var_name = as.factor(var_name))

  # --- 3. Generate a Plot for Each Domain ---
  # We identify the unique domains (Lit, Num, ICT)
  domains <- unique(all_plot_data$domain)
  plot_list <- list()

  for (dom in domains) {

    # Filter data for just this domain
    dom_data <- all_plot_data |> filter(domain == dom)

    # Create the plot
    p <- ggplot(dom_data, aes(x = forcats::fct_rev(var_name),
                              y = mean_value,
                              colour = dataset_source,
                              shape = dataset_source)) +
      geom_point(size = 4, position = position_dodge(width = 0.6)) +
      geom_linerange(aes(ymin = lower, ymax = upper, linetype = dataset_source),
                     size = 1.3,
                     position = position_dodge(width = 0.6)) +

      coord_flip() +
      ylab("MRP-ATE for probability not health literate") +
      xlab("") +
      ggtitle(paste("Domain:", toupper(dom))) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold")
      ) +
      scale_x_discrete(expand = expansion(add = 2)) +

      # CHANGE 2: Update the name in the manual color scale to match exactly
      scale_color_manual(values = c("AME Newham" = "#1f78b4", "AME England" = "#e31a1c")) +

      # Arrows (Adjust y-coordinates if your data scale is different)
      annotate("segment", x = 0.5, xend = 0.5, y = -0.1, yend = -0.3,
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "black") +
      annotate("text", x = 0, y = -0.2, label = "Better", hjust = 0.5, color = "black") +
      annotate("segment", x = 0.5, xend = 0.5, y = 0.1, yend = 0.3,
               arrow = arrow(type = "open", length = unit(0.2, "cm")), color = "black") +
      annotate("text", x = 0, y = 0.2, label = "Worse", hjust = 0.5, color = "black")

    plot_list[[dom]] <- p
  }

  return(plot_list)
}
