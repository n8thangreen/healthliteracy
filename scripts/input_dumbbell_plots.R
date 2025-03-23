# dumbbell plots
#
#https://r-charts.com/distribution/dumbbell-plot-ggplot2/

library(ggalt)

# select
# output <- "literacy"
# output <- "numeracy"
output <- "ICT"

colnum <- ifelse(
  output == "literacy", 4,
  ifelse(output == "numeracy", 6, 8))

plot_dat <- data.frame(var = full_table$Variable,
                       cat = full_table$Category,
                       sfl = as.numeric(full_table[, colnum]),
                       pop = as.numeric(full_table[, 10]))

plot_dat <- plot_dat |>
  mutate(pop = ifelse(is.na(pop), 0, pop))

# plot_dat <- plot_dat |>
#   mutate(pop = ifelse(var == "Gross Income (£)" & cat == "Other", 0, pop),
#          pop = ifelse(var == "IMD (decile)" & cat == "7", 0, pop),
#          pop = ifelse(var == "IMD (decile)" & cat == "9", 0, pop))

# downward fill empty variable rows
plot_dat <- plot_dat %>%
  mutate(var = na_if(var, "")) %>%
  tidyr::fill(var, .direction = "down")

p <-
  ggplot(data = plot_dat, aes(y = cat, x = sfl, xend = pop)) +
  geom_dumbbell() +
  # geom_dumbbell(size_x = 3,
  #               size_xend = 3,
  #               colour_x = "#F69541",
  #               colour_xend = "#699DC6") +
  # Plot points separately to handle missing data
  geom_point(aes(x = sfl), size = 4, colour = "#F69541", na.rm = TRUE) +
  geom_point(aes(x = pop), size = 3, shape = 1, stroke = 2, colour = "#699DC6", na.rm = TRUE) +
  ylab("") + xlab("") + theme_bw() + coord_flip() +
  facet_wrap(~ var, scales = "free_x")

ggsave(glue::glue("plots/dumbbell_plot_{output}.png"), p, width = 10, height = 10, units = "in", dpi = 300)
