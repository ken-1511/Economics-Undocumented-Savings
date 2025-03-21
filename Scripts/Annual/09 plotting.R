library(ggplot2)

# Define a custom, narrower bin width.
binwidth <- 1  # A smaller bin width than before.

# Create a basic histogram plot with the new custom binwidth to extract bin data.
p <- ggplot(data, aes(x = savings_rate)) +
  geom_histogram(aes(y = ..density..), binwidth = binwidth, fill = "lightblue", color = "white") +
  theme_minimal()

# Extract the histogram bin data.
hist_data <- ggplot_build(p)$data[[1]]

# Flag the bin that includes 0 (where xmin <= 0 and xmax > 0).
hist_data$special <- ifelse(hist_data$xmin <= 0 & hist_data$xmax > 0, "zero", "normal")

# Identify the zero bin (if multiple, take the first).
zero_bin <- hist_data[hist_data$special == "zero", ]
if(nrow(zero_bin) > 1) {
  zero_bin <- zero_bin[1, ]
}

# Compute the center of the zero bin.
zero_center <- (zero_bin$xmin + zero_bin$xmax) / 2

# Create the final plot.
final_plot <- ggplot() +
  # Recreate the histogram using precomputed bin data with the narrower bin width.
  geom_bar(data = hist_data, 
           aes(x = x, y = density, fill = special), 
           stat = "identity", 
           width = (hist_data$xmax - hist_data$xmin)[1], 
           color = "white", 
           show.legend = TRUE) +
  # Manually set fill colors: special bin in gold, others in lightblue.
  scale_fill_manual(name = "Bin Type", 
                    values = c("zero" = "gold", "normal" = "lightblue"),
                    labels = c("Histogram", "zero ± 0.5% SR")) +
  # Overlay the density curve.
  geom_density(data = data, 
               aes(x = savings_rate, color = "Density"), 
               size = 1, 
               show.legend = TRUE) +
  # Overlay the normal density curve.
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$savings_rate, na.rm = TRUE),
                            sd = sd(data$savings_rate, na.rm = TRUE)),
                aes(color = "Normal"), 
                size = 1.5, 
                show.legend = TRUE) +
  # Manually set the colors for the curves.
  scale_color_manual(name = "Curves", 
                     values = c("Density" = "blue", "Normal" = "red")) +
  labs(title = "        Figure 1\n                   Savings Rate Density vs. Normal Distribution",
       x = "Savings Rate",
       y = "Density") +
  theme_minimal()

# Display the plot.
print(final_plot)

# Save the plot to file (update path if needed)
ggsave(filename = paste0(rstudioapi::getActiveProject(), "/Plots/SR_distribution.png"), 
       plot = final_plot, width = 8, height = 6, dpi = 300)
