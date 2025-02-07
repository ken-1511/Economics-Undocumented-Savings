# plotting.R
# -----------
# This script creates a plot of the average savings rate (SR) over time.
# - X-axis: Year
# - Y-axis: Average Savings Rate (with limits from -100 to 100)
# - Color-coded by citizenship.
# Dots represent the average savings rate for each year, with lines connecting dots of the same citizenship.

library(tidyverse)

# Compute average savings rate by year and citizenship
avg_SR <- SIPP_savings %>%
  group_by(year, citizenship) %>%
  summarize(avg_SR = mean(SR, na.rm = TRUE)) %>%
  ungroup()

# Create the plot
plot_SR <- ggplot(avg_SR, aes(x = year, y = avg_SR, color = citizenship)) +
  geom_line(size = 1) +           # Connect points of the same citizenship
  geom_point(size = 3) +          # Draw dots for each average
  scale_y_continuous(limits = c(-100, 100)) +
  labs(
    title = "Average Savings Rate by Citizenship Over Time",
    x = "Year",
    y = "Average Savings Rate (%)",
    color = "Citizenship"
  ) +
  theme_minimal()

# Display the plot
print(plot_SR)

# Optionally, save the plot as a PNG file (adjust width/height as needed)
ggsave("./average_SR_by_year.png", plot = plot_SR, width = 8, height = 6)

