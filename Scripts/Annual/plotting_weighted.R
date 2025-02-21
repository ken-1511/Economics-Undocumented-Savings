#### Plot 3: Household-Size Adjusted Average Savings Rate by Citizenship (from SIPP_weighed) ####
adjusted_SR_plot <- SIPP_weighted %>%
  ggplot(aes(x = year, y = weighted_mean, color = citizenship)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 2.5)) +
  scale_x_continuous(breaks = unique(SIPP_weighted$year)) +  # Ensure all years are displayed
  labs(title = "Household-Size Adjusted Average Savings Rate by Citizenship over Time (Weighted)",
       x = "Year",
       y = "Household-Size Adjusted Savings Rate (%)",
       color = "Citizenship") +
  theme_minimal()

ggsave("Plots/adjusted_avg_savings_rate_weighted.png", plot = adjusted_SR_plot, width = 8, height = 6)

#### Plot 3: Average Savings Rate by Citizenship (from SIPP_weighed) ####
adjusted_SR_plot <- SIPP_weighted %>%
  ggplot(aes(x = year, y = weighted_mean, color = citizenship)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 2.5)) +
  scale_x_continuous(breaks = unique(SIPP_weighted$year)) +  # Ensure all years are displayed
  labs(title = "Average Savings Rate by Citizenship over Time (Weighted)",
       x = "Year",
       y = "Adjusted Savings Rate (%)",
       color = "Citizenship") +
  theme_minimal()

ggsave("Plots/avg_savings_rate_weighted.png", plot = adjusted_SR_plot, width = 8, height = 6)
