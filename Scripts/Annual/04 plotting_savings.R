# plotting.R
# -----------
# This script creates four plots:
# 1. Average Savings Rate by Citizenship over time (y-axis limited to -20 to 20).
# 2. Average Household Size (RHNUMPER) by Citizenship over time.
# 3. Weighted Average Savings Rate (weighted by RHNUMPER) by Citizenship over time (y-axis -20 to 20).
# 4. Average Savings Rate by Ethnicity (Hispanic vs. Non-Hispanic) over time (y-axis -20 to 20).
#
# All plots are saved in the folder "./Plots/" with descriptive filenames ending with "feb11".
#
# Assumptions: SIPP_savings contains the variables:
#   year, SR, citizenship, RHNUMPER, EORIGIN.
# For ethnicity, we assume EORIGIN==2 indicates Hispanic; all others are Non-Hispanic.

library(tidyverse)

# Ensure the output folder exists
if (!dir.exists("Plots")) {
  dir.create("Plots")
}

#### Plot 1: Average Savings Rate by Citizenship ####
avg_SR_plot <- SIPP_savings %>%
  group_by(year, citizenship) %>%
  summarize(avg_SR = mean(SR, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = avg_SR, color = citizenship)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-20, 20)) +
  labs(title = "Average Savings Rate by Citizenship over Time",
       subtitle = "Savings Rate (%) (y-axis limited to -20 to 20)",
       x = "Year",
       y = "Average Savings Rate (%)",
       color = "Citizenship") +
  theme_minimal()

ggsave("Plots/avg_savings_rate_feb13.png", plot = avg_SR_plot, width = 8, height = 6)

#### Plot 2: Average Household Size (RHNUMPER) by Citizenship ####
avg_RHNUMPER_plot <- SIPP_savings %>%
  group_by(year, citizenship) %>%
  summarize(avg_RHNUMPER = mean(RHNUMPER, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = avg_RHNUMPER, color = citizenship)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Average Household Size (RHNUMPER) by Citizenship over Time",
       subtitle = "Household Size (Number of People)",
       x = "Year",
       y = "Average RHNUMPER",
       color = "Citizenship") +
  theme_minimal()

ggsave("Plots/avg_RHNUMPER_feb13.png", plot = avg_RHNUMPER_plot, width = 8, height = 6)

#### Plot 3: Weighted Average Savings Rate by Citizenship ####
# Weighting SR by household size (RHNUMPER)
weighted_SR_plot <- SIPP_savings %>%
  group_by(year, citizenship) %>%
  summarize(weighted_avg_SR = sum(SR * RHNUMPER, na.rm = TRUE) / sum(RHNUMPER, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = weighted_avg_SR, color = citizenship)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-20, 20)) +
  labs(title = "Weighted Average Savings Rate by Citizenship over Time",
       subtitle = "Weighted by Household Size (RHNUMPER); y-axis limited to -20 to 20",
       x = "Year",
       y = "Weighted Average Savings Rate (%)",
       color = "Citizenship") +
  theme_minimal()

ggsave("Plots/weighted_avg_savings_rate_feb13.png", plot = weighted_SR_plot, width = 8, height = 6)

#### Plot 4: Average Savings Rate by Ethnicity (EORIGIN) ####
# Recode EORIGIN into a new variable 'ethnicity'
SIPP_savings <- SIPP_savings %>%
  mutate(ethnicity = if_else(as.character(EORIGIN) == "2", "Hispanic", "Non-Hispanic"))

ethnicity_SR_plot <- SIPP_savings %>%
  group_by(year, ethnicity) %>%
  summarize(avg_SR = mean(SR, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = avg_SR, color = ethnicity)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-20, 20)) +
  labs(title = "Average Savings Rate by Ethnicity over Time",
       subtitle = "Comparison of Hispanics vs. Non-Hispanics (y-axis limited to -20 to 20)",
       x = "Year",
       y = "Average Savings Rate (%)",
       color = "Ethnicity") +
  theme_minimal()

ggsave("Plots/ethnicity_savings_rate_feb13.png", plot = ethnicity_SR_plot, width = 8, height = 6)

#### Clean up the environment
rm(list = setdiff(ls(), c("SIPP_savings", "SIPP_wrangled", "SIPP_combined", "folder_path", "process_annual")))
