# plotting.R
# -----------
# This script creates four plots:
# 1. Average Savings Rate by Citizenship over time (y-axis limited to -15 to 15 in increments of 2.5).
# 2. Average Household Size (RHNUMPER) by Citizenship over time.
# 3. Household-Size Adjusted Average Savings Rate by Citizenship over time (adjusted by RHNUMPER; y-axis -15 to 15 in increments of 2.5).
# 4. Average Savings Rate by Ethnicity (Hispanic vs. Non-Hispanic) over time (y-axis -15 to 15 in increments of 2.5).
#
# All plots are saved in the folder "./Plots/" with descriptive filenames ending with "feb13".
#
# Assumptions: SIPP_savings contains the variables:
#   year, SR, citizenship, RHNUMPER, EORIGIN.
# For ethnicity, we assume EORIGIN==2 indicates Hispanic; all others are Non-Hispanic.

library(tidyverse)
setwd(rstudioapi::getActiveProject())
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
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 2.5)) +
  scale_x_continuous(breaks = unique(SIPP_savings$year)) +  # Ensure all years are displayed
  labs(title = "Average Savings Rate by Citizenship over Time",
       subtitle = "Savings Rate (%) (y-axis limited to -15 to 15, increments of 2.5)",
       x = "Year",
       y = "Average Savings Rate (%)",
       color = "Citizenship") +
  theme_minimal()

ggsave("Plots/avg_savings_rate.png", plot = avg_SR_plot, width = 8, height = 6)

#### Plot 2: Average Household Size (RHNUMPER) by Citizenship ####
avg_RHNUMPER_plot <- SIPP_savings %>%
  group_by(year, citizenship) %>%
  summarize(avg_RHNUMPER = mean(RHNUMPER, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = avg_RHNUMPER, color = citizenship)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(SIPP_savings$year)) +  # Ensure all years are displayed
  labs(title = "Average Household Size (RHNUMPER) by Citizenship over Time",
       subtitle = "Household Size (Number of People)",
       x = "Year",
       y = "Average RHNUMPER",
       color = "Citizenship") +
  theme_minimal()

ggsave("Plots/avg_RHNUMPER.png", plot = avg_RHNUMPER_plot, width = 8, height = 6)

#### Plot 3: Household-Size Adjusted Average Savings Rate by Citizenship ####
# Adjusting SR by household size (RHNUMPER)
adjusted_SR_plot <- SIPP_savings %>%
  group_by(year, citizenship) %>%
  summarize(adjusted_avg_SR = sum(SR * RHNUMPER, na.rm = TRUE) / sum(RHNUMPER, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = adjusted_avg_SR, color = citizenship)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 2.5)) +
  scale_x_continuous(breaks = unique(SIPP_savings$year)) +  # Ensure all years are displayed
  labs(title = "Household-Size Adjusted Average Savings Rate by Citizenship over Time",
       subtitle = "Adjusted by Household Size (RHNUMPER)",
       x = "Year",
       y = "Household-Size Adjusted Savings Rate (%)",
       color = "Citizenship") +
  theme_minimal()

ggsave("Plots/adjusted_avg_savings_rate.png", plot = adjusted_SR_plot, width = 8, height = 6)

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
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 2.5)) +
  scale_x_continuous(breaks = unique(SIPP_savings$year)) +  # Ensure all years are displayed
  labs(title = "Average Savings Rate by Ethnicity over Time",
       subtitle = "Comparison of Hispanics vs. Non-Hispanics (y-axis limited to -15 to 15)",
       x = "Year",
       y = "Average Savings Rate (%)",
       color = "Ethnicity") +
  theme_minimal()

ggsave("Plots/ethnicity_savings_rate.png", plot = ethnicity_SR_plot, width = 8, height = 6)

#### Clean up the environment
rm(list = setdiff(ls(), c("SIPP_savings", "SIPP_wrangled", "SIPP_combined", "folder_path", "process_annual")))
message("Plots saved successfully.")
setwd("./Scripts/Annual")
