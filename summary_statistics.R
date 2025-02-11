# summary_statistics.R
# ---------------------
# This script generates two sets of summary statistics for the savings rate (SR)
# by citizenship and year. The first set uses the raw (unweighted) SR values,
# and the second uses SR weighted by the household size (RHNUMPER).
# Both summaries are saved as CSV files in the ./Statistics/ folder.

library(tidyverse)

# Create the output folder if it doesn't exist.
if (!dir.exists("Statistics")) {
  dir.create("Statistics", recursive = TRUE)
}

# Unweighted summary statistics by year and citizenship.
unweighted_summary <- SIPP_savings %>%
  group_by(year, citizenship) %>%
  summarize(
    count = n(),
    mean_SR = mean(SR, na.rm = TRUE),
    median_SR = median(SR, na.rm = TRUE),
    Q1 = quantile(SR, 0.25, na.rm = TRUE),
    Q3 = quantile(SR, 0.75, na.rm = TRUE)
  ) %>%
  ungroup()

# Save the unweighted summary.
write_csv(unweighted_summary, "Statistics/unweighted_savings_summary_feb11.csv")

# Weighted summary statistics by year and citizenship.
# Here, the weighted average SR is calculated using RHNUMPER as weights.
weighted_summary <- SIPP_savings %>%
  group_by(year, citizenship) %>%
  summarize(
    count = n(),
    weighted_mean_SR = sum(SR * RHNUMPER, na.rm = TRUE) / sum(RHNUMPER, na.rm = TRUE)
  ) %>%
  ungroup()

# Save the weighted summary.
write_csv(weighted_summary, "Statistics/weighted_savings_summary_feb11.csv")

# (Optional) Clean up temporary objects.
rm(unweighted_summary, weighted_summary)
