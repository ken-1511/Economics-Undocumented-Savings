# sipp_match.R
# -----------
# This script computes the savings rate (SR) for each household (main respondent)
# that has more than one year of data.
#
# For each household (identified by SSUID), we first order the records by year.
# We then compute:
#   FVS = adjusted_net_worth_current - 1.07 * prev_adjusted_net_worth
#
# Next, we compute the average monthly savings (AMS) as the present value equivalent:
#   AMS = FVS / denom, where 
#     denom = (1 - (1 + r)^(-12)) / r, with r = 0.07/12.
#
# Finally, we compute the savings rate (SR) as:
#   SR = (AMS / household_income) * 100.
#
# We then replace any SR above 100 or below –100 with NA.
#
# Only households with at least two years of data are included (i.e. rows with a valid
# previous year's adjusted net worth). The new column 'prev_adjusted_net_worth' is retained
# in the final output.
#
# The final dataset, SIPP_savings, includes all original columns from SIPP_wrangled plus
# the new columns 'SR' and 'prev_adjusted_net_worth'.

library(tidyverse)

# Calculate the present value factor for 12 monthly payments at r = 0.05/12.
denom <- (1 - (1 + 0.05/12)^(-12)) / (0.05/12)

SIPP_savings <- SIPP_wrangled %>%
  # Group by household (SSUID) and retain only households with more than one record.
  group_by(SSUID) %>%
  filter(n() > 1) %>%
  arrange(year) %>% 
  # For each household, create a column for the previous year's adjusted net worth.
  mutate(prev_adjusted_net_worth = lag(adjusted_net_worth)) %>%
  ungroup() %>%
  # Exclude records that do not have a previous year (i.e. earliest record for a household).
  filter(!is.na(prev_adjusted_net_worth)) %>%
  # Compute the net worth change (FVS), then compute AMS and the savings rate (SR).
  mutate(
    AMS = if_else(THVAL_BANK > 0, (adjusted_net_worth - 1.05 * prev_adjusted_net_worth) / denom, ((adjusted_net_worth - prev_adjusted_net_worth) / 12)),
    SR  = AMS / household_income * 100,
    SR  = if_else(SR > 100 | SR < -100, as.numeric(NA), SR)
  ) %>%
  # Filter out records with non-positive time worked.
  filter(TMWKHRS > 0) %>%
  # Remove temporary columns FVS and AMS, but keep prev_adjusted_net_worth.
  select(-AMS)
message("Savings rate computation complete. SIPP_savings dataset created.")
# Clean up the environment, leaving only SIPP_savings, SIPP_wrangled, folder_path, and process_year.
rm(list = setdiff(ls(), c("SIPP_savings", "SIPP_wrangled", "SIPP_combined", "folder_path", "process_year")))
