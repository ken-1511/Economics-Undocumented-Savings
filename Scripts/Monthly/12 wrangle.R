# wrangle.R
# -----------
# This script wrangles the SIPP_combined dataset by:
# 1. Creating a new citizenship variable using the specified logic.
# 2. Renaming TFTOTINC to household_income.
# 3. Creating an adjusted_net_worth variable (net worth minus home value plus mortgage debt)
#    while treating any NA values as 0.
# 4. Overwriting PNUM with a unique person ID using paste0(SSUID, "-", PNUM).
# 5. Collapsing the data so that each household (SSUID) is represented only once per year.
#    Within each household-year, the main respondent is selected based on:
#       - Most non-missing information (info_count),
#       - If tied, preferring non-citizen status (i.e. citizenship not "Native"),
#       - If still tied, choosing the respondent with the lower PNUM.
# 6. Removing redundant variables.
#
# After processing, only SIPP_wrangled, SIPP_combined, folder_path, and process_year remain.

library(tidyverse)

monthly_SIPP_wrangled <- m_SIPP_combined %>%
  mutate(
    citizenship = case_when(
      EBORNUS == 1 ~ "Native",
      ECITIZEN == 1 & ENATCIT %in% c(2, 3, 4, 5) ~ "Native",
      EBORNUS == 2 & ECITIZEN == 1 ~ "Legalized",
      EBORNUS == 2 & ECITIZEN == 2 & TIMSTAT == 1 ~ "Legalized",
      EBORNUS == 2 & ECITIZEN == 2 & TIMSTAT == 2 ~ "Undocumented",
      EBORNUS == 2 & ECITIZEN == 2 & is.na(TIMSTAT) ~ "Undocumented",
      TRUE ~ "Other"
    )) %>%
  # Create helper variables for selecting the main respondent.
  mutate(
    # Use a subset of key columns as a proxy for "information available"
#    info_count = rowSums(!is.na(across(c(TMWKHRS, HHNUMP, household_income, adjusted_net_worth, citizenship, year)))),
    #desc(info_count), 
    # Create a binary indicator: 1 if non-citizen (i.e. citizenship not "Native"), 0 if "Native"
    non_citizen = if_else(citizenship == "Native", 0, 1)
  ) %>%
  # Remove redundant variables from the earlier processing.
  dplyr::select(-c(EBORNUS, ECITIZEN, ENATCIT, TIMSTAT, TPRLOANAMT, THNETWORTH, THVAL_HOME)) %>%
  # Group by household and year; select one record per group.
  group_by(SSUID, year, MONTHCODE) %>%
  arrange(desc(non_citizen), PNUM) %>% 
  slice(1) %>% 
  ungroup()

# save unemployment data
unemployment <- monthly_SIPP_wrangled %>%
  group_by(SSUID, year) %>%
  summarise(months_unemployed = sum(ENJFLAG == 1 & ENJ_LKWRK == 1, na.rm = TRUE), .groups = "drop")
  
monthly_SIPP_wrangled <- monthly_SIPP_wrangled %>%
  dplyr::select(-non_citizen)

# Clear temporary objects, leaving only SIPP_wrangled, SIPP_combined, folder_path, and process_year.
message("Wrangling complete. SIPP_wrangled dataset created.")