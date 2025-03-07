library(tidyverse)
library(survey)

# 04 weight.R
# Calculates the weighted means for analysis.
# This script uses the survey package to calculate weighted means of the savings rate (SR)
# by citizenship over time. The weighted means are computed using the Fay method with 
# a design effect of 0.5. The results are stored in variables
# ----------------------------------------------------------
# cleans NAs

rep_col_pattern = "^REPWGT\\d+$"
rep_cols <- names(monthly_SIPP_wrangled)[grepl(rep_col_pattern, names(monthly_SIPP_wrangled))]
monthly_clean <- monthly_SIPP_wrangled %>%
  filter(!is.na(WPFINWGT)) %>% 
  filter(WPFINWGT > 0) %>%
  mutate(across(all_of(rep_cols), ~ if_else(is.na(.), WPFINWGT, .))) %>%
  mutate(REPWGT0 = WPFINWGT) %>%
  mutate(across(matches("^REPWGT\\d+$"), ~ if_else(is.na(.), WPFINWGT, .))) %>%
  mutate(across(matches("^REPWGT\\d+$"), ~ . * RHNUMPER)) %>%
  mutate(bank = if_else(THVAL_BANK > 0, "Yes", "No"))

# Create the survey design object
unemp.svy <- svrepdesign(
  data = monthly_clean,
  weights = ~WPFINWGT,
  repweights = "REPWGT[1-9]+",  # Adjust this regex as needed for your dataset
  type = "Fay",
  rho = 0.5,
  mse = TRUE)

unemp <- svyby(~(ENJFLAG==1 & ENJ_LKWRK==1), ~citizenship + MONTHCODE + year, unemp.svy, svymean, na.rm = TRUE)