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
rep_cols <- names(m_SIPP_wrangled)[grepl(rep_col_pattern, names(m_SIPP_wrangled))]
monthly_clean <- m_SIPP_wrangled %>%
  filter(!is.na(WPFINWGT)) %>% 
  filter(WPFINWGT > 0) %>%
  mutate(across(all_of(rep_cols), ~ if_else(is.na(.), WPFINWGT, .))) %>%
  mutate(REPWGT0 = WPFINWGT) %>%
  mutate(across(matches("^REPWGT\\d+$"), ~ if_else(is.na(.), WPFINWGT, .))) %>%
  mutate(across(matches("^REPWGT\\d+$"), ~ . * RHNUMPER)) %>%
  mutate(bank = if_else(THVAL_BANK > 0, "Yes", "No"))


# Create the survey design object
sipp.svy <- svrepdesign(
  data = monthly_clean,
  weights = ~WPFINWGT,
  repweights = "REPWGT[1-9]+",  # Adjust this regex as needed for your dataset
  type = "Fay",
  rho = 0.5,
  mse = TRUE)

SR_citizenship <- svyby(~SR, ~citizenship + year, sipp.svy, svymean, na.rm = TRUE)