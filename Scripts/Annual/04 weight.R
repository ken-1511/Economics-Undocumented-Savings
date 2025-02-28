library(tidyverse)
library(survey)

# 04 weight.R
# Calculates the weighted means for analysis.
# This script uses the survey package to calculate weighted means of the savings rate (SR)
# by citizenship over time. The weighted means are computed using the Fay method with 
# a design effect of 0.5. The results are stored in variables
# ----------------------------------------------------------
# cleans NAs
savings_clean <- SIPP_savings %>%
  filter(!is.na(WPFINWGT)) %>% 
  mutate(across(all_of(rep_cols), ~ if_else(is.na(.), WPFINWGT, .)))

# Create the survey design object
sipp.svy <- svrepdesign(
  data = savings_clean,
  weights = ~WPFINWGT,
  repweights = "REPWGT[1-9]+",  # Adjust this regex as needed for your dataset
  type = "Fay",
  rho = 0.5,
  mse = TRUE)

# Compute the weighted mean savings rate (SR) by citizenship.
SR_citizenship <- svyby(~SR, ~citizenship + year, sipp.svy, svymean, na.rm = TRUE)

