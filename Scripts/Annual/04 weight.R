# 04_weight.R
# -----------------------------
# This script processes the SIPP_savings dataset to create SIPP_weighted.
# It calculates weighted savings rates using WPFINWGT and aggregates data at the household level.
# Assumes that the dataset has already been filtered for household reference persons.

library(tidyverse)
library(survey)

SIPP_savings <- SIPP_savings %>%
  mutate(SR = as.numeric(SR), WPFINWGT = as.numeric(WPFINWGT))

mean_weight <- sum(SIPP_savings$WPFINWGT, na.rm = TRUE) / sum(!is.na(SIPP_savings$WPFINWGT))

# Create SIPP_weighted from SIPP_savings
SIPP_weighted <- SIPP_savings %>%
  mutate(weight = (WPFINWGT / mean_weight)) %>%
  mutate(weight = weight / sum(weight, na.rm = TRUE)) %>%
  mutate(weight = weight / (1/ length(!is.na(weight)))) %>%
  mutate(weighted_SR = weighted.mean(SR, weight, na.rm = TRUE)) 
weighted_SR <- SIPP_weighted %>%
  group_by(year, citizenship) %>%
  summarise(mean(weighted_SR, na.rm = TRUE))
