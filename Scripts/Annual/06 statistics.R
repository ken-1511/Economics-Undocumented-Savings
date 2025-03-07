# summary_statistics.R
# ---------------------
# This script generates two sets of summary statistics for the savings rate (SR)
# by citizenship and year. The first set uses the raw (unweighted) SR values,
# and the second uses SR weighted by the household size (RHNUMPER).
# Both summaries are saved as CSV files in the ./Statistics/ folder.

library(tidyverse)

# Create the output folder if it doesn't exist.
if (!dir.exists(paste0(rstudioapi::getActiveProject(),"/Statistics"))) {
  dir.create(paste0(rstudioapi::getActiveProject(),"/Statistics"), recursive = TRUE)
}

