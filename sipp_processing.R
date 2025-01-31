# SIPP_processing.R

rm(list = ls())
# Load necessary libraries
library(tcltk)        # For file chooser (tk_choose.dir)
library(data.table)   # For data manipulation
library(dplyr)        # Tidyverse library
library(tidyverse)    # Additional Tidyverse functionality

# Source the second script that defines processing functions
source("process_year.R")

# Prompt the user for the SIPP folder
sipp_folder <- tk_choose.dir()
if (is.na(sipp_folder)) {
  stop("No folder was selected. Exiting.")
}

# List all files in the folder
sipp_files <- list.files(sipp_folder, full.names = TRUE)
if (length(sipp_files) == 0) {
  stop("No files found in the selected folder.")
}

# Process each file in the folder
for (file_path in sipp_files) {
  message("Processing file: ", file_path)
  update_sipp_data(file_path)
}

# 1) Filter to DECEMBER data ONLY
sipp <- sipp[MONTHCODE == 12]

# 2) Convert the result to a tidyverse tibble
sipp <- as_tibble(sipp)

# 3) Remove unneeded variables
sipp <- sipp %>% 
  select(-WPFINWGT, -EBORNUS, -ECITIZEN, -ENATCIT, -TIMSTAT)

# 4) Remove rows where TMWKHRS <= 0
sipp <- sipp %>%
  filter(TMWKHRS > 0)

# 5) Adjust net worth by subtracting home value and adding mortgage
sipp <- sipp %>%
  mutate(THNETWORTH = THNETWORTH - THVAL_HOME + TPRLOANAMT)

# 6) Calculate annual savings rate from previous December’s net worth
#    Group by ID, sort by YEAR, then lag
sipp <- sipp %>%
  group_by(ID) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    THNETWORTH_LAG = lag(THNETWORTH),
    ANNUAL_SAVINGS_RATE = (THNETWORTH - THNETWORTH_LAG) / (TFTOTINC * 12)
  ) %>%
  ungroup()

# Save the final dataset
saveRDS(sipp, file = "sipp_annual_december.rds")
message("All files processed, December-only data saved as 'sipp_annual_december.rds'.")
