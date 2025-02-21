# rw_dat.R
# 
# reads in .dat files from the replicate weights, and adds the data to existing tibbles.
# assumes that replicate weight data from 2008 panel waves 5, 8, and 11 are available
# and in the location of {folder_path}/Replicate Weights/ as .dat files.
# ----------------------------------------------------------

library(tidyverse)

# Define the folder path for replicate weight files
rep_weights_folder <- file.path(folder_path, "Replicate Weights")
dat_files <- list.files(path = rep_weights_folder, pattern = "\\.dat$", full.names = TRUE)

# Define column widths:
# - SSUID: 12 characters
# - SPANEL: 4 characters
# - SWAVE: 2 characters
# - SREFMON: 1 character
# - PNUM: 4 characters
# - 120 replicate weight columns (REPWGT1 to REPWGT120): each 10 characters wide
widths <- c(12, 4, 2, 1, 4, rep(10, 120))

# Define column names:
col_names <- c("SSUID", "SPANEL", "SWAVE", "SREFMON", "PNUM",
               paste0("REPWGT", 1:120))

# Loop over each .dat file
for(file in dat_files) {
  # Read the file into a tibble
  tibble_data <- readr::read_fwf(file, fwf_widths(widths, col_names), col_types = cols(.default = col_double()))
  
  # Filter to only keep records for one reference month (e.g., SREFMON == 1)
  tibble_data <- tibble_data %>% 
    filter(SREFMON == 1) %>%
    # Keep only SSUID, PNUM, and replicate weight columns (columns starting with REPWGT followed by digits)
    select(SSUID, PNUM, matches("^REPWGT\\d+$"))
  
  # Extract the file's base name and ending digits (before .dat)
  file_base <- basename(file)
  ending_digits <- stringr::str_extract(file_base, "\\d+(?=\\.dat$)")
  
  # Assign the tibble to the appropriate variable based on the ending digits
  if (ending_digits == "5") {
    assign("rw2010", tibble_data, envir = .GlobalEnv)
  } else if (ending_digits == "8") {
    assign("rw2011", tibble_data, envir = .GlobalEnv)
  } else if (ending_digits == "11") {
    assign("rw2012", tibble_data, envir = .GlobalEnv)
  }
  rm(tibble_data)
}


