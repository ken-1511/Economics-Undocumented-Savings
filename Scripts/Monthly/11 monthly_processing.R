# monthly_processing.R
# ------------------
# Clear the environment except for the folder_path variable.
rm(list = setdiff(ls(), "folder_path"))

# Load required libraries.
library(tidyverse)
library(data.table)

# Source process_annual.R so that the process_annual() function is available.
# source("2008 Processing/2008 process_runtime.R") ** data from 2008 has unresolved issues

source("10 process_monthly.R")
source("Replicate Weights/rw_csv.R")

# Ensure that folder_path is defined (it should be provided by master_runtime.R).
if (!exists("folder_path")) {
  stop("folder_path variable not set. Please set folder_path in master_runtime.R.")
}

# List all SIPP CSV files in the specified folder.
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store processed tibbles.
sipp_list <- list()

# Loop over each file and process it using the process_year() function.
for (file in file_list) {
  message("Processing file: ", file)
  processed_data <- process_monthly(file)
  sipp_list[[file]] <- processed_data
}

# Combine all processed tibbles into one tibble.
m_SIPP_combined <- bind_rows(sipp_list)
