# sipp_processing.R
# ------------------
# Clear the environment except for the folder_path variable.
rm(list = setdiff(ls(), "folder_path"))

# Load required libraries.
library(tidyverse)
library(data.table)

# Source process_year.R so that the process_year() function is available.
source("process_year.R")

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
  processed_data <- process_year(file)
  sipp_list[[file]] <- processed_data
}

# Combine all processed tibbles into one tibble.
SIPP_combined <- bind_rows(sipp_list)

# Optionally, save the combined tibble for later use (e.g., as an RDS file).
saveRDS(SIPP_combined, file = file.path(folder_path, "sipp_combined.rds"))
message("Processing complete. Combined data saved as 'sipp_combined.rds' in folder: ", folder_path)

# Clear any temporary objects, leaving only SIPP_combined, folder_path, and process_year.
rm(list = setdiff(ls(), c("SIPP_combined", "folder_path", "process_year")))
