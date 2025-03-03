library(tidyverse)

# Define the folder path for the replicate weight CSV files
rep_weights_folder <- file.path(folder_path, "Replicate Weights")

# List all .csv files in the folder (full file paths)
csv_files <- list.files(path = rep_weights_folder, pattern = "\\.csv$", full.names = TRUE)

# Loop over each CSV file
for(file in csv_files) {
  # Read the CSV file assuming pipe delimiter, filter for December (SREFMON == 12),
  # and select only SSUID, PNUM, and replicate weight columns
  tibble_data <- readr::read_delim(file, delim = "|", , col_types = cols(.default = col_double())) %>%
    rename_with(toupper) %>%
    rename_with(~ stringr::str_replace(., "^REPWT(\\d+)$", "REPWGT\\1")) %>%
    filter(MONTHCODE == 12) %>%
    select(SSUID, PNUM, matches("^REPWGT\\d+$"))

  
  # Extract the file's base name (without extension)
  file_base <- tools::file_path_sans_ext(basename(file))
  
  # Assign the tibble to a variable in the global environment using the file base name
  assign(file_base, tibble_data, envir = .GlobalEnv)
}