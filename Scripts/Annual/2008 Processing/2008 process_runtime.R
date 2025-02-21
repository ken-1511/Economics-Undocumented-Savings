# 2008 process_runtime.R
# -----------------
# This script loads required packages, sources the core and topical processing scripts,
# and then returns a new .csv file for each annual panel wave that contains financial 
# topical data matched with the corresponding core data.

library(tidyverse)

source("2008 Processing/core.R")
source("2008 Processing/topical.R")

file_list <- list.files(path = file.path(folder_path, "2008 panel waves"), 
                        pattern = "\\.dat$", full.names = TRUE)

core_files <- file_list[basename(file_list) %>% str_starts("l08puw")]

for (file in core_files) {
  file_name <- tools::file_path_sans_ext(basename(file))
  assign(file_name, core_annual(file), envir = .GlobalEnv) 
}
# Remove all 0 observation tibbles
rm(list = ls()[sapply(ls(), function(x) is_tibble(get(x)) && nrow(get(x)) == 0)])
# Renames tibbles
invisible(sapply(ls(), function(x) {
  if (is_tibble(get(x)) && "RHCALYR" %in% colnames(get(x))) {
    tibble_data <- get(x)
    new_name <- paste0("core_", unique(tibble_data$RHCALYR)[1]) # Use first unique RHCALYR
    assign(new_name, tibble_data, envir = .GlobalEnv)
    rm(list = x, envir = .GlobalEnv) # Remove old name
  }
}))

topical_files <- file_list[basename(file_list) %>% str_starts("p08putm")]

# Iterate through topical files
for (file in topical_files) {
  file_name <- tools::file_path_sans_ext(basename(file))  # Extract filename without extension
  
  # Process topical file and store it as a tibble
  topical_tibble <- topical_annual(file)
  assign(file_name, topical_tibble, envir = .GlobalEnv) 
  
  # Extract the wave value from the topical tibble and adjust for the core wave
  topical_wave <- unique(topical_tibble$WAVE)[1]  # Take the first unique WAVE value
  core_wave <- topical_wave + 1  # Core wave is always one wave ahead
  
  # Find the matching core tibble by checking for WAVE = topical WAVE + 1
  core_tibble_name <- ls(pattern = "core_")  # Assuming core tibbles are named "core_..."
  core_tibble <- NULL
  
  for (core_name in core_tibble_name) {
    temp_core <- get(core_name)  # Retrieve the core tibble
    
    if ("WAVE" %in% colnames(temp_core) && core_wave %in% temp_core$WAVE) {
      core_tibble <- temp_core  # Store the matched tibble
      break  # Stop searching once a match is found
    }
  }
  
  # Proceed only if a matching core tibble is found
  if (!is.null(core_tibble)) {
    # Perform matching on SSUID and PNUM
    matched_tibble <- topical_tibble %>%
      inner_join(core_tibble, by = c("SSUID", "PNUM"))
    
    # Assign matched tibble to the environment with a new name
    assign(paste0("pu", (topical_wave %/% 3) + 2009), matched_tibble, envir = .GlobalEnv)
  } else {
    warning(paste("No matching core tibble found for WAVE:", core_wave, "in file", file_name))
  }
}
rm(list = setdiff(ls()[!grepl("\\d{4}$", ls())], "folder_path"))

sapply(ls(pattern = "^pu"), function(x) {
  df <- get(x) %>%
    mutate(TIMSTAT = NA) %>%   # Add new column with NA values
    select(-matches("wave\\.x|wave\\.y"))  # Remove wave.x and wave.y columns
  
  # Save as pipe-delimited CSV inside folder_path
  write_delim(df, file.path(folder_path, paste0(x, ".csv")), delim = "|")
})
message("Processing complete. Matched data saved as .csv files in folder: ", folder_path)