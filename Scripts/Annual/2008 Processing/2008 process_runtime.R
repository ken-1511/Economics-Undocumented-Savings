library(tidyverse)

source("2008 Processing/core.R")

file_list <- list.files(path = "{folder_path}/2008 panel waves/", pattern = "\\.dat$", full.names = TRUE)
# Iterate through files and create a tibble for each one
for (file in file_list) {
  file_name <- tools::file_path_sans_ext(basename(file)) # Extract file name without extension
  assign(file_name, core(file), envir = .GlobalEnv) # Store tibble as an object
}