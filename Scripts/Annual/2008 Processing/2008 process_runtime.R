library(tidyverse)

source("2008 Processing/core.R")

file_list <- list.files(path = "{folder_path}/2008 panel waves/", pattern = "\\.dat$", full.names = TRUE)

core_files <- file_list[basename(file_list) %>% str_starts("l08puw")]

for (file in core_files) {
  file_name <- tools::file_path_sans_ext(basename(file))
  assign(file_name, core_annual(file), envir = .GlobalEnv) 
}

topical_files <- file_list[basename(file_list) %>% str_starts("p08putm")]

for (file in topical_files) {
  file_name <- tools::file_path_sans_ext(basename(file))
  assign(file_name, topical_annual(file), envir = .GlobalEnv) 
}