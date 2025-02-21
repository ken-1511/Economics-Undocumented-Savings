library(tidyverse)

topical_annual <- function(file_path) {
  
  # Extract the file name (without extension)
  file_name <- tools::file_path_sans_ext(basename(file_path))
  # Source helper functions
  source("2008 Processing/topical_helpers.R")
  
  # Determine the helper function to use based on the file name
  if (str_ends(file_name, "4")) {
    return(topical_09(file_path))
    
  } else if (str_ends(file_name, "7")) {
    return(topical_10(file_path))
    
  } else if (str_ends(file_name, "10")) {
    return(topical_11(file_path))
    
  } else {
    stop("File is not a recognized topical wave (4, 7, or 10).
         \nThe scope of this project only included financial data from these topical waves.
         \nTo process other topical waves, update topical.R inside the 2008 Processing folder.")
  }
}