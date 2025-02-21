library(tidyverse)

topical_09 <- function(file_path) {
  # Define column specifications with explicit start & end positions
  col_specs <- fwf_positions(
    start = c(6, 45, 806, 696, 676, 22),
    end   = c(17, 48, 815, 705, 685, 23),
    col_names = c("SSUID", "PNUM", "TPRLOANAMT", "THVAL_HOME", "THNETWORTH", "WAVE")
  )
  
  # Read the fixed-width file
  sipp_data <- read_fwf(file_path, col_specs, col_types = cols(.default = col_double()))
  return(as_tibble(sipp_data))
}

topical_10 <- function(file_path) {
  # Define column specifications with explicit start & end positions
  col_specs <- fwf_positions(
    start = c(6, 45, 798, 688, 668, 22),
    end   = c(17, 48, 807, 697, 677, 23),
    col_names = c("SSUID", "PNUM", "TPRLOANAMT", "THVAL_HOME", "THNETWORTH", "WAVE")
  )
  
  # Read the fixed-width file
  sipp_data <- read_fwf(file_path, col_specs, col_types = cols(.default = col_double()))
  return(as_tibble(sipp_data))
}

topical_11 <- function(file_path) {
  # Define column specifications with explicit start & end positions
  col_specs <- fwf_positions(
    start = c(6, 45, 798, 688, 668, 22),
    end   = c(17, 48, 807, 697, 677, 23),
    col_names = c("SSUID", "PNUM", "TPRLOANAMT", "THVAL_HOME", "THNETWORTH", "WAVE")
  )
  
  # Read the fixed-width file
  sipp_data <- read_fwf(file_path, col_specs, col_types = cols(.default = col_double()))
  return(as_tibble(sipp_data))
}
