# process_year.R
# --------------
# Load required libraries
library(data.table)
library(dplyr)
library(tidyverse)

# Define a function to process a single SIPP file.
# This function reads the file, selects desired columns (including HHNUMP for household size and PNUM for Person identifier),
# extracts a year from the file name (subtracting 1), overwrites PNUM with a unique identifier,
# and filters the data.
process_year <- function(file_path) {
  # Extract a four-digit number (year) from the file name and subtract 1.
  extracted_year <- as.numeric(str_extract(basename(file_path), "[0-9]{4}"))
  year_val <- extracted_year - 1
  
  # Read the file using fread; select the desired columns.
  dt <- fread(file_path, sep = "|", select = c(
    "PNUM",         # Person identifier
    "TMWKHRS",      # Average time worked
    "WPFINWGT",     # Person weight
    "MONTHCODE",    # Month identifier
    "SSUID",        # Household-case identifier
    "TFTOTINC",     # Household income
    "TPRLOANAMT",   # Total personal loan amount
#    "HHNUMP",       # Number of people in the household
    "EBORNUS",      # Born in the US
    "ECITIZEN",     # Citizenship status
    "ENATCIT",      # How citizenship was acquired
    "TIMSTAT",      # Immigration entry status
    "THVAL_HOME",   # Value of home property
    "THNETWORTH"    # Net worth
  ))
  
  # Convert the data.table to a tibble.
  df <- as_tibble(dt)
  
  # Add the year variable (extracted from the filename, minus 1).
  df <- df %>% mutate(year = year_val)
  
  # Keep only observations corresponding to month 12 and filter out records with negative values.
  df <- df %>% 
    filter(MONTHCODE == 12,
           TFTOTINC >= 0,
           TMWKHRS >= 0)
  
  return(df)
}
