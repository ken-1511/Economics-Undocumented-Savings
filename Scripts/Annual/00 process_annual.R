# process_year.R
# --------------
# Load required libraries
library(data.table)
library(dplyr)
library(tidyverse)

# Define a function to process a single SIPP file.
# This function:
#   1. Extracts a year from the file name (subtracting 1).
#   2. Reads only the desired columns (by first reading the header and converting to uppercase).
#   3. Renames columns to standard uppercase names.
#   4. Filters the data for month 12 records and nonnegative household income and work hours.
process_annual <- function(file_path) {
  # Extract a four-digit number (year) from the file name and subtract 1.
  extracted_year <- as.numeric(str_extract(basename(file_path), "[0-9]{4}"))
  year_val <- extracted_year - 1
  
  # Desired columns (in uppercase) with inline comments:
  desired <- c(
    "PNUM",         # Person identifier
    "TMWKHRS",      # Average time worked
    "WPFINWGT",     # Person weight
    "MONTHCODE",    # Month identifier
    "SSUID",        # Household-case identifier
    "TFTOTINC",     # Household income
    "TPRLOANAMT",   # Total personal loan amount
    "RHNUMPER",     # Number of people in the household
    "EORIGIN",      # Hispanic origin
    "EBORNUS",      # Born in the US
    "ECITIZEN",     # Citizenship status
    "ENATCIT",      # How citizenship was acquired
    "TIMSTAT",      # Immigration entry status
    "THVAL_HOME",   # Value of home property
    "THNETWORTH"    # Net worth
  )
  
  # Read only the header (nrows = 0) and convert names to uppercase.
  header <- names(fread(file_path, sep = "|", nrows = 0))
  header_upper <- toupper(header)
  
  # Find the indices of the desired columns in the header.
  select_idx <- match(desired, header_upper)
  
  # Warn if any desired columns are not found.
  if(any(is.na(select_idx))) {
    warning("The following desired columns were not found in the file: ", 
            paste(desired[is.na(select_idx)], collapse = ", "))
  }
  
  # Read the file using fread with the select parameter to load only the needed columns.
  dt <- fread(file_path, sep = "|", select = select_idx)
  
  # Set the column names to our desired names.
  setnames(dt, desired)
  
  # Convert the data.table to a tibble.
  df <- as_tibble(dt)
  
  # Add the new 'year' column.
  df <- df %>% mutate(year = year_val)
  
  # Filter to keep only records for which:
  #   - MONTHCODE equals 12 (December)
  #   - TFTOTINC (household income) is nonnegative
  #   - TMWKHRS (average time worked) is nonnegative
  df <- df %>% filter(MONTHCODE == 12, TFTOTINC >= 0, TMWKHRS >= 0)
  
  return(df)
}
