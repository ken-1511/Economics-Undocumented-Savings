# process_year.R

library(data.table)
library(stringr)  # For easy extraction of digits (str_extract_all)

# Check if 'sipp' exists; if not, create an empty data.table
if (!exists("sipp")) {
  sipp <- data.table()
}

# Function to load and process a single SIPP file
process_sipp_file <- function(file_path) {
  # Load data
  temp_data <- fread(
    file_path,
    sep = "|",
    select = c(
      "SSUID",       # Household identifier
      "PNUM",        # Person identifier
      "MONTHCODE",   # Month of data
      "TMWKHRS",     # Hours worked
      "WPFINWGT",    # Person weight
      "TFTOTINC",    # Total household income
      "EBORNUS",     # Born in US
      "ECITIZEN",    # Citizenship status
      "ENATCIT",     # How they became citizen
      "TIMSTAT",     # Immigration status
      "THVAL_HOME",  # Home value
      "THNETWORTH",   # Net worth
      "TPRLOANAMT"   # Mortgage Loan amount
    )
  )
  
  # Extract all digits from the filename
  base_name <- basename(file_path)
  year_digits <- str_extract_all(base_name, "\\d+")[[1]]  
  # If multiple digit groups, pick the last set (commonly the year)
  year_val <- as.numeric(tail(year_digits, 1))
  
  # Assign YEAR column
  temp_data[, YEAR := year_val]
  
  # Create unique person ID
  temp_data[, ID := paste0(SSUID, "-", PNUM)]
  
  return(temp_data)
}

# Function to update the global 'sipp' dataset with a new file
update_sipp_data <- function(file_path) {
  # 1) Load the new file
  new_data <- process_sipp_file(file_path)
  
  # 2) Merge into global sipp table
  if (nrow(sipp) == 0) {
    sipp <<- new_data
  } else {
    sipp <<- rbindlist(list(sipp, new_data), use.names = TRUE, fill = TRUE)
  }
  
  # 3) Order by ID, YEAR, MONTHCODE
  setorder(sipp, ID, YEAR, MONTHCODE)
  
  # 4) Calculate month-to-month change in net worth (lag by person ID)
  sipp[, THNETWORTH := THNETWORTH - THVAL_HOME]
  sipp[, SAVINGS_RATE := fifelse((THNETWORTH - THNETWORTH_LAG) < 0, 
                                 0, 
                                 THNETWORTH - THNETWORTH_LAG) / (TFTOTINC +1)]
  # 5) Define household type
  sipp[, HOUSEHOLD_TYPE := fifelse(
    EBORNUS == 1, "Native",
    fifelse(ECITIZEN == 1, "Naturalized", "Non-Citizen")
  )]
}
