library(tidyverse)

core_annual <- function(file_path) {
# Define column specifications based on the data dictionary
  col_specs <- fwf_cols(
    SSUID = c(6, 12),        # Household-case identifier
    PNUM = c(503, 3),        # Person identifier
    TMWKHRS = c(843, 2),      # Average time worked
    WPFINWGT = c(271, 10),     # Person weight
    MONTHCODE = c(),    # Month identifier
    TFTOTINC = c(310, 8),     # Household income
    #TPRLOANAMT,   # Total personal loan amount
    RHNUMPER = c(59, 3),     # Number of people in the household
    EORIGIN = c(522, 2),      # Hispanic origin
    EBORNUS = c(525, 2),      # Born in the US
    ECITIZEN = c(528, 2),     # Citizenship status
    ENATCIT = c(531, 2),      # How citizenship was acquired
    #TIMSTAT,      # Immigration entry status
    #THVAL_HOME,   # Value of home property
    #THNETWORTH,    # Net worth
    ERACE = c(520,1) # Race, 
  )
  
  # Read the fixed-width file
  sipp_data <- read_fwf(file_path, col_specs, col_types = cols(.default = col_character()))
  return(as_tibble(sipp_data))
}
# View the extracted data

