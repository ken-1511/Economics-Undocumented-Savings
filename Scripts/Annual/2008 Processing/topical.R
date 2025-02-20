library(tidyverse)

topical_annual <- function(file_path, file_name) {
  
}

col_specs <- fwf_cols(
  SSUID = c(6, 12),        # Household-case identifier
  PNUM = c(503, 3),        # Person identifier
  TPRLOANAMT,   # Total personal loan amount
  TIMSTAT,      # Immigration entry status
  THVAL_HOME,   # Value of home property
  THNETWORTH,    # Net worth
)