library(tidyverse)

core_annual <- function(file_path) {
# Define column specifications based on the data dictionary
  col_specs <- fwf_positions(
    start = c(6, 503, 843, 271, 26, 310, 59, 522, 525, 528, 531, 520, 28, 22),
    end   = c(17, 506, 844, 280, 27, 317, 61, 523, 526, 529, 532, 520, 31, 23),
    col_names = c("SSUID", "PNUM", "TMWKHRS", "WPFINWGT", "MONTHCODE",
                  "TFTOTINC", "RHNUMPER", "EORIGIN", "EBORNUS", "ECITIZEN",
                  "ENATCIT", "ERACE", "RHCALYR", "WAVE")
  )
  
  # Debugging: Check what `fwf_positions()` generates
  print(col_specs)
  
  # Read the fixed-width file
  sipp_data <- read_fwf(file_path, col_specs, col_types = cols(.default = col_double()))
  return(as_tibble(sipp_data) %>% filter(MONTHCODE == "12")) # only Decembers for annual data
}
