# Load necessary packages

library(survey)
library(dplyr)
library(segmented)

rm(list = setdiff(ls(),c("unemployment", "folder_path", "monthly_sipp_wrangled", "savings_clean", "model_01", "sipp.svy")))

data <- savings_clean  # Ensure you're using the correct dataset

gdp_growth_lookup <- c(
  "2010" = 2.69519,
  "2011" = 1.56441,
  "2012" = 2.28911,
  "2013" = 2.11783,
  "2014" = 2.52382,
  "2015" = 2.94555,
  "2016" = 1.81945,
  "2017" = 2.45762,
  "2018" = 2.96650,
  "2019" = 2.58383,
  "2020" = -2.16303,
  "2021" = 6.05506,
  "2022" = 2.51238,
  "2023" = 2.88755
)

# Create a 5-bin income variable
data <- data %>%
  mutate(real_gdp_growth = gdp_growth_lookup[as.character(year)])

# Remove rows with NAs in the model variables
data <- data %>%
  mutate(
    undocumented = ifelse(citizenship == "Undocumented", 1, 0),
    covid_pandemic = ifelse(year >= 2020, 1, 0),  
    trump = ifelse(year >= 2017 & year <= 2020, 1, 0),  
    savings_rate = as.numeric(SR),  
    unbanked = ifelse(bank == "No", 1, 0),
    household_size = as.numeric(RHNUMPER),   
    household_inc = as.numeric(income),
    months_unemployed = months_unemployed,
    year = year,
    savings_log = log((savings_rate + 101)),
    race = as_factor(TRACE),
    hispanic = as_factor(EORIGIN),
    age = TAGE_EHC
  ) %>%
  filter(!is.na(savings_rate) & !is.na(undocumented) & !is.na(covid_pandemic) & 
           !is.na(trump_administration) & !is.na(unbanked) & !is.na(household_inc) & 
           !is.na(household_size), !is.na(months_unemployed))  # Drop rows with NA in any model variable


# Fit a weighted linear model with the desired main effects and interactions.
lm_model <- lm(savings_rate ~ 
                 household_inc +
                 undocumented +
                 unbanked +
                 months_unemployed +
                 household_inc:undocumented +
                 real_gdp_growth +
                 age,
               data = data, 
               weights = WPFINWGT)

# Apply segmented regression on household_inc,
# using its median as a starting value for the breakpoint.
seg_model <- segmented(lm_model, seg.Z = ~ household_inc, 
                       psi = median(data$household_inc, na.rm = TRUE))

# Display the summary of the segmented model.
summary(seg_model)

lm_model <- lm(savings_rate ~ 
                 trump +
                 undocumented:trump +
                 household_inc +
                 undocumented +
                 unbanked +
                 months_unemployed +
                 household_inc:undocumented +
                 real_gdp_growth +
                 age,
               data = data, 
               weights = WPFINWGT)

# Apply segmented regression on household_inc,
# using its median as a starting value for the breakpoint.
seg_model <- segmented(lm_model, seg.Z = ~ household_inc, 
                       psi = median(data$household_inc, na.rm = TRUE))
summary(seg_model)
