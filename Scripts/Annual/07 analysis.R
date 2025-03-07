# Load necessary packages
library(survey)
library(dplyr)

data <- savings_clean  # Ensure you're using the correct dataset

# Remove rows with NAs in the model variables
data <- data %>%
  mutate(
    undocumented = ifelse(citizenship == "Undocumented", 1, 0),
    covid_pandemic = ifelse(year >= 2020, 1, 0),  
    trump_administration = ifelse(year >= 2017 & year <= 2020, 1, 0),  
    savings_rate = as.numeric(SR),  
    unbanked = ifelse(bank == "No", 1, 0),
    household_size = as.numeric(RHNUMPER),   
    household_inc = as.numeric(income),
    months_unemployed = months_unemployed
  ) %>%
  filter(!is.na(savings_rate) & !is.na(undocumented) & !is.na(covid_pandemic) & 
           !is.na(trump_administration) & !is.na(unbanked) & !is.na(household_inc) & 
           !is.na(household_size), !is.na(months_unemployed))  # Drop rows with NA in any model variable

# Define survey design
survey_design <- svrepdesign(
  data = data,
  weights = ~WPFINWGT,  
  repweights = data %>% select(starts_with("REPWGT")),  
  type = "BRR",  
  mse = TRUE  
)

# Run weighted regression model
model_01 <- svyglm(
  savings_rate ~ (undocumented + covid_pandemic + trump_administration + unbanked + months_unemployed + household_inc + household_size)^2,
  design = survey_design,
  family = gaussian()  
)

# Print model results
summary(model_01)