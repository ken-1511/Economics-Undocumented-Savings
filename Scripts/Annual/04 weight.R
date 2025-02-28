library(tidyverse)
library(survey)
source("Replicate Weights/rw_wmean.R")

# Assume SIPP_savings is your complete dataset.
unique_years <- sort(unique(SIPP_savings$year))
results_list <- list()
rep_col_pattern = "^REPWGT\\d+$"

for (yr in unique_years) {
  message("Processing year: ", yr)
  
  # Subset data for the given year and remove rows with missing SR, WPFINWGT, or citizenship.
  dat_year <- SIPP_savings %>%
    filter(year == yr, !is.na(SR), !is.na(WPFINWGT), !is.na(citizenship))
  # Identify replicate weight columns in SIPP_savings
    rep_cols <- names(dat_year)[grepl("^REPWGT\\d+$", names(SIPP_savings))]
  # Identify columns that are completely NA
    cols_to_remove <- rep_cols[sapply(dat_year[rep_cols], function(x) all(is.na(x)))]
  # Remove those columns from SIPP_savings
    dat_year <- dat_year %>% select(-all_of(cols_to_remove))
    
  # Compute the weighted mean of SR by citizenship for this year's data.
  result_year <- compute_weighted_mean_survey(
    data = dat_year,
    response = "SR",
    weight = "WPFINWGT",
    predictors = "citizenship"
  )
  
  # Tag the result with the year for later identification.
  result_year$year <- yr
  
  results_list[[as.character(yr)]] <- result_year
}

# Combine the results for all years into one tibble.
SR_results <- bind_rows(results_list)

# View the combined results.
print(SR_results)

# Optionally, graph the weighted mean savings rate over time by citizenship.
ggplot(SR_results, aes(x = year, y = SR, color = citizenship, group = citizenship)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 2.5)) +
  scale_x_continuous(breaks = unique(SIPP_savings$year)) +  # Ensure all years are displayed
  labs(title = "Weighted Mean Savings Rate Over Time by Citizenship",
       x = "Year",
       y = "Weighted Mean Savings Rate") +
  theme_minimal()
