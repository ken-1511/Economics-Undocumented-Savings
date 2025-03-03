library(tidyverse)
library(survey)

compute_weighted_mean_survey <- function(data, response, weight = "WPFINWGT",
                                         predictors = NULL) {
  rep_col_pattern = "^REPWGT\\d+$"
  rep_cols <- names(data)[grepl(rep_col_pattern, names(data))]
  # Replace missing values in each replicate weight column with the main weight value.
  data_clean <- data %>%
    mutate(across(all_of(rep_cols), ~ replace(., is.na(.), .data[[weight]])))
  
  # Create the survey design object using only the available replicate weight columns.
  design <- svrepdesign(
    data = data_clean,
    weights = as.formula(paste0("~", weight)),
    repweights = rep_cols,  # a character vector of the replicate weight column names
    type = "Fay",
    rho = 0.5,
    mse = TRUE
  )
  
  # Compute the weighted mean.
  # If grouping variables are provided, use svyby to compute group-specific weighted means.
  if (!is.null(predictors)) {
    # Construct a formula for the grouping variable(s), e.g., ~citizenship.
    group_formula <- as.formula(paste0("~", predictors))
    out <- svyby(formula = as.formula(paste0("~", response)),
                 by = group_formula,
                 design = design,
                 FUN = svymean,
                 na.rm = TRUE)
  } else {
    out <- svymean(as.formula(paste0("~", response)), design, na.rm = TRUE)
  }
  
  return(out)
}
