library(foreach)
library(doSNOW)
library(segmented)

# Rename your main data frame to rep_data and ensure all columns are atomic
rep_data <- lapply(data, function(x) {
  if (is.list(x)) unlist(x) else x
})

# Automatically identify replicate weight column names using a regex.
replicate_names <- grep("^REPWGT[0-9]+$", names(rep_data), value = TRUE)
R <- length(replicate_names)

# Fay parameter (rho)
rho <- 0.5

# Set up the doSNOW cluster and register it
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores, type = "SOCK")
registerDoSNOW(cl)

# Set up a progress bar
pb <- txtProgressBar(max = R, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Process each replicate weight in parallel using foreach with progress reporting
replicate_results <- foreach(i = 1:R, .packages = c("segmented"), .options.snow = opts) %dopar% {
  weight_var <- replicate_names[i]
  
  # Ensure the replicate weight is numeric
  rep_weight <- as.numeric(rep_data[[weight_var]])
  # Compute psi value as the median of household_inc
  psi_val <- as.numeric(median(rep_data$household_inc, na.rm = TRUE))
  
  # Baseline model with full set of predictors
  lm_model_rep <- lm(savings_rate ~ 
                       household_inc +
                       undocumented +
                       unbanked +
                       months_unemployed +
                       household_inc:undocumented +
                       real_gdp_growth +
                       age,
                     data = rep_data, 
                     weights = rep_weight)
  seg_model_rep <- segmented(lm_model_rep, seg.Z = ~ household_inc, psi = psi_val)
  coef_baseline <- coef(seg_model_rep)
  
  # Model with trump dummy and its interaction
  lm_model_rep_trump <- lm(savings_rate ~ 
                             trump +
                             undocumented:trump +
                             household_inc +
                             undocumented +
                             unbanked +
                             months_unemployed +
                             household_inc:undocumented +
                             real_gdp_growth +
                             age,
                           data = rep_data, 
                           weights = rep_weight)
  seg_model_rep_trump <- segmented(lm_model_rep_trump, seg.Z = ~ household_inc, psi = psi_val)
  coef_trump <- coef(seg_model_rep_trump)
  
  list(baseline = coef_baseline, trump = coef_trump)
}

# Close the progress bar and stop the cluster
close(pb)
stopCluster(cl)

# Extract coefficients for baseline and trump models from replicate results
coefficients_rep <- lapply(replicate_results, `[[`, "baseline")
coefficients_rep_trump <- lapply(replicate_results, `[[`, "trump")

# Convert lists to matrices (assumes same coefficient names across replicates)
coefficients_mat <- do.call(rbind, coefficients_rep)
coefficients_mat_trump <- do.call(rbind, coefficients_rep_trump)

# Assume you have already run your full sample models in your environment:
full_coef <- coef(seg_model)
full_coef_trump <- coef(seg_model_trump)

# Compute replicate variance estimates using Fay's modified BRR:
rep_var <- apply(coefficients_mat, 2, function(x) {
  sum((x - full_coef[names(x)])^2) / (R * (1 - rho)^2)
})
rep_var_trump <- apply(coefficients_mat_trump, 2, function(x) {
  sum((x - full_coef_trump[names(x)])^2) / (R * (1 - rho)^2)
})

rep_se <- sqrt(rep_var)
rep_se_trump <- sqrt(rep_var_trump)

# Combine results into a list for output
results <- list(
  full_model_summary = summary(seg_model),
  full_model_trump_summary = summary(seg_model_trump),
  replicate_coefficients = coefficients_mat,
  replicate_coefficients_trump = coefficients_mat_trump,
  replicate_standard_errors = rep_se,
  replicate_standard_errors_trump = rep_se_trump
)

# Save the results if needed
saveRDS(results, file = paste0(rstudioapi::getActiveProject(), "/Data/replicate_results.rds"))
