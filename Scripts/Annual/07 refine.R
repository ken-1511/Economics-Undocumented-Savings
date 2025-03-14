# refine.R
# ---------------------
library(DHARMa)
library(dplyr)
library(survey)

# Calculate the median income, ignoring missing values
median_income <- median(data$household_inc, na.rm = TRUE)

# Create a new factor variable 'income_bin'
# Calculate quantiles for household_inc
quantiles <- quantile(data$household_inc, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
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
  mutate(
    real_gdp_growth = gdp_growth_lookup[as.character(year)],
    income_bin = case_when(
      household_inc < quantiles[1] ~ "Very Low",
      household_inc < quantiles[2] ~ "Low",
      household_inc < quantiles[3] ~ "Middle",
      household_inc < quantiles[4] ~ "High",
      TRUE ~ "Very High"
    ),
    income_bin = factor(income_bin, levels = c("Very Low", "Low", "Middle", "High", "Very High"))
  )

# Optionally, view the distribution of income_bin
table(data$income_bin)

survey_design <- svrepdesign(
  data = data,
  weights = ~WPFINWGT,  
  repweights = "REPWGT[1-9]+", 
  type = "Fay",
  rho = 0.5,
  mse = TRUE  )

model_02 <- svyglm(
  savings_rate ~ income_bin * (undocumented + unbanked + months_unemployed + real_gdp_growth),
  design = survey_design,
  family = gaussian()
)

summary(model_02)

# Fit the null (intercept-only) model using the same survey design as model_02
null_model <- update(model_02, . ~ 1)

# Compute the deviance-based pseudo R²
pseudo_R2_deviance <- 1 - (deviance(model_02) / deviance(null_model))
print(pseudo_R2_deviance)

model_03 <- svyglm(
  savings_rate ~ income_bin * (undocumented + unbanked + months_unemployed + trump_administration + covid_pandemic + real_gdp_growth),
  design = survey_design,
  family = gaussian()
)

summary(model_03)

# Fit the null (intercept-only) model using the same survey design as model_02
null_model <- update(model_03, . ~ 1)

# Compute the deviance-based pseudo R²
pseudo_R2_deviance <- 1 - (deviance(model_03) / deviance(null_model))
print(pseudo_R2_deviance)