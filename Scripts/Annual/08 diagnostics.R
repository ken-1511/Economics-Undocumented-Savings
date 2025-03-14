library(ggplot2)
library(dplyr)
library(tibble)
library(moments)

model_02.resid <- resid(model_02)
plot(model_02.resid)

# 1. Q–Q Plot
qqnorm(data$savings_rate,
       main = "Normal Q–Q Plot of Savings Rate")
qqline(data$savings_rate, col = "red", lty = 2)

library(ggplot2)

ggplot(data, aes(x = savings_rate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "white") +
  geom_density(color = "blue", size = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$savings_rate, na.rm = TRUE),
                            sd = sd(data$savings_rate, na.rm = TRUE)),
                color = "red", linetype = "dashed", size = 1) +
  labs(title = "Savings Rate Density vs. Normal Distribution",
       x = "Savings Rate",
       y = "Density") +
  theme_minimal()

library(dplyr)
library(survey)

# Winsorize the savings_rate at the 5th and 95th percentiles
winsor_limits <- quantile(data$savings_rate, probs = c(0.05, 0.95), na.rm = TRUE)

data <- data %>%
  mutate(
    savings_rate_wins = ifelse(savings_rate < winsor_limits[1],
                               winsor_limits[1],
                               ifelse(savings_rate > winsor_limits[2],
                                      winsor_limits[2],
                                      savings_rate))
  )

# Define your survey design (assuming same settings as before)
survey_design <- svrepdesign(
  data       = data,
  weights    = ~WPFINWGT,
  repweights = "REPWGT[1-9]+",
  type       = "Fay",   
  rho        = 0.5,
  mse        = TRUE
)

# Run your regression model using the winsorized savings_rate
model_wins <- svyglm(
  savings_rate_wins ~ income_bin * (undocumented + covid_pandemic + trump_administration +
                                      unbanked + months_unemployed + real_gdp_growth),
  design = survey_design,
  family = gaussian()
)

summary(model_wins)
# Fit the null (intercept-only) model using the same survey design as model_02
null_model <- update(model_wins, . ~ 1)

# Compute the deviance-based pseudo R²
pseudo_R2_deviance <- 1 - (deviance(model_wins) / deviance(null_model))
print(pseudo_R2_deviance)
