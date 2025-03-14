library(tibble)
library(dplyr)
library(knitr)
library(kableExtra)

# --- Custom Summary for Baseline Model (model_final) ---

coefs <- coef(model_final)
new_se <- sqrt(diag(model_final$cov))
t_values <- coefs / new_se
df_resid <- model_final$df.residual  # residual degrees of freedom
p_values <- 2 * pt(-abs(t_values), df = df_resid)

significance <- case_when(
  p_values < 0.001 ~ "***",
  p_values < 0.01  ~ "**",
  p_values < 0.05  ~ "*",
  p_values < 0.1   ~ ".",
  TRUE             ~ ""
)

custom_summary_baseline <- tibble(
  term = names(coefs),
  Estimate = formatC(coefs, format = "f", digits = 4),
  `Std. Error` = formatC(new_se, format = "f", digits = 4),
  `t value` = formatC(t_values, format = "f", digits = 4),
  `Pr(>|t|)` = formatC(p_values, format = "f", digits = 4),
  Significance = significance
)

# --- Custom Summary for Trump Model (trump_final) ---

coefs_trump <- coef(trump_final)
new_se_trump <- sqrt(diag(trump_final$cov))
t_values_trump <- coefs_trump / new_se_trump
df_resid_trump <- trump_final$df.residual
p_values_trump <- 2 * pt(-abs(t_values_trump), df = df_resid_trump)

significance_trump <- case_when(
  p_values_trump < 0.001 ~ "***",
  p_values_trump < 0.01  ~ "**",
  p_values_trump < 0.05  ~ "*",
  p_values_trump < 0.1   ~ ".",
  TRUE                 ~ ""
)

custom_summary_trump <- tibble(
  term = names(coefs_trump),
  Estimate = formatC(coefs_trump, format = "f", digits = 4),
  `Std. Error` = formatC(new_se_trump, format = "f", digits = 4),
  `t value` = formatC(t_values_trump, format = "f", digits = 4),
  `Pr(>|t|)` = formatC(p_values_trump, format = "f", digits = 4),
  Significance = significance_trump
)

# --- Create Styled Tables with Enhanced Font and Header Styling ---

baseline_table <- kable(custom_summary_baseline, format = "html", escape = FALSE, caption = "Baseline Model Summary") %>%
  kable_styling(full_width = FALSE, font_size = 16) %>%
  row_spec(0, bold = TRUE, font_size = 18, color = "white", background = "#0073C2") %>%
  column_spec(1:ncol(custom_summary_baseline), bold = TRUE, color = "black")

trump_table <- kable(custom_summary_trump, format = "html", escape = FALSE, caption = "Trump Model Summary") %>%
  kable_styling(full_width = FALSE, font_size = 16) %>%
  row_spec(0, bold = TRUE, font_size = 18, color = "white", background = "#0073C2") %>%
  column_spec(1:ncol(custom_summary_trump), bold = TRUE, color = "black")

# Display the tables (in an RMarkdown document or RStudio Viewer these will appear styled)
baseline_table
trump_table


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

lm_model_trump <- lm(savings_rate ~ 
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
seg_model_trump <- segmented(lm_model_trump, seg.Z = ~ household_inc, 
                             psi = median(data$household_inc, na.rm = TRUE))
summary(seg_model_trump)
