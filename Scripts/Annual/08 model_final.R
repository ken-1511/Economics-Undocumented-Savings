library(tibble)
library(dplyr)
library(knitr)
library(kableExtra)
library(Hmisc) 

# --- For the Baseline Model ---
# Convert the native coefficient matrix into a tibble with row names as a column
baseline_df <- as.data.frame(summary(seg_model)$coefficients)
baseline_df <- tibble::rownames_to_column(baseline_df, var = "term")

# Add significance codes based on the p-value column "Pr(>|t|)"
baseline_df <- baseline_df %>% 
  mutate(Significance = case_when(
    `Pr(>|t|)` < 0.001 ~ "***",
    `Pr(>|t|)` < 0.01  ~ "**",
    `Pr(>|t|)` < 0.05  ~ "*",
    `Pr(>|t|)` < 0.1   ~ ".",
    TRUE               ~ ""
  ))

# --- For the Trump Model ---
trump_df <- as.data.frame(summary(seg_model_trump)$coefficients)
trump_df <- tibble::rownames_to_column(trump_df, var = "term")

trump_df <- trump_df %>% 
  mutate(Significance = case_when(
    `Pr(>|t|)` < 0.001 ~ "***",
    `Pr(>|t|)` < 0.01  ~ "**",
    `Pr(>|t|)` < 0.05  ~ "*",
    `Pr(>|t|)` < 0.1   ~ ".",
    TRUE               ~ ""
  ))

# --- Create Styled Tables with Alternating Row Shading and Enhanced Header Styling ---

baseline_table <- kable(baseline_df, format = "html", escape = FALSE, caption = "Table B: Baseline Model Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 16) %>%
  row_spec(0, bold = TRUE, font_size = 18, color = "white", background = "#0073C2")

trump_table <- kable(trump_df, format = "html", escape = FALSE, caption = "Table C: Trump Model Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 16) %>%
  row_spec(0, bold = TRUE, font_size = 18, color = "white", background = "#0073C2")

# Display the tables (in an HTML-capable viewer such as RStudio's Viewer or within an RMarkdown document)
baseline_table
trump_table

# Define variables
binary_vars <- c("trump", "undocumented", "unbanked")
cont_vars <- c("savings_rate", "household_inc", "months_unemployed", "real_gdp_growth", "age")


# Continuous summary with weights
cont_summary <- data %>%
  dplyr::select(all_of(cont_vars), weight = WPFINWGT) %>%
  pivot_longer(cols = -weight, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    mean = wtd.mean(value, weights = weight, na.rm = TRUE),
    sd = sqrt(wtd.var(value, weights = weight, na.rm = TRUE)),
    min = min(value, na.rm = TRUE),
    q1 = wtd.quantile(value, weights = weight, probs = 0.25, na.rm = TRUE),
    median = wtd.quantile(value, weights = weight, probs = 0.5, na.rm = TRUE),
    q3 = wtd.quantile(value, weights = weight, probs = 0.75, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

# Binary summary with weights
binary_summary <- data %>%
  dplyr::select(all_of(binary_vars), weight = WPFINWGT) %>%
  pivot_longer(cols = -weight, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    raw_count_1 = sum(value == 1 & !is.na(value), na.rm = TRUE),
    raw_count_0 = sum(value == 0 & !is.na(value), na.rm = TRUE),
    weighted_prop_1 = sum(weight * (value == 1), na.rm = TRUE) / sum(weight[!is.na(value)]),
    weighted_prop_0 = sum(weight * (value == 0), na.rm = TRUE) / sum(weight[!is.na(value)]),
    total_observations = raw_count_1 + raw_count_0,
    .groups = "drop"
  )

# Print both tables
cont_summary %>%
  kable(digits = 3, caption = "        Table A:\n                   Summary of Continuous Variables", format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE)

binary_summary %>%
  kable(digits = 3, caption = "        Table A:\n                   Summary of Binary Variables", format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE)