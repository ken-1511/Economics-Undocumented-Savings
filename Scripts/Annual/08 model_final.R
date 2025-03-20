library(tibble)
library(dplyr)
library(knitr)
library(kableExtra)

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

baseline_table <- kable(baseline_df, format = "html", escape = FALSE, caption = "Baseline Model Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 16) %>%
  row_spec(0, bold = TRUE, font_size = 18, color = "white", background = "#0073C2")

trump_table <- kable(trump_df, format = "html", escape = FALSE, caption = "Trump Model Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 16) %>%
  row_spec(0, bold = TRUE, font_size = 18, color = "white", background = "#0073C2")

# Display the tables (in an HTML-capable viewer such as RStudio's Viewer or within an RMarkdown document)
baseline_table
trump_table
