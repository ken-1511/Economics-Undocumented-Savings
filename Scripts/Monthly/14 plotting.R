# plotting.R
# -----------
library(ggplot2)
library(lubridate)
library(dplyr)  # Ensure dplyr is loaded for pipe usage

# Create a proper date variable from 'year' and 'month'
unemp <- unemp %>%
  rename(unemployment_rate = `ENJFLAG == 1 & ENJ_LKWRK == 1TRUE`) %>%
  mutate(date = make_date(year = year, month = MONTHCODE, day = 1))

# Filter for only June and December dates for x-axis labels
axis_dates <- unemp %>%
  filter(MONTHCODE %in% c(6, 12)) %>%
  pull(date)  # Extract only the relevant dates for breaks

# Plot using the unemployment rate
ggplot(unemp, aes(x = date, y = unemployment_rate, color = citizenship, group = citizenship)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 0.12), labels = scales::percent_format(accuracy = 1)) +  # Set y-axis from 0% to 12%
  scale_x_date(breaks = axis_dates, date_labels = "%b %Y") +  # Show only June & December
  geom_hline(yintercept = 0, color = "black", size = 1) +  # Ensure this is added in the ggplot() object
  labs(title = "        Figure 3\n                        Unemployment Rate Over Time by Citizenship",
       x = "Time",
       y = "Unemployment Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
ggsave(paste0(rstudioapi::getActiveProject(),"/Plots/unemployment.png"), width = 8, height = 6, dpi = 300)