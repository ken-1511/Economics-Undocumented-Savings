# plotting.R
# -----------
library(ggplot2)
library(lubridate)

# Create a date variable from 'year' and 'month' columns (using the first day of each month)
Unemp$date <- as.Date(with(Unemp, paste(year, month, "01", sep = "-")))

# Replace 'your_y_variable' with the appropriate variable for your y-axis.
ggplot(Unemp, aes(x = date, y = your_y_variable, color = citizenship, group = citizenship)) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "",
       x = "Time",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))