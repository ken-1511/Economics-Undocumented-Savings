# plotting.R
# -----------
ggplot(SR_citizenship, aes(x = year, y = SR, color = citizenship, group = citizenship)) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(-5, 15), breaks = seq(-5, 12.5, 2.5)) +
  scale_x_continuous(breaks = unique(SIPP_savings$year)) +
  labs(title = "Weighted Mean Savings Rate Over Time by Citizenship",
       x = "Year \n(data runs from December year prior to December listed year)\nExample: 2014 = Dec 2013 -> Dec 2014",
       y = "Weighted Mean Savings Rate (%)") +
  theme_minimal()

