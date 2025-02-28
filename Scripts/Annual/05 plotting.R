# plotting.R
# -----------
ggplot(SR_citizenship, aes(x = year, y = SR, color = citizenship, group = citizenship)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 2.5)) +
  scale_x_continuous(breaks = unique(SIPP_savings$year)) +
  labs(title = "Weighted Mean Savings Rate Over Time by Citizenship",
       x = "Year \n(data runs from December year prior to December listed year)\nExample: 2014 = Dec 2013 -> Dec 2014",
       y = "Weighted Mean Savings Rate") +
  theme_minimal()

ggsave(paste0(rstudioapi::getActiveProject(),"/Plots/SR_citizenship.png"), width = 8, height = 6, dpi = 300)
