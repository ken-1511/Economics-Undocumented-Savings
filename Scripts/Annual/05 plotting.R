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

ggsave(paste0(rstudioapi::getActiveProject(),"/Plots/SR_citizenship.png"), width = 8, height = 6, dpi = 300)

ggplot(SR_bank, aes(x = year, y = SR, color = bank, fill = bank)) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = TRUE, level = .68, size = 1, alpha = 0.3) +
  scale_x_continuous(breaks = unique(SIPP_savings$year)) +
  labs(title = "Savings Rate by Banking Access Over Time",
       x = "Year \n(data runs from December year prior to December listed year)\nExample: 2014 = Dec 2013 -> Dec 2014",
       y = "Weighted Mean Savings Rate (%)") +
  theme_minimal()

ggsave(paste0(rstudioapi::getActiveProject(),"/Plots/SR_bank.png"), width = 8, height = 6, dpi = 300)

ggplot(bank_citizenship, aes(x = year, y = bankNo, color = citizenship, fill = citizenship)) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = TRUE, level = .90, size = 1, alpha = 0.3) +
  scale_x_continuous(breaks = unique(SIPP_savings$year)) +
  labs(title = "Bank Ownership by Citizenship",
       x = "Year \n(data runs from December year prior to December listed year)\nExample: 2014 = Dec 2013 -> Dec 2014",
       y = "Proportion Without Banking Access") +
  theme_minimal()

ggsave(paste0(rstudioapi::getActiveProject(),"/Plots/bank_citizenship.png"), width = 8, height = 6, dpi = 300)