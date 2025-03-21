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
  labs(title = "Household Savings Rate by Banking Access Over Time",
       subtitle = "bands represent 2 standard errors",
       x = "Year \n(data runs from December year prior to December listed year)\nExample: 2014 = Dec 2013 -> Dec 2014",
       y = "Weighted Mean Savings Rate (%)") +
  theme_minimal()

ggsave(paste0(rstudioapi::getActiveProject(),"/Plots/SR_bank.png"), width = 8, height = 6, dpi = 300)

ggplot(bank_citizenship, aes(x = year, y = bankNo, color = citizenship, fill = citizenship)) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = TRUE, level = .90, size = 1, alpha = 0.3) +
  scale_x_continuous(breaks = unique(SIPP_savings$year)) +
  labs(title = "        Figure 2\n                        Unbanked by Household Citizenship",
       subtitle = "bands represent 90% confidence intervals",
       x = "Year \n(data runs from December year prior to December listed year)\nExample: 2014 = Dec 2013 -> Dec 2014",
       y = "Proportion Without Banking Access") +
  theme_minimal()

ggsave(paste0(rstudioapi::getActiveProject(),"/Plots/bank_citizenship.png"), width = 8, height = 6, dpi = 300)

ggplot(income_citizenship, aes(x = year, y = income, color = citizenship, fill = citizenship)) +
  geom_hline(yintercept = 0, color = "black", size = 1) +  # Black x-axis line
  geom_hline(aes(yintercept = 106400, linetype = "2022 CPS Mean Household"), color = "grey50", size = 0.8) +  # Grey dashed reference line
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = TRUE, level = .90, size = 1, alpha = 0.3) +
  scale_x_continuous(breaks = unique(income_citizenship$year)) +  # Use unique years from dataset
  scale_y_continuous(limits = c(0, 200000), breaks = seq(0, 200000, 25000)) +
  scale_linetype_manual(name = "Reference Line", values = c("2022 CPS Mean Household" = "dashed")) +  # Add to legend
  labs(title = "Nominal Household Income by Citizenship",
       subtitle = "Before taxes, including benefits\nBands represent 90% confidence intervals",
       x = "Year",
       y = "Income ($)") +
  theme_minimal() +
  theme(legend.position = "right")

ggsave(paste0(rstudioapi::getActiveProject(),"/Plots/income_citizenship.png"), width = 8, height = 6, dpi = 300)

ggplot(AMS_citizenship, aes(x = year, y = Monthly_Savings_Amount, color = citizenship, fill = citizenship)) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = TRUE, level = .90, size = 1, alpha = 0.3) +
  scale_x_continuous(breaks = unique(SIPP_savings$year)) +
  scale_y_continuous(limits = c(-5000, 5000), breaks = seq(-5000, 5000, 2000)) +
  labs(title = "Nominal Household Savings Amount by Citizenship \n(before taxes, including benefits)",
       subtitle = "bands represent 90% confidence intervals",
       x = "Year",
       y = "Dollar Amount Saved Per Month") +
  theme_minimal()

ggsave(paste0(rstudioapi::getActiveProject(),"/Plots/AMS_citizenship.png"), width = 8, height = 6, dpi = 300)