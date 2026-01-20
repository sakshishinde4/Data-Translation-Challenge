# Exploratory and explanatory graphs for ACME memo
# master_df is created in 02_clean_merge.R

library(ggplot2)
library(dplyr)


# Plot 1: Distribution of Net Agricultural Profit
# This histogram shows how agricultural profit is distributed
# across households. It helps us understand whether profits
# are evenly spread or skewed across the population.

plot_profit <- master_df %>%
  filter(is.finite(profit_net))

p1 <- ggplot(plot_profit, aes(x = profit_net)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Distribution of Net Agricultural Profit",
    x = "Net Profit",
    y = "Number of Households"
  )

ggsave(
  "figures/01_profit_net_distribution.png",
  plot = p1,
  width = 7,
  height = 5
)

# Plot 2: Education vs Net Profit
# This scatter plot shows the relationship between average
# household education and net agricultural profit.
# The fitted regression line highlights the overall trend.

plot_education <- master_df %>%
  filter(
    is.finite(edu_level_mean),
    is.finite(profit_net)
  )

p2 <- ggplot(plot_education, aes(x = edu_level_mean, y = profit_net)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(
    title = "Average Household Education vs Net Profit",
    x = "Average Education Level",
    y = "Net Profit"
  )

ggsave(
  "figures/02_education_vs_profit.png",
  plot = p2,
  width = 7,
  height = 5
)

# Plot 3: Irrigation and Profit per Area
# This boxplot compares profit per unit of land between
# irrigated and non-irrigated households.
# It helps assess whether irrigation is associated with
# higher land productivity.

plot_irrigation <- master_df %>%
  filter(
    is.finite(profit_per_area_net),
    !is.na(irrigated)
  )

p3 <- ggplot(plot_irrigation, aes(x = factor(irrigated), y = profit_per_area_net)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(
    title = "Profit per Area by Irrigation Status",
    x = "Irrigated (0 = No, 1 = Yes)",
    y = "Profit per Area"
  )

ggsave(
  "figures/03_irrigation_profit_per_area.png",
  plot = p3,
  width = 7,
  height = 5
)

