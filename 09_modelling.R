# Regression Models for Agricultural Profit

library(dplyr)

dir.create("output", showWarnings = FALSE)
dir.create("output/models", showWarnings = FALSE)

# MODEL 1 — Education & Skills → Profit (Net)
model1 <- master_df %>%
  select(
    profit_net,
    edu_level_mean,
    numeracy_rate,
    voc_rate
  ) %>%
  filter(complete.cases(.)) %>%
  lm(
    profit_net ~ edu_level_mean + numeracy_rate + voc_rate,
    data = .
  )

summary(model1)

saveRDS(
  model1,
  file = "output/models/model1_results.rds"
)


# MODEL 2 — Education + Inputs → Profit per Area
model2 <- master_df %>%
  select(
    profit_per_area_net,
    edu_level_mean,
    literacy_rate,
    fertilizer_inorganic,
    purchased_seeds
  ) %>%
  filter(complete.cases(.)) %>%
  lm(
    profit_per_area_net ~
      edu_level_mean +
      literacy_rate +
      fertilizer_inorganic +
      purchased_seeds,
    data = .
  )

saveRDS(
  model2,
  file = "output/models/model2_results.rds"
)

summary(model2)

# MODEL 3 — Inputs & Land Scale → Profit (Net)
model3 <- master_df %>%
  select(
    profit_net,
    fertilizer_inorganic,
    purchased_seeds,
    land_area_ha
  ) %>%
  filter(complete.cases(.)) %>%
  lm(
    profit_net ~
      fertilizer_inorganic +
      purchased_seeds +
      land_area_ha,
    data = .
  )

summary(model3)

saveRDS(
  model3, 
  "output/models/model3_results.rds")


# MODEL 4 — Land Quality & Skills → Profit per Area
model4 <- master_df %>%
  select(
    profit_per_area_net,
    soil_fertility,
    irrigated,
    numeracy_rate,
    voc_rate
  ) %>%
  filter(complete.cases(.)) %>%
  lm(
    profit_per_area_net ~
      soil_fertility +
      irrigated +
      numeracy_rate +
      voc_rate,
    data = .
  )

summary(model4)

saveRDS(model4, "output/models/model4_results.rds")

# MODEL 5 — Robustness Check
model5 <- master_df %>%
  select(
    profit_net,
    literacy_rate,
    numeracy_rate,
    fertilizer_inorganic
  ) %>%
  filter(complete.cases(.)) %>%
  lm(
    profit_net ~
      literacy_rate +
      numeracy_rate +
      fertilizer_inorganic,
    data = .
  )

summary(model5)
saveRDS(model5, "output/models/model5_results.rds")

# MODEL 6 — Alternative Specification (Log Profit)
model6 <- master_df %>%
  filter(profit_net > 0) %>%
  select(
    profit_net,
    edu_level_mean,
    numeracy_rate,
    voc_rate
  ) %>%
  mutate(log_profit = log(profit_net)) %>%
  filter(complete.cases(.)) %>%
  lm(
    log_profit ~ edu_level_mean + numeracy_rate + voc_rate,
    data = .
  )

summary(model6)

saveRDS(model6, "output/models/model6_results.rds")


# MODEL DIAGNOSTICS
# Diagnostic plots are used to visually assess linearity,
# normality of residuals, and homoskedasticity

library(ggplot2)

# Diagnostics for Model 1

model1_df <- data.frame(
  fitted = fitted(model1),
  residuals = resid(model1)
)

# Residuals vs Fitted
ggplot(model1_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Model 1: Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  )

# Q-Q Plot of Residuals
ggplot(model1_df, aes(sample = residuals)) +
  stat_qq(alpha = 0.6) +
  stat_qq_line(color = "red") +
  theme_minimal() +
  labs(
    title = "Model 1: Normal Q-Q Plot"
  )

# Diagnostics for Model 4 (Key Model)

model4_df <- data.frame(
  fitted = fitted(model4),
  residuals = resid(model4)
)

# Residuals vs Fitted
ggplot(model4_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Model 4: Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  )

# Q-Q Plot
ggplot(model4_df, aes(sample = residuals)) +
  stat_qq(alpha = 0.6) +
  stat_qq_line(color = "red") +
  theme_minimal() +
  labs(
    title = "Model 4: Normal Q-Q Plot"
  )
# -----------------------------------------
# Sample Size Information
# -----------------------------------------

# Total households in final dataset
n_households <- nrow(master_df)

# Number of observations used in each model
n_model1 <- nrow(model.frame(model1))
n_model2 <- nrow(model.frame(model2))
n_model3 <- nrow(model.frame(model3))
n_model4 <- nrow(model.frame(model4))
n_model5 <- nrow(model.frame(model5))
n_model6 <- nrow(model.frame(model6))

# Print results
print(paste("Total households in dataset:", n_households))
print(paste("Model 1 observations:", n_model1))
print(paste("Model 2 observations:", n_model2))
print(paste("Model 3 observations:", n_model3))
print(paste("Model 4 observations:", n_model4))
print(paste("Model 5 observations:", n_model5))
print(paste("Model 6 observations:", n_model6))


