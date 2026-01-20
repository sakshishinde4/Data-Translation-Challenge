# DESCRIPTIVE STATISTICS

library(dplyr)

# Count total number of unique households in the final master dataset
n_households <- nrow(master_df)
print(paste("Number of households:", n_households))

# Profit Summary

profit_summary <- master_df %>%
  summarise(
    mean_profit_net       = mean(profit_net, na.rm = TRUE),
    median_profit_net     = median(profit_net, na.rm = TRUE),
    sd_profit_net         = sd(profit_net, na.rm = TRUE),
    mean_profit_per_area  = mean(profit_per_area_net, na.rm = TRUE),
    median_profit_per_area= median(profit_per_area_net, na.rm = TRUE)
  )

print(profit_summary)

# Education Summary

education_summary <- master_df %>%
  summarise(
    mean_edu_level        = mean(edu_level_mean, na.rm = TRUE),
    literacy_rate_mean    = mean(literacy_rate, na.rm = TRUE),
    numeracy_rate_mean    = mean(numeracy_rate, na.rm = TRUE),
    voc_rate_mean         = mean(voc_rate, na.rm = TRUE)
  )

print(education_summary)

# Land & Inputs Summary

land_inputs_summary <- master_df %>%
  summarise(
    mean_land_area_ha     = mean(land_area_ha, na.rm = TRUE),
    mean_soil_fertility   = mean(soil_fertility, na.rm = TRUE),
    irrigated_share       = mean(irrigated, na.rm = TRUE),
    mean_fertilizer_cost  = mean(fertilizer_inorganic, na.rm = TRUE),
    mean_seed_cost        = mean(purchased_seeds, na.rm = TRUE)
  )

print(land_inputs_summary)


# Table for Memo (Key Vars)

key_vars <- master_df %>%
  select(
    profit_net,
    profit_per_area_net,
    edu_level_mean,
    literacy_rate,
    numeracy_rate,
    land_area_ha,
    soil_fertility,
    irrigated,
    fertilizer_inorganic,
    purchased_seeds,
    crop_output_qty
  )

summary(key_vars)

