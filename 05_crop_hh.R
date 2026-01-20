# CROP PRODUCTION, SALES, AND LOSSES (HOUSEHOLD)

library(dplyr)

# 5.1 Crop Output (Section 8C)

sec8c <- data_list[["sec8c1.dta"]]

crop_output_hh <- sec8c %>%
  group_by(nh) %>%
  summarise(
    crop_output_qty = sum(s8cq3a, na.rm = TRUE),
    .groups = "drop"
  )

# 5.2 Crop Sales Quantity (8D)

sec8d <- data_list[["sec8d.dta"]]

crop_sales_qty_hh <- sec8d %>%
  group_by(nh) %>%
  summarise(
    sales_qty = sum(s8dq2a, na.rm = TRUE),
    .groups = "drop"
  )

# 5.3 Crop Sales Value (8E)

sec8e <- data_list[["sec8e.dta"]]

crop_sales_value_hh <- sec8e %>%
  group_by(nh) %>%
  summarise(
    sales_value = sum(s8eq2, na.rm = TRUE),
    home_consumption_value = sum(s8eq5, na.rm = TRUE),
    .groups = "drop"
  )

# 5.4 Crop Losses (8G)
sec8g <- data_list[["sec8g.dta"]]

crop_losses_hh <- sec8g %>%
  group_by(nh) %>%
  summarise(
    crop_loss_weather = sum(s8gq4, na.rm = TRUE),
    crop_loss_pests   = sum(s8gq5, na.rm = TRUE),
    .groups = "drop"
  )

# 5.5 Combine All Crop Data

crop_hh <- crop_output_hh %>%
  left_join(crop_sales_qty_hh,   by = "nh") %>%
  left_join(crop_sales_value_hh, by = "nh") %>%
  left_join(crop_losses_hh,      by = "nh")

# Final safety cleanup
crop_hh <- crop_hh %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .)))

# Quick checks
head(crop_hh)
summary(crop_hh)
