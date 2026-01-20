source("Code/00_helpers.R")

# PROFIT AGGREGATES + FINAL MASTER DATASET
library(haven)
library(dplyr)

# Load Aggregates (Profit)

agg2 <- read_dta("dataset/aggregates/agg2.dta")

agg2_hh <- agg2 %>%
  group_by(nh) %>%
  summarise(
    agri2c   = safe_max(agri2c),   # gross agricultural profit
    hhagdepn = safe_max(hhagdepn), # agricultural expenses
    .groups = "drop"
  ) %>%
  mutate(
    profit_gross = agri2c,
    profit_net   = agri2c - hhagdepn
  )

# Local Area Characteristics

sec0_all <- bind_rows(data_list[grep("sec0", names(data_list), ignore.case = TRUE)])

local_area_hh <- sec0_all %>%
  group_by(nh) %>%
  summarise(
    region   = safe_max(region),
    district = safe_max(district),
    ez       = safe_max(ez),    # ecological zone
    .groups = "drop"
  )

# Final Master Merge

master_df <- agg2_hh %>%
  left_join(education_hh,     by = "nh") %>%
  left_join(agri_inputs_hh,   by = "nh") %>%
  left_join(land_hh,          by = "nh") %>%
  left_join(crop_hh,          by = "nh") %>%
  left_join(local_area_hh,    by = "nh")


# Derived Variables

master_df <- master_df %>%
  mutate(
    profit_per_area_net = ifelse(
      !is.na(land_area_ha) & land_area_ha > 0,
      profit_net / land_area_ha,
      NA
    )
  )

# Final safety cleanup
master_df <- master_df %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .)))


# Sanity Checks

head(master_df)
summary(master_df$profit_net)
summary(master_df$profit_per_area_net)
nrow(master_df)

# Final variable structure check

str(master_df %>%
      select(
        profit_net,
        profit_per_area_net,
        edu_level_mean,
        literacy_rate,
        numeracy_rate,
        voc_rate,
        soil_fertility,
        irrigated
      ))
