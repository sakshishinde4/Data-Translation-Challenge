source("Code/00_helpers.R")

# Land characteristics (household level)

library(tidyverse)

# data_list is assumed to be loaded from 01_load_data.R

# Combine all Section 8A files

sec8a_files <- grep("sec8a", names(data_list), ignore.case = TRUE)
sec8a_all <- bind_rows(data_list[sec8a_files])

# Detect irrigation variable safely
irrigation_var <- intersect(c("s8aq23", "s8aq24"), names(sec8a_all))[1]

# Create household-level land data

land_hh <- sec8a_all %>%
  group_by(nh) %>%
  summarise(
    land_area_total = sum(s8aq4, na.rm = TRUE),
    land_unit       = max(s8aq3, na.rm = TRUE),
    land_type       = max(s8aq2, na.rm = TRUE),
    soil_fertility  = safe_max(s8aq8),
    soil_slope      = if ("s8aq12" %in% names(sec8a_all)) safe_max(s8aq12) else NA,
    soil_erosion    = if ("s8aq15" %in% names(sec8a_all)) safe_max(s8aq15) else NA,
    irrigation_raw  = if (!is.null(irrigation_var)) safe_max(.data[[irrigation_var]]) else NA,
  )

# Keep only hectares (unit code = 2)

land_hh <- land_hh %>%
  mutate(
    land_area_ha = ifelse(land_unit == 2, land_area_total, NA),
    irrigated    = ifelse(!is.na(irrigation_raw) & irrigation_raw > 0, 1, 0)
  ) %>%
  select(
    nh,
    land_area_ha,
    land_type,
    soil_fertility,
    soil_slope,
    soil_erosion,
    irrigated
  )

# Replace Inf with NA
land_hh <- land_hh %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .)))

# Quick checks
head(land_hh)
summary(land_hh)
