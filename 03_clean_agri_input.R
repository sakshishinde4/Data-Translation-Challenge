# Agricultural input costs (household level)
library(tidyverse)

# data_list is loaded in 01_read_data.R and contains all raw GLSS4 datasets

# Identify Section 8F file
sec8f_index <- grep("sec8f", names(data_list), ignore.case = TRUE)
sec8f <- data_list[[sec8f_index]]

# Detect household ID column safely
hh_col <- intersect(c("nh", "NH", "hhid", "HHID"), names(sec8f))[1]

# Select & clean variables
agri_inputs <- sec8f %>%
  select(
    nh = all_of(hh_col),
    input_code = crpexpcd,
    input_cost = s8fq2
  ) %>%
  mutate(
    input_code = as.character(input_code),
    input_cost = as.numeric(input_cost)
  )



# Map input codes to names
input_labels <- c(
  "1"  = "fertilizer_inorganic",
  "2"  = "fertilizer_organic",
  "3"  = "insecticides",
  "4"  = "herbicides",
  "6"  = "purchased_seeds",
  "7"  = "irrigation",
  "9"  = "fuel_cost",
  "11" = "hired_labor",
  "12" = "transport",
  "13" = "renting_animals",
  "14" = "renting_equipment",
  "17" = "repairs_maintenance"
)

agri_inputs <- agri_inputs %>%
  mutate(input_type = input_labels[input_code]) %>%
  filter(!is.na(input_type))

# Aggregate to household level

agri_inputs_hh <- agri_inputs %>%
  group_by(nh, input_type) %>%
  summarise(
    total_cost = sum(input_cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = input_type,
    values_from = total_cost,
    values_fill = 0
  )

# Quick checks
head(agri_inputs_hh)
summary(agri_inputs_hh)
