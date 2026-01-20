# Household-level education variables
library(tidyverse)

# data_list is already loaded in 01_load_data.R
# It contains all raw GLSS4 .dta files read into a named list
# Identify education datasets
sec2_files <- data_list[grep("^sec2", names(data_list), ignore.case = TRUE)]
sec2_files <- sec2_files[order(names(sec2_files))]
sec2a <- sec2_files[[1]]  # basic education
sec2b <- sec2_files[[2]]  # vocational / tertiary
sec2c <- sec2_files[[3]]  # literacy / numeracy / apprenticeship

# Basic education (Sec2A)
edu_person <- sec2a %>%
  group_by(nh, pid) %>%
  summarise(
    edu_level = max(s2aq2, na.rm = TRUE),
    .groups = "drop"
  )

edu_basic_hh <- edu_person %>%
  group_by(nh) %>%
  summarise(
    edu_level_max  = max(edu_level, na.rm = TRUE),
    edu_level_mean = mean(edu_level, na.rm = TRUE),
    .groups = "drop"
  )

# Literacy / numeracy / apprentice (Sec2C)
skills_person <- sec2c %>%
  group_by(nh, pid) %>%
  summarise(
    can_read_write = as.integer(any(
      s2cq1 == 1 | s2cq2 == 1 | s2cq3 == 1 | s2cq4 == 1,
      na.rm = TRUE
    )),
    can_do_math = as.integer(any(s2cq5 == 1, na.rm = TRUE)),
    apprentice  = as.integer(any(s2cq7 == 1, na.rm = TRUE)),
    .groups = "drop"
  )

skills_hh <- skills_person %>%
  group_by(nh) %>%
  summarise(
    literacy_rate       = mean(can_read_write, na.rm = TRUE),
    numeracy_rate       = mean(can_do_math, na.rm = TRUE),
    apprenticeship_rate = mean(apprentice, na.rm = TRUE),
    .groups = "drop"
  )

# Vocational / tertiary (Sec2B)
voc_person <- sec2b %>%
  group_by(nh, pid) %>%
  summarise(
    voc_school = as.integer(any(s2bq1 == 1, na.rm = TRUE)),
    tertiary   = as.integer(any(s2bq5 == 1, na.rm = TRUE)),
    .groups = "drop"
  )

voc_hh <- voc_person %>%
  group_by(nh) %>%
  summarise(
    voc_rate      = mean(voc_school, na.rm = TRUE),
    tertiary_rate = mean(tertiary, na.rm = TRUE),
    .groups = "drop"
  )

# Final education dataset
education_hh <- edu_basic_hh %>%
  left_join(skills_hh, by = "nh") %>%
  left_join(voc_hh, by = "nh")

# Quick checks
head(education_hh)
summary(education_hh)
