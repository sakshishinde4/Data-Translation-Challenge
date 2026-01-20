# Load all GLSS4 raw datasets

library(haven)
library(tidyverse)

# Path to GLSS4 raw data
data_path <- "dataset/glss4_new"

# Find all .dta files recursively
dta_files <- list.files(
  path = data_path,
  pattern = "\\.dta$",
  full.names = TRUE,
  recursive = TRUE
)

# Read all .dta files into a named list
data_list <- lapply(dta_files, read_dta)
names(data_list) <- basename(dta_files)

# Quick check
length(data_list)
names(data_list)
