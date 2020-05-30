## Script to generate UK census at five or ten year intervals, non-sex disaggregated
## Author: Mark Verhagen (LCDS)
## Created: 16-03-2020

# Load packages
packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "lattice", "reshape2", "dplyr")
lapply(packages, require, character.only = TRUE)

# Load functions
source("src/census_functions.r")

# Load Data
uk <- readRDS("data/census UK/UK_2018_all.rds") %>%
  filter(LA..2019.boundaries. == "")  # Drops the county aggregated numbers
names(uk) <- gsub("X|\\.", "", names(uk))  # change column names of the form X\\d{1} to just the digit

# Turn data to wide, aggregated by age bin
uk_wide <- aggregate_years(uk)

test_aggregates(uk_wide)  # unit test
saveRDS(uk_wide, "data/census UK/UK_2018_wide.rds")

# Get rates
# rates <- get_rates2(interpolate = FALSE)  ## for 10-year intervals
rates <- get_rates2()

# Attach rates and aggregate data on the LSOA level
agg_uk_h <- aggregate_data_h(uk_wide, rates)

save(uk_wide, rates, agg_uk_h, file="data/uk_census_data.rda")
