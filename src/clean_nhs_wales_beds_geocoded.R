# Join the geocoded Welsh NHS facilities to the bed data

library(tidyverse)

geocoded <- read_csv('data/wales_bed_data/nhs_wales_facilities_geocoded.csv') %>%
  select(facility, lat, long)

beds <- read_csv('data/wales_bed_data/nhs_wales_facilities_beds.csv')

# Get all facilities at the same level of nesting
beds$X3[116:158] <- beds$X4[116:158]

beds <- select(beds, facility = X3, beds = `2018-19 (2)`) %>%
  filter(beds != '.')

beds <- na.omit(beds)

intensive_care_beds <- read_csv('data/wales_bed_data/nhs_wales_facilities_intensive_care_beds.csv')

intensive_care_beds$X3[116:158] <- intensive_care_beds$X4[116:158]

intensive_care_beds <- select(intensive_care_beds, facility = X3, intensive_care_beds = `2018-19 (2)`) %>%
  filter(intensive_care_beds != '.')

intensive_care_beds <- na.omit(intensive_care_beds)

beds <- left_join(beds, intensive_care_beds)

beds$intensive_care_beds[is.na(beds$intensive_care_beds)] <- 0

beds <- left_join(beds, geocoded)

write_csv(beds, 'data/wales_bed_data/nhs_wales_facilities_geocoded_cleaned.csv')
