# Get NHS Wales facilities

library(tidyverse)

beds <- read_csv('data/wales_bed_data/nhs_wales_facilities_beds.csv')

# Get all facilities at the same level of nesting
beds$X3[116:158] <- beds$X4[116:158]

beds <- select(beds, local_health_board = X2, facility = X3, beds = `2018-19 (2)`) %>%
  fill(local_health_board) %>%
  filter(beds != '.')

beds <- na.omit(beds)

beds$post_code <- NA
beds$lat <- NA
beds$long <- NA

write_csv(beds, 'data/wales_bed_data/nhs_wales_facilities_for_geocoding.csv')
