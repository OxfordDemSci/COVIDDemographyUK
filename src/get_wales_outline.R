# Get Wales outline and add bed data

library(sf)
library(readr)

df <- read_sf(dsn = 'data/wales_bed_data/wales_outline')

df$beds <- 10563.7
df$intensive_care_beds <- 153.2

write_rds(df, 'wales_outline_beds.rds')
