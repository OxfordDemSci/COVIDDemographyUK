# Clean and merge the NHS Wales bed data

library(tidyverse)
library(sf)
library(cowplot)

shape <- read_sf(dsn = "data/wales_bed_data/LocalHealthBoards")

# Read in the beds data and remove Velindre Trust which provides specialty
# cancer care. Rename to match the shapefile and add demographic info from
# http://www.publichealthwalesobservatory.wales.nhs.uk/information-about-the-interactive-map
beds <- read_csv('data/wales_bed_data/nhs_wales_health_board_beds.csv') %>%
  select(name = X2, beds = `2018-19 (2)`) %>%
  filter(name != 'Velindre NHS Trust', !is.na(name))

beds_intensive_care <- read_csv('data/wales_bed_data/nhs_wales_health_board_intensive_care_beds.csv') %>%
  select(name = X2, intensive_care_beds = `2018-19 (2)`) %>%
  filter(name != 'Velindre NHS Trust', !is.na(name))

beds_intensive_care$intensive_care_beds[2] <- '0'
beds_intensive_care$intensive_care_beds <- as.numeric(beds_intensive_care$intensive_care_beds)

beds <- left_join(beds, beds_intensive_care)

# Doing this in a quick and dirty way
beds$name <- c(
  'Betsi Cadwaladr University',
  'Powys Teaching',
  'Hywel Dda',
  'Aneurin Bevan University',
  'Cardiff and Vale University',
  'Abertawe Bro Morgannwg University',
  'Cwm Taf University'
)

health_board_info = tibble(
  name = c(
    'Betsi Cadwaladr University',
    'Powys Teaching',
    'Hywel Dda',
    'Aneurin Bevan University',
    'Cardiff and Vale University',
    'Abertawe Bro Morgannwg University',
    'Cwm Taf University'
  ),
  pop_2014 = c(
    694000,
    132700,
    384000,
    580400,
    482000,
    523000,
    296000
  ),
  prop_75_plus = c(
    0.098,
    0.114,
    0.103,
    0.084,
    0.073,
    0.088,
    0.079
  ),
  prop_smoke = c(
    0.217,
    0.195,
    0.180,
    0.206,
    0.184,
    0.187,
    0.228
  )
)

beds <- left_join(beds, health_board_info) %>%
  mutate(beds_per_1000 = beds / (pop_2014 / 1000), intensive_care_beds_per_1000 = intensive_care_beds / (pop_2014 / 1000))

beds <- left_join(shape, beds, by = 'name')

write_rds(beds, 'data/wales_bed_data/nhs_wales_beds_cleaned.rds')

p_beds <- ggplot(beds, aes(fill = beds_per_1000)) + geom_sf() + labs(fill = 'Beds per 1,000')
p_intensive <- ggplot(beds, aes(fill = intensive_care_beds_per_1000)) + geom_sf() + labs(fill = 'Intensive care beds\nper 1,000')
p_75 <- ggplot(beds, aes(fill = prop_75_plus)) + geom_sf() + labs(fill = 'Proportion 75+')
p_smoke <- ggplot(beds, aes(fill = prop_smoke)) + geom_sf() + labs(fill = 'Proportion of adults\nwho smoke')

p_all <- plot_grid(p_beds, p_intensive, p_75, p_smoke, align = "hv", nrow = 2)

ggsave('figs/maps/wales.pdf', p_all, scale = 1.5)
