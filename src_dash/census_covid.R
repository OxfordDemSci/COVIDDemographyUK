rm(list = ls())
## Script for Covid-19 geographical risk analysis
## Author: Mark Verhagen (LCDS)
## Created: 16-03-2020
## Code Review:

setwd("")

# Load packages
packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "sp", "gpclib", "maptools", "spdep",
              "maptools", "RColorBrewer", "lattice", "gridExtra", "sf", "reshape2")
lapply(packages, require, character.only = TRUE)

# Load Mark's API key
census_api_key("cf1a084fd0e5d3dacafb4296d357400be25c95f2", install = TRUE)

# Load functions
source("src/functions.r")

# Read data from census API
df <- get_county_data()

# Focus on NY
ny_df <- df %>%
  filter(grepl("New York", NAME))

# Or not and just use the full set! Comment out for NY
ny_df <- df

# Cast to wide and reformat to 10 year age intervals, no sex distinction
ny_df_wide <- l2w(ny_df)

# Read mortality rates
rates <- get_rates()

# Melt and concatenate rates
ny_df_rates <- ny_df_wide %>%
  melt(id.vars = c("GEOID", "NAME")) %>%
  filter(variable != "Age_All") %>%
  rename(age = variable) %>%
  left_join(rates)

# Calculate expected fatalities given infection rate
infection_rate <- 0.4
ny_df_rates$fatalities <- round(ny_df_rates$value * infection_rate * ny_df_rates$lethality_rate)
final <- make_final_set(ny_df_rates)

shape_df <- sf::st_read("shapefiles/cb_2018_us_county_500k.shp") %>%
  sp::merge(final, by="GEOID")

# Make the graphs for each state

for (i in unique(shape_df$STATEFP)) {
  temp <- shape_df %>%
    filter(STATEFP %in% i)
  state_name <- unique(gsub(".*, ", "", temp$NAME.x))
  png(paste0("figs/maps/", state_name, "_County.png"))
  print(ggplot(data = temp[, c("relative_hazard", "geometry")], aes(fill = relative_hazard, geometry = geometry)) + geom_sf() +
    theme_linedraw() + ggtitle(paste0("Percentage of population at risk, ", state_name)) +
    scale_fill_distiller(palette = 'PiYG') +
    labs(caption='Source: US Census and ISS, as per March, 13th. Assuming 40% infection rate across age groups', fill = "Percentage at risk"))
  dev.off()
  
}