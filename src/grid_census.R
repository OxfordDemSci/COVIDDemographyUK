rm(list = ls())
## Script to generate UK census at five or ten year intervals, non-sex disaggregated
## Author: Mark Verhagen (LCDS)
## Created: 16-03-2020
## Code Review:

setwd("/Users/markverhagen/Dropbox/Academic Work/Cooperative work/COVIDDemographyUK/")

# Load packages
packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "sp", "gpclib", "maptools", "spdep", "raster", "rgdal",
              "maptools", "RColorBrewer", "lattice", "gridExtra", "sf", "reshape2", "spData", "rgeos",
              "maps", "rmapshaper", "geojson")
lapply(packages, require, character.only = TRUE)

# Load functions
# Load Data
# uk_wide <- readRDS("data/final/UK_2018_wide.rds")
# 
# ccg_shape <- sf::st_read("data/prepared shapefiles/ccg_simple.geojson")
# lsoa_shape <- sf::st_read("data/prepared shapefiles/LSOA_complex_alldata_final_simplified.geojson")
# 
# cw_lsoa_CCG <- read.csv("data/geo_crosswalks/CW_LSOA_CCG_UK.csv")
# 
# hospital_CCG <- readRDS("data/for graphs/ccg_capacity.rds") %>%
#   as.data.frame() %>%
#   dplyr::select(-CCG19NM, -LAT, -LONG) %>%
#   group_by(CCG19CD) %>%
#   summarise_all(list(sum))
# save(uk_wide,
#      lsoa_census,
#      ccg_census,
#      ccg_shape,
#      lsoa_shape,
#      cw_lsoa_CCG,
#      hospital_CCG,
#      file="data/final/grid_set.dta")



rm(list = ls())
load("data/final/grid_set.dta")
source("src/census_functions.r")

for (i_par in c(0.5, 1, 2)) {
  print(i_par)
  uk_agg <- generate_agg(uk_wide, i_multiplier = i_par)
  lsoa_df <- uk_agg %>%
    group_by(AreaCodes) %>%
    summarise_all(list(sum)) %>%
    mutate(pc_hosp = hospitalization / value * 1000,
           pc_hosp_acute = hospitalization_acute / value * 1000,
           pc_fatailties = fatalities / value * 1000) %>%
    left_join(lsoa_census, by = "AreaCodes") %>%
    rename_arcgis_lsoa()
    
  st_write(sp::merge(lsoa_df, lsoa_shape[, c("AreaCodes", "geometry")], by="AreaCodes"),
           paste0("data/final/grid/", "lsoa_df_", i_par, "_None.geojson"),
           delete_dsn=TRUE)
  
  for (c_par in c(1, 1.5, 2)) {
    uk_agg <- generate_agg(uk_wide, i_multiplier = i_par)
    hospital_CCG_rated <- hospital_CCG %>%
      mutate(general_cap = c_par * general_cap,
             acute_cap = c_par * acute_cap)
    
    CCG_df <- uk_agg %>%
      merge.data.frame(cw_lsoa_CCG, by.x="AreaCodes", by.y="LSOA11CD") %>%
      dplyr::select(-LSOA11NM, -CCG19CDH, -CCG19NM, -LAD19NM, -LAD19CD, -FID, -AreaCodes) %>%
      group_by(CCG19CD) %>%
      summarise(pop = sum(value),
                fatalities = sum(fatalities),
                hospitalizations = sum(hospitalization),
                hospitalizations_acute = sum(hospitalization_acute)) %>%
      left_join(hospital_CCG_rated, ny = "CCG19CD") %>%
      mutate(general_cap = ifelse(is.na(general_cap), 0, general_cap),
             acute_cap = ifelse(is.na(acute_cap), 0, acute_cap)) %>%
      left_join(ccg_census, by="CCG19CD") %>%
      create_map_stats() %>%
      rename_arcgis_ccg()
    
    st_write(sp::merge(CCG_df, ccg_shape[, c("CCG19CD", "geometry")], by="CCG19CD"),
             paste0("data/final/grid/", "ccg_df_", i_par, "_", c_par, ".geojson"),
             delete_dsn=TRUE)
  }
}

