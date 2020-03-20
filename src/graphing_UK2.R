rm(list = ls())
## Script for Covid-19 geographical risk analysis, final data
## Author: Mark Verhagen (LCDS)
## Created: 20-03-2020
## Code Review:

# Load packages
packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "sp", "gpclib", "maptools", "spdep",
              "maptools", "RColorBrewer", "lattice", "gridExtra", "sf", "reshape2", "spData", "rgeos")
lapply(packages, require, character.only = TRUE)

## --- Functions --- ##
create_map_stats <- function(df) {
  return(df %>%
           mutate(pc_capacity = general_cap / pop * 1000,
                  pc_capacity_acute = acute_cap / pop* 1000,
                  pc_hosp = hospitalizations / pop * 1000,
                  pc_hosp_acute = hospitalizations_acute / pop * 1000,
                  abs_excess_demand_hosp = (hospitalizations - general_cap) * 1000 / pop,
                  abs_excess_demand_hosp_acute = (hospitalizations_acute - acute_cap) * 1000 / pop,
                  ## 10% infection rate divided by the ratio of hosp and cap to get tipping point
                  tipping_point_capacity = 10 / (pc_hosp / pc_capacity),  
                  tipping_point_capacity_acute = 10 / (pc_hosp_acute / pc_capacity_acute)))
}

## --- Reading Data --- ##

# read crosswalks
cw_lda_region <- read.csv("data/geo_crosswalks/CW_LDA_Region.csv")  # this crosswalk is LDA to Region (we have both LSOA and LDA in the raw data)
cw_lsoa_ccounty <- read.csv("data/geo_crosswalks/CW_LSOA_CCounty.csv") %>%
  mutate(NAME = gsub("\\&", "and", NAME))  # this crosswalk is generated using a multipolygon matching algorithm to fit LSOA to county

# read data
region_df <- readRDS("data/for graphs/region_data.rds")
ccounty_df <- readRDS("data/for graphs/ccounty_data.rds")
lsoa_df <- readRDS("data/for graphs/lsoa_data.rds")
wales_df <- readRDS("data/for graphs/wales_outline_beds.rds")

# read shapefiles
region_shape <- sf::st_read("shapefiles/UK/Regions/Regions_December_2017_Generalised_Clipped_Boundaries_in_England.shp")
ccounty_shape <- sf::st_read("shapefiles/UK/CCounties/Boundary-line-ceremonial-counties_region.shp") %>%
  mutate(NAME = gsub(" Cer", "", NAME),
         NAME = gsub("\\&", "and", NAME),
         NAME = toupper(NAME))
lsoa_shape <- sf::st_read("shapefiles/UK/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC.shp") %>%
  merge.data.frame(cw_lsoa_ccounty[, c("LSOA11CD", "NAME")], by="LSOA11CD")  # Not necessary yet

## --- Creating sf's --- ##

# Regional SF
agg_region_shape <- sp::merge(region_df, region_shape, by.x="geo_code", by.y="rgn17cd") %>%
  create_map_stats()
head(agg_region_shape)

# County SF
agg_ccounty_shape <- sp::merge(ccounty_df, ccounty_shape, by.x="CCTY19NM", by.y="NAME", all.x=T) %>%
  create_map_stats()

# LSOA SF
agg_lsoa_shape <- sp::merge(lsoa_df, lsoa_shape, by.x="AreaCodes", by.y="LSOA11CD", all.x=T) %>%
  mutate(pc_hosp = hospitalization / value * 1000,
         pc_hosp_acute = hospitalization_acute / value * 1000,
         pc_fatailties = fatalities / value * 1000)

## --- Plotting --- ##
caption <- "Source: Leverhume Center for Demographic Science (using data from ONS, NHS and StatsWales)"  # to be used everywhere
plot_title1 <- "Regional Hospital Bed Capacity (per 1,000) for General Hospitalization and Critical Care. England & Wales"
plot_title2 <- "County Hospital Bed Capacity (per 1,000) for General Hospitalization and Critical Care. England & Wales"
plot_title3 <- "County Expected Hospitalization (per 1,000) for General Hospitalization and Critical Care. England & Wales"
plot_title4 <- "Excess Need for Hospital Beds (per 1,000) in case of a 10% Nationwide Infection. England & Wales"
plot_title5 <- "Excess Need for Hospital Beds (per 1,000) and Capacity in case of a 10% Nationwide Infection. Wales"
plot_title6 <- "County Tipping Point of Infection for General Hospitalization and Critical Care. England & Wales"
plot_title7 <- "London Local Differences in Hospitalization Need"

# -- Plot 1
# data: agg_region_shape
# Left panel: pc_capacity
# Right panel: pc_capacity_acute
# plot / indicate individual cities: Liverpool (53.4084, 2.9916), Birmingham (52.4862, 1.8904),
# Manchester (53.4808, 2.2426), Newcastle (54.9783, 1.6178), London (51.5074, 0.1278)

# -- Plot 2
# data: agg_ccounty_shape
# Left panel: pc_capacity
# Right panel: pc_capacity_acute
# plot / indicate above cities

# -- Plot 3
# data: agg_ccounty_shape
# Left panel: pc_hosp
# Right panel: pc_hosp_acute
# plot / indicate above cities

# -- Plot 4
# data: agg_ccounty_shape
# Left panel: abs_excess_demand_hosp
# Right panel: abs_excess_demand_hosp_acute
# plot / indicate above cities

# -- Plot 5
# data: agg_lsoa_shape subsetted by Wales counties
# agg_lsoa_shape[grepl("Powys|Gwent|Glamorgan|Dyfed|Gwynedd|Clwyd", agg_lsoa_shape$NAME), ]
# Left panel: abs_excess_demand_hosp
# Right panel: abs_excess_demand_hosp_acute
# read.csv("data/wales_bed_data/nhs_wales_facilities_geocoded_cleaned.csv")
# plot / indicate hospitals

# -- Plot 6
# data: agg_ccounty_shape
# Left panel: tipping_point_capacity
# Right panel: tipping_point_capacity_acute
# plot / indicate cities

# -- Plot 7
# data: agg_lsoa_shape
# Main figure: abs_excess_demand_hosp
# zoombox for two areas: Harrow 001C, Newham 013G (areacodes E01033583 and E01002225)
# readRDS("data/for graphs/london_highlight.rds")

# -- Plot 8
# data: agg_ccount_shape
# new variable: pop divided by area? 