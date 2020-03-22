# rm(list = ls()) # https://www.tidyverse.org/blog/2017/12/workflow-vs-script/
## Script for Covid-19 geographical risk analysis, final data
## Author: Mark Verhagen (LCDS)
## Created: 20-03-2020
## Code Review: Ilya Kashnitsky

# Load packages
# packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "sp", "gpclib", "maptools", "spdep", "raster", "rgdal",
#               "maptools", "RColorBrewer", "lattice", "gridExtra", "sf", "reshape2", "spData", "rgeos",
#               "maps")
# lapply(packages, require, character.only = TRUE)

library(tidyverse)
library(sf)
library(rmapshaper)
library(maptools)

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
CCG_df <- readRDS("data/for graphs/ccg_data.rds")

# read shapefiles
region_shape <- sf::st_read("shapefiles/UK/Regions/Regions_December_2017_Generalised_Clipped_Boundaries_in_England.shp")
ccounty_shape <- sf::st_read("shapefiles/UK/CCounties/Boundary-line-ceremonial-counties_region.shp") %>%
  mutate(NAME = gsub(" Cer", "", NAME),
         NAME = gsub("\\&", "and", NAME),
         NAME = toupper(NAME))
lsoa_shape <- sf::st_read("shapefiles/UK/LSOA_shape/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC.shp") %>%
  merge.data.frame(cw_lsoa_ccounty[, c("LSOA11CD", "NAME")], by="LSOA11CD") 
ccg_shape <- sf::st_read("shapefiles/UK/CCG/Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BFC.shp")

# # here go IK local paths to the heavy shapes
# lsoa_shape <- sf::st_read("~/Downloads/uk-shp/LSOA_shape/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC.shp") %>%
#   merge.data.frame(cw_lsoa_ccounty[, c("LSOA11CD", "NAME")], by="LSOA11CD")
# ccg_shape <- sf::st_read("~/Downloads/uk-shp/Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BFC/Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BFC.shp")

## --- Creating sf's --- ##

# County SF
agg_ccounty_shape <- sp::merge(ccounty_df, ccounty_shape, by.x="CCTY19NM", by.y="NAME", all.x=T) %>%
  create_map_stats()

# Wales at region level
wales_df <- agg_ccounty_shape %>% 
  filter(CCTY19NM %in% c("POWYS","GWENT","GLAMORGAN","DYFED","GWYNEDD","CLWYD")) %>% 
  st_as_sf %>% 
  ms_dissolve() %>% 
  transmute(
    # beds = 10563.7,
    # intensive_care_beds  = 153.2,
    general_cap = 10465,
    acute_cap = 153,
    pop = 3138631,
    fatalities = 7939,
    hospitalizations = 26242,
    hospitalizations_acute = 8758,
    RGN19CD = "Wales",
    geo_code = "Wales",
    geometry
  )

# Regional SF
agg_region_shape <- sp::merge(region_df, region_shape, by.x="geo_code", by.y="rgn17cd") %>%
  dplyr::select(names(wales_df)) %>% 
  st_as_sf() %>% 
  rbind(wales_df %>% st_transform(crs = 27700)) %>% # unify projections
  create_map_stats() 

# LSOA SF
agg_lsoa_shape <- sp::merge(lsoa_df, lsoa_shape, by.x="AreaCodes", by.y="LSOA11CD", all.x=T) %>%
  mutate(pc_hosp = hospitalization / value * 1000,
         pc_hosp_acute = hospitalization_acute / value * 1000,
         pc_fatailties = fatalities / value * 1000)

# CCG SF
agg_ccg_shape <- sp::merge(CCG_df, ccg_shape, by = "CCG19CD") %>%
  create_map_stats()

# simplify polygons !!!
# the shapefiles are unnecessarily detailed
# note -- we keep 1% of all the dots, and it's still fine
library(rmapshaper)

agg_region_s <- agg_region_shape %>% 
  ms_simplify(keep = .01)

# create borders layer as lines
# this way we don't have the ugly outlined costal lines
agg_region_b <- agg_region_s %>% 
  ms_innerlines()

agg_ccounty_s <- agg_ccounty_shape %>% 
  st_as_sf() %>% 
  ms_simplify(keep = .01)

agg_ccounty_b <- agg_ccounty_s %>% 
  ms_innerlines()

agg_ccounty_b_s4 <- agg_ccounty_s %>% 
  filter(
    CCTY19NM %in% toupper(c("Powys","Gwent","Glamorgan","Dyfed","Gwynedd","Clwyd"))
  ) %>% 
  ms_innerlines()

## CCG

agg_ccg_s <- agg_ccg_shape %>% 
  st_as_sf() %>% 
  ms_simplify(keep = .01)

agg_ccg_b <- agg_ccg_s %>% 
  ms_innerlines()

# subset for specific figures
agg_lsoa_s_s4 <- agg_lsoa_shape %>% 
  filter(
    NAME %in% c("Powys","Gwent","Glamorgan","Dyfed","Gwynedd","Clwyd")
  ) %>% 
  st_as_sf() %>% 
  ms_simplify(keep = .01)

agg_lsoa_s_5 <- agg_lsoa_shape %>% 
  filter(
    NAME %in% c("Greater London", "City and County of the City of London")
  ) %>% 
  st_as_sf() %>% 
  ms_simplify(keep = .01)

# cities
cities <- maps::world.cities %>% 
  dplyr::filter(
    country.etc=="UK", 
    name %in% c("Liverpool", "Birmingham", "Manchester", "Cardiff",
                "Newcastle upon Tyne", "London", "Bristol")
  ) %>% 
  transmute(name = name %>% str_remove(" upon Tyne"), long, lat) %>% 
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  )

# Wales hospitals
wales_h <- read.csv("data/wales_bed_data/nhs_wales_facilities_geocoded_cleaned.csv") %>% 
  drop_na() %>% 
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  ) %>% 
  ms_clip(wales_df %>% ms_dissolve())


# save and load back -- for furute return
save(
  agg_region_s,
  agg_region_b,
  agg_ccounty_s,
  agg_ccounty_b,
  agg_ccg_s,
  agg_ccg_b,
  agg_lsoa_s_s4,
  agg_ccounty_b_s4,
  wales_h,
  agg_lsoa_s_5,
  cities,
  file = "data/for graphs/ready.rda"
)

load("data/for graphs/ready.rda")


## --- Plotting --- ##
caption <- "Source: Leverhume Center for Demographic Science (using data from ONS, NHS and StatsWales)"  # to be used everywhere
plot_title_01 <- "Regional Hospital Bed Capacity (per 1,000) for General Hospitalization (A) and Critical Care (B). England & Wales"
plot_title_02 <- "County Expected Hospitalization (per 1,000) for General Hospitalization (A) and Critical Care (B). England & Wales"
plot_title_03 <- "County Excess Need for Hospital Beds (per 1,000) in Case of a 10% Nationwide Infection for General Hospitalization (A) and Critical Care (B). England & Wales"
plot_title_04 <- "County Tipping Point of Infection for General Hospitalization (A) and Critical Care (B). England & Wales"
plot_title_05 <- "London Local Differences in Hospitalization Need in Case of a 10% Overall Infection"
plot_title_s01 <- "County Hospital Bed Capacity (per 1,000) for General Hospitalization (A) and Critical Care (B). England & Wales"
plot_title_s02 <- "CCG Hospital Bed Capacity (per 1,000) for General Hospitalization (A) and Critical Care (B). England"
plot_title_s03 <- "CCG Expected Hospitalization (per 1,000) for General Hospitalization (A) and Critical Care (B). England"
plot_title_s04 <- "CCG Excess Need for Hospital Beds (per 1,000) in Case of a 10% Nationwide Infection for General Hospitalization (A) and Critical Care (B). England"
plot_title_s05 <- "Local Expected Hospitalizatoin (per 1,000) in Case of a 10% Nationwide Infection for General Hospitalization (A) and Critical Care (B) and Local Hospital Capacity. Wales"
plot_title_s06 <- "CCG Excess Need for Hospital Beds (per 1,000) in Case of a 10% Nationwide Infection for General Hospitalization (A) and Critical Care (B). England"


save(caption, plot_title1, plot_title2, plot_title3, plot_title4, plot_title5,
     plot_title6, plot_title7, plot_title8, plot_title9, plot_title10, 
     file = "data/for graphs/labs.rda")

# -- Plot 1
## Can you include the wales_df sf to the agg_region_shape one? I can't get is to work
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
# agg_lsoa_shape[grepl("Powys|Gwent|Glamorgan|Dyfed|Gwynedd|Clwyd", agg_lsoa_shape$NAME), ])
# Left panel: pc_hosp
# Right panel: pc_hosp_acute
# read.csv("data/wales_bed_data/nhs_wales_facilities_geocoded_cleaned.csv")
# plot / indicate hospitals

# -- Plot 6
# data: agg_ccounty_shape
# Left panel: tipping_point_capacity
# Right panel: tipping_point_capacity_acute
# plot / indicate cities

# -- Plot 7
# data: agg_lsoa_shape
# Main figure: pc_hosp
# zoombox for two areas: Harrow 001C, Newham 013G (areacodes E01033583 and E01002225)
# readRDS("data/for graphs/london_highlight.rds")

# -- Plot 8
# data: agg_ccount_shape
# new variable: pop divided by area? 


