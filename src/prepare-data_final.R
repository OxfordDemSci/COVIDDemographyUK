## Script for Covid-19 geographical risk analysis, final data
## Author: Mark Verhagen (LCDS)
## Created: 20-03-2020
## Code Review: Ilya Kashnitsky

# Load packages
packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "sp", "gpclib", "maptools", "spdep", "raster", "rgdal",
              "maptools", "RColorBrewer", "lattice", "gridExtra", "sf", "reshape2", "spData", "rgeos",
              "maps", "rmapshaper", "biscale")
lapply(packages, require, character.only = TRUE)
setwd("/Users/markverhagen/Dropbox/Academic Work/Cooperative work/COVIDDemographyUK/")
library(tidyverse)
library(sf)
library(rmapshaper)
library(maptools)
library(osmdata)

## --- Functions --- ##
source("src/graphing_functions.R")

## --- Reading Data --- ##

# eco variables constructed in ecological_indices.R
load("data/for graphs/eco_vars.rda")

# read crosswalks
cw_lda_region <- read.csv("data/geo_crosswalks/CW_LDA_Region.csv")  # this crosswalk is LDA to Region (we have both LSOA and LDA in the raw data)
cw_lsoa_ccounty <- read.csv("data/geo_crosswalks/CW_LSOA_CCounty.csv") %>%
  mutate(NAME = gsub("\\&", "and", NAME))  # this crosswalk is generated using a multipolygon matching algorithm to fit LSOA to county

# read data
region_df <- readRDS("data/for graphs/region_data.rds")
ccounty_df <- readRDS("data/for graphs/ccounty_data.rds")
lsoa_df <- readRDS("data/for graphs/lsoa_data.rds")
CCG_df <- readRDS("data/for graphs/ccg_data.rds")
lsoa_df_dem <- readRDS("data/for graphs/lsoa_data_dem.rds")
CCG_df_dem <- readRDS("data/for graphs/ccg_data_dem.rds")

# read shapefiles
region_shape <- sf::st_read("shapefiles/UK/Regions/Regions_December_2017_Generalised_Clipped_Boundaries_in_England.shp")
ccounty_shape <- sf::st_read("shapefiles/UK/CCounties/Boundary-line-ceremonial-counties_region.shp") %>%
  mutate(NAME = gsub(" Cer", "", NAME),
         NAME = gsub("\\&", "and", NAME),
         NAME = toupper(NAME))
lsoa_shape <- sf::st_read("shapefiles/UK/LSOA/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC.shp") %>%
  merge.data.frame(cw_lsoa_ccounty[, c("LSOA11CD", "NAME")], by="LSOA11CD") 
ccg_shape <- sf::st_read("shapefiles/UK/CCG/Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BFC.shp")

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
agg_lsoa_shape <- sp::merge(lsoa_df_dem, lsoa_shape, by.x="AreaCodes", by.y="LSOA11CD", all.x=T) %>%
  mutate(pc_hosp = hospitalization / value * 1000,
         pc_hosp_acute = hospitalization_acute / value * 1000,
         pc_fatailties = fatalities / value * 1000)

# simplified version included in repository >> standard algorithms can't handle simplification
# which was therefore done in arqgis
saveRDS(agg_lsoa_shape, "data/final/LSOA_complex_alldata.rds")

# CCG SF
agg_ccg_shape <- sp::merge(CCG_df_dem, ccg_shape, by = "CCG19CD") %>% create_map_stats()

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

agg_ccounty_b_s5 <- agg_ccounty_s %>% 
  filter(
    CCTY19NM %in% toupper(c("Powys","Gwent","Glamorgan","Dyfed","Gwynedd","Clwyd"))
  ) %>% 
  ms_innerlines()

## CCG

agg_ccg_s <- agg_ccg_shape %>% 
  st_as_sf() %>% 
  ms_simplify(keep = .01)
saveRDS(agg_ccg_s, "data/final/CCG_simplified_alldata.rds")
# rename and save as 

agg_ccg_b <- agg_ccg_s %>% 
  ms_innerlines()

agg_lsoa_s <- sf::st_read("data/final/LSOA_complex_alldata_final_simplified.geojson")
saveRDS(agg_lsoa_s, "data/final/LSOA_simplified_alldata.rds")

# subset for specific figures
agg_lsoa_s_s5 <- agg_lsoa_shape %>% 
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

agg_lsoa_man <- agg_lsoa_shape %>% 
  filter(
    NAME %in% c("Greater Manchester")
  ) %>% 
  st_as_sf() %>% 
  ms_simplify(keep = .01)

agg_lsoa_man_b <- agg_lsoa_man %>% 
  ms_innerlines()

agg_lsoa_5_b <- agg_lsoa_s_5 %>% 
  ms_innerlines()

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


## Create ecological datasets ##
# ecological data
legend_depriv <- bi_legend(pal = "DkBlue",
                           dim = 3,
                           xlab = "Higher expected\nhospitalization",
                           ylab = "Higher deprivation",
                           size = 10)

legend_eth <- bi_legend(pal = "DkCyan",
                        dim = 3,
                        xlab = "Higher expected\nhospitalization",
                        ylab = "Higher % ethnic",
                        size = 10)

legend_dens <- bi_legend(pal = "DkViolet",
                         dim = 3,
                         xlab = "Higher expected\nhospitalization",
                         ylab = "Higher population\ndensity",
                         size = 10)

LSOA_eco_vars <- LSOA_eco_vars %>%
  rename(AreaCodes = LSOA)
b_londen <- agg_lsoa_s_5 %>%
  left_join(LSOA_eco_vars)
b_londen_depriv <- biscale::bi_class(b_londen, x=pc_hosp, y=depriv)
b_londen_eth <- biscale::bi_class(b_londen, x=pc_hosp, y=Risk)

CCG_eco_vars <- CCG_eco_vars %>%
  rename(CCG19NM = NAME)
agg_ccg_s$CCG19NM.x <- NULL
agg_ccg_s$CCG19NM <- agg_ccg_s$CCG19NM.y
agg_ccg_s_eco <- agg_ccg_s %>%
  left_join(CCG_eco_vars, by="CCG19NM")

ccg_depriv_df <- biscale::bi_class(agg_ccg_s_eco, x=pc_hosp, y=depriv)
ccg_dens_df <- biscale::bi_class(agg_ccg_s_eco, x=pc_hosp, y=dens)

save(file = "data/for graphs/final_eco.rda",
     legend_dens, legend_depriv, legend_eth,
     LSOA_eco_vars, b_londen, b_londen_depriv, b_londen_eth,
     CCG_eco_vars, agg_ccg_s_eco, ccg_depriv_df, ccg_dens_df)

# save and load back -- for furute return
save(
  agg_region_s,
  agg_region_b,
  agg_ccounty_s,
  agg_ccounty_b,
  agg_ccg_s,
  agg_ccg_b,
  agg_lsoa_s_s5,
  agg_lsoa_man,
  agg_lsoa_man_b,
  agg_ccounty_b_s5,
  wales_h,
  agg_lsoa_s_5,
  cities,
  agg_lsoa_5_b,
  file = "data/for graphs/ready.rda"
)

# save for app
save(
  agg_region_s,
  agg_region_b,
  agg_ccounty_s,
  agg_ccounty_b,
  agg_ccg_s,
  agg_ccg_b,
  wales_h,
  cities,
  file = "app/data/graphing.rda"
)

## --- Plotting --- ##
caption <- "Source: Leverhulme Center for Demographic Science (using data from ONS, NHS and StatsWales)"  # to be used everywhere
plot_title_01 <- "Regional Baseline Hospital Bed Capacity (per 1,000) for General Hospitalization (A) and Critical Care (B). England & Wales"
plot_title_02 <- "County Expected Hospitalization (per 1,000) for General Hospitalization (A) and Critical Care (B). England & Wales"
plot_title_03 <- "County Excess Need for Hospital Beds Relative to Baseline Capacity (per 1,000) in Case of a 10% Nationwide Infection for General Hospitalization (A) and Critical Care (B). England & Wales"
plot_title_04 <- "County Tipping Point of Infection for General Hospitalization (A) and Critical Care (B). England & Wales"
plot_title_05 <- "London Local Differences in Hospitalization Need in Case of a 10% Overall Infection"
plot_title_s01 <- "County Baseline Hospital Bed Capacity (per 1,000) for General Hospitalization (A) and Critical Care (B). England & Wales"
plot_title_s02 <- "CCG Baseline Hospital Bed Capacity (per 1,000) in Case of a 10% Nationwide Infection Rate for General Hospitalization (A) and Critical Care (B). England & Wales"
plot_title_s03 <- "CCG Expected Hospitalization (per 1,000) in Case of a 10% Nationwide Infection Rate for General Hospitalization (A) and Critical Care (B). England"
plot_title_s04 <- "CCG Excess Need for Hospital Beds Relative to Baseline Capacity (per 1,000) in Case of a 10% Nationwide Infection for General Hospitalization (A) and Critical Care (B). England"
plot_title_s05 <- "Local Expected Hospitalization (per 1,000) in Case of a 10% Nationwide Infection for General Hospitalization (A) and Critical Care (B) and Baseline Local Hospital Capacity. Wales"
plot_title_s06 <- "CCG Excess Need for Hospital Beds Relative to Baseline Capacity (per 1,000) in Case of a 10% Nationwide Infection for General Hospitalization (A) and Critical Care (B). England"


save(caption, 
     plot_title_01, plot_title_02, plot_title_03, plot_title_04, plot_title_05, 
     plot_title_s01, plot_title_s02, plot_title_s03, plot_title_s04,
     plot_title_s05, plot_title_s06,
     file = "data/for graphs/labs.rda")