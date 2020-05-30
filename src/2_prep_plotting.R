## Script for Covid-19 geographical risk analysis, final data
## Author: Mark Verhagen (LCDS) & Ilya Kashnitsky
## Created: 20-03-2020

# Load packages
packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "sp", "gpclib", "maptools", "spdep", "raster", "rgdal",
              "maptools", "RColorBrewer", "lattice", "gridExtra", "sf", "reshape2", "spData", "rgeos",
              "maps", "rmapshaper", "biscale")
lapply(packages, require, character.only = TRUE)

## --- Functions --- ##
source("src/graphing_functions.R")

## --- Reading Data --- ##

load("data/eco_vars.rda")  # population level variables
load("data/crosswalks.rda")  # crosswalks
load("data/agg_data.rda")  # data on various aggregation levels
load("data/shapefiles.rda")  # shapefiles


## --- Creating sf's --- ##
# County SF
agg_ccounty_shape <- sp::merge(ccounty_df, ccounty_shape, by.x="CCTY19NM", by.y="NAME", all.x=T) %>%
  create_map_stats()

# Wales at region level  (include dat manually)
wales_df <- agg_ccounty_shape %>% 
  filter(CCTY19NM %in% c("POWYS","GWENT","GLAMORGAN","DYFED","GWYNEDD","CLWYD")) %>% 
  st_as_sf %>% 
  ms_dissolve() %>% 
  transmute(
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
agg_lsoa_shape <- sp::merge(lsoa_df_final, lsoa_shape, by="AreaCodes", all.x=T) %>%
  mutate(pc_hosp = hospitalization / value * 1000,
         pc_hosp_acute = hospitalization_acute / value * 1000,
         pc_fatailties = fatalities / value * 1000)

# CCG SF
agg_ccg_shape <- sp::merge(CCG_df_final, ccg_shape, by = "CCG19CD") %>% create_map_stats()

# -- simplify for improved plotting and save boundaries only for aesthetics -- #
# region
agg_region_s <- agg_region_shape %>% 
  ms_simplify(keep = .01)
agg_region_b <- agg_region_s %>% 
  ms_innerlines()

# ccounty
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
agg_ccg_b <- agg_ccg_s %>% 
  ms_innerlines()

# Wales (pre-loaded because LSOA)
agg_lsoa_s_s5 <- readRDS("data/LSOA_wales.rds")
# London
agg_lsoa_s_5 <- agg_lsoa_shape %>% 
  filter(
    NAME %in% c("Greater London", "City and County of the City of London")
  ) %>% 
  st_as_sf()

# Manchester
agg_lsoa_man <- agg_lsoa_shape %>% 
  filter(
    NAME %in% c("Greater Manchester")
  ) %>% 
  st_as_sf()
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
wales_h <- read.csv("data/hospital beds/nhs_wales_facilities_geocoded_cleaned.csv") %>% 
  drop_na() %>% 
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  ) %>% 
  ms_clip(wales_df %>% ms_dissolve())

## -- Create ecological datasets -- ##
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

save(file = "data/inal_eco.rda",
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
  file = "data/ready.rda"
)

# save for app
# save(
#   agg_region_s,
#   agg_region_b,
#   agg_ccounty_s,
#   agg_ccounty_b,
#   agg_ccg_s,
#   agg_ccg_b,
#   wales_h,
#   cities,
#   file = "app/data/graphing.rda"
# )
