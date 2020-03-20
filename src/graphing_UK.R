rm(list = ls())
## Script for Covid-19 geographical risk analysis
## Author: Mark Verhagen (LCDS)
## Created: 16-03-2020
## Code Review:

setwd("")

# Load packages
packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "sp", "gpclib", "maptools", "spdep",
              "maptools", "RColorBrewer", "lattice", "gridExtra", "sf", "reshape2", "spData", "rgeos")
lapply(packages, require, character.only = TRUE)

# Load functions
source("src/graphing_functions.R")

# Load Data
agg_uk_h <- readRDS("data/final/UK_2018_all_rates_01.rds")  # Data at 1% infection uniform
agg_uk_h_0025 <- readRDS("data/final/UK_2018_all_rates_0025.rds")  # Data at 0.25% infection uniform

# Crosswalk from LDA to Region
cw_lda_region <- read.csv("data/geo_crosswalks/CW_LDA_Region.csv")  # this crosswalk is LDA to Region (we have both LSOA and LDA in the raw data)
cw_lsoa_ccounty <- read.csv("data/geo_crosswalks/CW_LSOA_CCounty.csv") %>%
  mutate(NAME = gsub("\\&", "and", NAME))  # this crosswalk is generated using a multipolygon matching algorithm to fit LSOA to county

# Shapefiles for Region and Ceremonial County
region_shape <- sf::st_read("shapefiles/UK/Regions/Regions_December_2017_Generalised_Clipped_Boundaries_in_England.shp")
ccounty_shape <- sf::st_read("shapefiles/UK/CCounties/Boundary-line-ceremonial-counties_region.shp") %>%
  mutate(NAME = gsub(" Cer", "", NAME),
         NAME = gsub("\\&", "and", NAME),
         NAME = toupper(NAME))
lsoa_shape <- sf::st_read("shapefiles/UK/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC.shp") %>%
  merge.data.frame(cw_lsoa_ccounty[, c("LSOA11CD", "NAME")], by="LSOA11CD")  # Not necessary yet

# Capacity data for hospital beds (Region and Ceremonial County level)
hospital_region <- read_region_hospital()
hospital_ccounty <- rbind(read_county_hospital(), read_wales_hospital(ccounty_shape)) %>%
  mutate(general_cap = as.numeric(general_cap),
         acute_cap = as.numeric(acute_cap))

# Read the original census file and clean the Local Authority District names to 2019 versions
uk <- read.csv("data/census UK/UK_2018_all.csv") %>%
  mutate(LSOA = gsub(" \\d{1}.*", "", LSOA)) %>%
  change_LDA()

# Create a region and county aggregated count of fatalities, hospitalizations etc.
region_df <- regional_agg(agg_uk_h, uk, cw_lda_region, hospital_region)
ccounty_df <- county_agg(agg_uk_h, uk, cw_lsoa_ccounty, hospital_ccounty)
lsoa_df <- agg_uk_h %>%
  group_by(AreaCodes) %>%
  summarise_all(list(sum))

# Same for 0.25% infection  
region_df_0025 <- regional_agg(agg_uk_h_0025, uk, cw_lda_region, hospital_region)
ccounty_df_0025 <- county_agg(agg_uk_h_0025, uk, cw_lsoa_ccounty, hospital_ccounty)

# Address that there are only 7 NHS regions, whereas there are 9 administrative regions. Decompose capacity by pop
dup1 <- c("E12000004", "E12000005")
dup2 <- c("E12000001", "E12000003")

# Do this for the 1% data
region_df <- region_df %>%
  weight_capacity(dup1) %>%
  weight_capacity(dup2)

# Do this for the 0.25% data
region_df_0025 <- region_df_0025 %>%
  weight_capacity(dup1) %>%
  weight_capacity(dup2)

# # Make a regional capacity file for use elsewhere
# write_csv(region_df %>%
#   merge.data.frame(cw_lda_region[!duplicated(cw_lda_region$RGN19NM), ]) %>%
#   select(-LAD19CD, -LAD19NM), "data/hospital beds/region_capacity.csv")


## ---- Generate a set of data for some London zoom-ins ---- ##
London_codes <- lsoa_shape$LSOA11CD[grepl("London", lsoa_shape$NAME)]
London_df <- agg_uk_h[agg_uk_h$AreaCodes %in% London_codes, ] %>%
  mutate(hosp_rate = hospitalization / value)
London_df <- London_df[order(London_df$hosp_rate, decreasing = T), ]

# plot differences in age structure between two selected LSOA's
London_specific <- readRDS("data/census UK/UK_2018_wide.rds") %>%
  filter(AreaCodes %in% c("E01033583", "E01002225"))
London_specific[, 4:22] <- London_specific[, 4:22] / rowSums(London_specific[, 4:22])
ggplot(London_specific %>% select(-AllAges) %>% melt(id.vars = c("AreaCodes", "LSOA"))) +
  geom_bar(aes(x = variable, y = value, fill=LSOA), stat = "identity", position = "dodge") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = "Age structure of Harrow (001C) and Newham (013G)", x="Proportion of population", y="Age")

# E01033583 
# E01002901


# Merge to shapefiles and generate per capita measures
agg_region_shape <- sp::merge(region_df, region_shape, by.x="geo_code", by.y="rgn17cd") %>%
  mutate(pc_capacity = general_cap / pop * 1000,
         pc_capacity_acute = acute_cap / pop* 1000,
         pc_hosp = hospitalizations / pop * 1000,
         pc_hosp_acute = hospitalizations_acute / pop * 1000,
         abs_excess_demand_hosp = (hospitalizations - general_cap) * 1000 / pop,
         abs_excess_demand_hosp_acute = (hospitalizations_acute - acute_cap) * 1000 / pop,
         tipping_point_capacity = 10 / (pc_hosp / pc_capacity),
         tipping_point_capacity_acute = 10 / (pc_hosp_acute / pc_capacity_acute))

print(paste0("All codes in shapefile match data?: ", all(ccounty_df$CCTY19NM %in% ccounty_shape$NAME)))

agg_ccounty_shape <- sp::merge(ccounty_df, ccounty_shape, by.x="CCTY19NM", by.y="NAME", all.x=T) %>%
  mutate(pc_capacity = general_cap / pop * 1000,
         pc_capacity_acute = acute_cap / pop * 1000,
         pc_hosp = hospitalizations / pop * 1000,
         pc_hosp_acute = hospitalizations_acute / pop * 1000,
         abs_excess_demand_hosp = (hospitalizations - general_cap) * 1000 / pop,
         abs_excess_demand_hosp_acute = (hospitalizations_acute - acute_cap) * 1000 / pop,
         tipping_point_capacity = 10 / (pc_hosp / pc_capacity),
         tipping_point_capacity_acute = 10 / (pc_hosp_acute / pc_capacity_acute))

agg_lsoa_shape <- sp::merge(lsoa_df, lsoa_shape, by.x="AreaCodes", by.y="LSOA11CD", all.x=T) %>%
  mutate(pc_hosp = hospitalization / value * 1000,
         pc_hosp_acute = hospitalization_acute / value * 1000,
         pc_fatailties = fatalities / value * 1000)

## ---- LSOA Specific Graphs ---- ##

tiff("figs/capacity_plots/hospitalization_Powys.tiff", units = 'in', width=10, height = 8, res = 100)
ggplot(data = agg_lsoa_shape[agg_lsoa_shape$NAME == "Powys", ], aes(fill = pc_hosp, geometry = geometry)) + geom_sf(color=NA) +
  theme_linedraw() + ggtitle(paste0("Expected hospitalizations (per 1,000), Powys")) +
  scale_fill_distiller(palette = 'PRGn', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science. Assuming 1% infection.', fill = "Hospitalization per 1,000")
dev.off()


tiff("figs/capacity_plots/hospitalization_Cumbria.tiff", units = 'in', width=10, height = 8, res = 100)
ggplot(data = agg_lsoa_shape[agg_lsoa_shape$NAME == "Cumbria", ], aes(fill = pc_hosp, geometry = geometry)) + geom_sf(color=NA) +
  theme_linedraw() + ggtitle(paste0("Expected hospitalizations (per 1,000), Cumbria")) +
  scale_fill_distiller(palette = 'PRGn', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science. Assuming 1% infection.', fill = "Hospitalization per 1,000")
dev.off()

tiff("figs/capacity_plots/hospitalization_London.tiff", units = 'in', width=10, height = 8, res = 100)
ggplot(data = agg_lsoa_shape[grepl("London", agg_lsoa_shape$NAME), ], aes(fill = pc_hosp, geometry = geometry)) + geom_sf(color=NA) +
  theme_linedraw() + ggtitle(paste0("Expected hospitalizations (per 1,000), London")) +
  scale_fill_distiller(palette = 'PRGn', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science. Assuming 1% infection.', fill = "Hospitalization per 1,000")
dev.off()

tiff("figs/capacity_plots/hospitalization_Cornwall.tiff", units = 'in', width=10, height = 8, res = 100)
ggplot(data = agg_lsoa_shape[agg_lsoa_shape$NAME == "Cornwall", ], aes(fill = pc_hosp, geometry = geometry)) + geom_sf(color=NA) +
  theme_linedraw() + ggtitle(paste0("Expected hospitalizations (per 1,000), Cornwall")) +
  scale_fill_distiller(palette = 'PRGn', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science. Assuming 1% infection.', fill = "Hospitalization per 1,000")
dev.off()

## ------ Capacity plots ------ ##
tiff("figs/capacity_plots/region_general_capacity.tiff")
ggplot(data = agg_region_shape, aes(fill = pc_capacity, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Per capita hospital bed capacity (per 1,000), England. Regional level")) +
  scale_fill_distiller(palette = 'Blues', direction=1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Mid-18 Pop. Estimates, Office for National Statistics and NHS England', fill = "Beds per 1,000")
dev.off()

png("figs/capacity_plots/region_acute_capacity.tiff")
ggplot(data = agg_region_shape, aes(fill = pc_capacity_acute, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Per capita acute care bed capacity (per 1,000), England. Regional level")) +
  scale_fill_distiller(palette = 'Reds', direction=1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Mid-18 Pop. Estimates, Office for National Statistics and NHS England', fill = "Beds per 1,000")
dev.off()

tiff("figs/capacity_plots/ccounty_general_capacity.tiff", units = 'in', width=10, height = 8, res = 100)
ggplot(data = agg_ccounty_shape, aes(fill = pc_capacity, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Per capita hospital bed capacity (per 1,000), England. Ceremonial County level")) +
  scale_fill_distiller(palette = 'Blues', direction=1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Mid-18 Pop. Estimates, Office for National Statistics and NHS England', fill = "Beds per 1,000")
dev.off()

tiff("figs/capacity_plots/ccounty_acute_capacity.tiff", units = 'in', width=10, height = 8, res = 100)
ggplot(data = agg_ccounty_shape, aes(fill = pc_capacity_acute, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Per capita acute care bed capacity (per 1,000), England. Ceremonial County level")) +
  scale_fill_distiller(palette = 'Reds', direction=1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Mid-18 Pop. Estimates, Office for National Statistics and NHS England', fill = "Beds per 1,000")
dev.off()

## ------ Expected Hospitalisation plots ------ ##

png("figs/capacity_plots/expected_hosp_demand_region_01.tiff")
ggplot(data = agg_region_shape, aes(fill = pc_hosp, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Expected hospitalizations (per 1,000), England. Regional level")) +
  scale_fill_distiller(palette = 'PRGn', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate', fill = "Cases per 1,000")
dev.off()

png("figs/capacity_plots/expected_hosp_acute_demand_region_01.tiff")
ggplot(data = agg_region_shape, aes(fill = pc_hosp_acute, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Expected hospitalizations requiring critical care (per 1,000), England. Regional level")) +
  scale_fill_distiller(palette = 'RdBu', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate', fill = "Cases per 1,000")
dev.off()

png("figs/capacity_plots/expected_hosp_demand_county_01.tiff")
ggplot(data = agg_ccounty_shape, aes(fill = pc_hosp, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Expected hospitalizations (per 1,000), England. County level")) +
  scale_fill_distiller(palette = 'PRGn', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate', fill = "Cases per 1,000")
dev.off()

png("figs/capacity_plots/expected_hosp_acute_demand_county_01.tiff")
ggplot(data = agg_ccounty_shape, aes(fill = pc_hosp_acute, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Expected hospitalizations requiring critical care (per 1,000), England. County level")) +
  scale_fill_distiller(palette = 'RdBu', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate', fill = "Cases per 1,000")
dev.off()

## ------ Excess demand per 1,000 plots based on 1% ------ ##

png("figs/capacity_plots/abs_diff_hosp_demand_region_01.tiff")
ggplot(data = agg_region_shape, aes(fill = abs_excess_demand_hosp, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Excess demand hospital beds(per 1,000), England. Regional level")) +
  scale_fill_distiller(palette = 'PRGn', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate', fill = "Excess demand per 1,000")
dev.off()

png("figs/capacity_plots/abs_diff_hosp_acute_demand_region_01.tiff")
ggplot(data = agg_region_shape, aes(fill = abs_excess_demand_hosp_acute, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Excess demand hospital beds, critical care (per 1,000), England. Regional level")) +
  scale_fill_distiller(palette = 'RdBu', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate', fill = "Excess per 1,000")
dev.off()

png("figs/capacity_plots/abs_diff_hosp_demand_county_01.tiff")
ggplot(data = agg_ccounty_shape, aes(fill = abs_excess_demand_hosp, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Excess demand hospital beds (per 1,000), England. County level")) +
  scale_fill_distiller(palette = 'PRGn', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate', fill = "Excess per 1,000")
dev.off()

png("figs/capacity_plots/abs_diff_hosp_acute_demand_county_01.tiff")
ggplot(data = agg_ccounty_shape, aes(fill = abs_excess_demand_hosp_acute, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Excess demand hospital beds, critical care (per 1,000), England. County level")) +
  scale_fill_distiller(palette = 'RdBu', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate', fill = "Excess per 1,000")
dev.off()


## ------ Per county excesse demand for 0.25% infection ------ ##
agg_region_shape_0025 <- sp::merge(region_df_0025, region_shape, by.x="geo_code", by.y="rgn17cd") %>%
  mutate(pc_capacity = general_cap / pop * 1000,
         pc_capacity_acute = acute_cap / pop* 1000,
         pc_hosp = hospitalizations / pop * 1000,
         pc_hosp_acute = hospitalizations_acute / pop * 1000,
         abs_excess_demand_hosp = (hospitalizations - general_cap) * 1000 / pop,
         abs_excess_demand_hosp_acute = (hospitalizations_acute - acute_cap) * 1000 / pop)

print(paste0("All codes in shapefile match data?: ", all(ccounty_df_0025$CCTY19NM %in% ccounty_shape$NAME)))

agg_ccounty_shape_0025 <- sp::merge(ccounty_df_0025, ccounty_shape, by.x="CCTY19NM", by.y="NAME", all.x=T) %>%
  mutate(pc_capacity = general_cap / pop * 1000,
         pc_capacity_acute = acute_cap / pop * 1000,
         pc_hosp = hospitalizations / pop * 1000,
         pc_hosp_acute = hospitalizations_acute / pop * 1000,
         abs_excess_demand_hosp = (hospitalizations - general_cap) * 1000 / pop,
         abs_excess_demand_hosp_acute = (hospitalizations_acute - acute_cap) * 1000 / pop)

png("figs/capacity_plots/abs_diff_hosp_demand_county_0025.tiff")
ggplot(data = agg_ccounty_shape_0025, aes(fill = abs_excess_demand_hosp, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Excess demand hospital beds (per 1,000), England. County level")) +
  scale_fill_distiller(palette = 'PRGn', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate', fill = "Excess per 1,000")
dev.off()

png("figs/capacity_plots/abs_diff_hosp_acute_demand_county_0025.tiff")
ggplot(data = agg_ccounty_shape_0025, aes(fill = abs_excess_demand_hosp_acute, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Excess demand hospital beds, critical care (per 1,000), England. County level")) +
  scale_fill_distiller(palette = 'RdBu', direction=-1) + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(caption='Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate', fill = "Excess per 1,000")
dev.off()