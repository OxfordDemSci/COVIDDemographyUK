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

# Load functions
source("src/graphing_functions.R")

# Load Data
# agg_uk <- readRDS("data/final/UK_census_rates.rds")
agg_uk_h <- readRDS("data/final/UK_2018_all_rates.rds")  # Data at 1% infection uniform
agg_uk_h_0025 <- readRDS("data/final/UK_2018_all_rates_0025.rds")  # Data at 0.25% infection uniform

# Capacity data for hospital beds (Region and Ceremonial County level)
hospital_region <- read_region_hospital()
hospital_ccounty <- read_county_hospital()

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
# lsoa_shape <- sf::st_read("shapefiles/UK/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC.shp") %>%
#   merge.data.frame(cw_lsoa_ccounty[, c("LSOA11CD", "NAME")], by="LSOA11CD")  # Not necessary yet

# Read the original census file and clean the Local Authority District names to 2019 versions
uk <- read.csv("data/census UK/UK_2018_all.csv") %>%
  mutate(LSOA = gsub(" \\d{1}.*", "", LSOA)) %>%
  change_LDA()

# Create a region and county aggregated count of fatalities, hospitalizations etc.
region_df <- regional_agg(agg_uk_h, uk, cw_lda_region, hospital_region)
ccounty_df <- county_agg(agg_uk_h, uk, cw_lsoa_ccounty, hospital_ccounty)
# lsoa_df <- agg_uk_h %>%
#   group_by(AreaCodes) %>%
#   summarise_all(list(sum))

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

# Merge to shapefiles and generate per capita measures
agg_region_shape <- sp::merge(region_df, region_shape, by.x="geo_code", by.y="rgn17cd") %>%
  mutate(pc_capacity = general_cap / pop * 1000,
         pc_capacity_acute = acute_cap / pop* 1000,
         pc_hosp = hospitalizations / pop * 1000,
         pc_hosp_acute = hospitalizations_acute / pop * 1000,
         abs_excess_demand_hosp = (hospitalizations - general_cap) * 1000 / pop,
         abs_excess_demand_hosp_acute = (hospitalizations_acute - acute_cap) * 1000 / pop)

print(paste0("All codes in shapefile match data?: ", all(ccounty_df$CCTY19NM %in% ccounty_shape$NAME)))

agg_ccounty_shape <- sp::merge(ccounty_df, ccounty_shape, by.x="CCTY19NM", by.y="NAME", all.x=T) %>%
  mutate(pc_capacity = general_cap / pop * 1000,
         pc_capacity_acute = acute_cap / pop * 1000,
         pc_hosp = hospitalizations / pop * 1000,
         pc_hosp_acute = hospitalizations_acute / pop * 1000,
         abs_excess_demand_hosp = (hospitalizations - general_cap) * 1000 / pop,
         abs_excess_demand_hosp_acute = (hospitalizations_acute - acute_cap) * 1000 / pop)

# moved here from the last section
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





# ik polish maps ----------------------------------------------------------

library(patchwork)
# theming packages
library(hrbrthemes)
library(cowplot)
library(showtext)
font_add_google("Roboto Condensed", "Roboto Condensed")
font_add_google("Roboto Slab", "Roboto Slab")
showtext::showtext_auto()


# define own theme
own_theme <- theme_map(font_size = 14, font_family = font_rc)+
  theme(
    legend.position = c(.1, .75),
    plot.title = element_text(family = "Roboto Slab", face = 2)
  )

own_plot_grid <- function(a, b, ...) {
  plot_grid(
    a, b, 
    labels = "AUTO", label_fontfamily = "Roboto Slab", 
    label_fontface = 2, label_size = 24, 
    label_x = -.02, label_y = .99
  )
}


# simplify polygons !!!
# the shapefiles are unnecessarily detailed
# note -- we keep 1% of all the dots, and it's still fine
library(rmapshaper)

agg_region_s <- agg_region_shape %>% 
  st_as_sf() %>% 
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

# 0025
agg_region_s_0025 <- agg_region_shape_0025 %>% 
  st_as_sf() %>% 
  ms_simplify(keep = .01)

# create borders layer as lines
# this way we don't have the ugly outlined costal lines
agg_region_b_0025 <- agg_region_s_0025 %>% 
  ms_innerlines()

agg_ccounty_s_0025 <- agg_ccounty_shape_0025 %>% 
  st_as_sf() %>% 
  ms_simplify(keep = .01)

agg_ccounty_b_0025 <- agg_ccounty_s_0025 %>% 
  ms_innerlines()

# save and load back -- for furute return
save(agg_region_s, agg_region_b, agg_ccounty_s, agg_ccounty_b, 
     agg_region_s_0025, agg_region_b_0025, agg_ccounty_s_0025, agg_ccounty_b_0025,
     file = "data/geodata_simplified.rda")

load("data/geodata_simplified.rda")


# produce maps ------------------------------------------------------------

## ------ Capacity plots ------ ##
# general -- regions
agg_region_s %>% 
  ggplot() + 
  geom_sf(aes(fill = pc_capacity), color = NA)+
  geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = pc_capacity %>%  round(1)), 
               size = 5,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'Blues', direction = 1,
    breaks = seq(1.6, 2.2, .2)
  ) + 
  own_theme +
  labs(
    title = "Per capita hospital bed capacity (per 1,000), England. Regional level" %>% str_wrap(width = 45),
    caption = 'Source: Mid-18 Pop. Estimates, Office for National Statistics and NHS England' %>% str_wrap(width = 80), 
    fill = "Beds per\n1,000"
  )

region_general_capacity <- last_plot()

# ggsave(filename = "figs-upd/region_general_capacity.pdf", 
#        region_general_capacity,
#        width = 6, height = 7, 
#        device = cairo_pdf)


# acute -- regions
agg_region_s %>% 
  ggplot() + 
  geom_sf(aes(fill = pc_capacity_acute), color = NA)+
  geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = pc_capacity_acute %>%  round(2)), 
               size = 5,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'Reds', direction = 1,
    breaks = seq(.06, .12, .02)
  ) + 
  own_theme +
  labs(
    title = "Per capita acute care bed capacity (per 1,000), England. Regional level" %>% str_wrap(width = 48),
    caption = 'Source: Mid-18 Pop. Estimates, Office for National Statistics and NHS England' %>% str_wrap(width = 80), 
    fill = "Beds per\n1,000"
  )

region_acute_capacity <- last_plot()


# general ccounty

agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_capacity), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = pc_capacity %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'Blues', direction = 1
  ) + 
  own_theme +
  labs(
    title = "Per capita hospital bed capacity (per 1,000), England. Ceremonial County level" %>% str_wrap(width = 45),
    caption = 'Source: Mid-18 Pop. Estimates, Office for National Statistics and NHS England' %>% str_wrap(width = 80), 
    fill = "Beds per\n1,000"
  )

ccounty_general_capacity <- last_plot()

# acute -- ccounty
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_capacity_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = pc_capacity_acute %>% 
                     round(2) %>% str_replace("0.", ".")), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'Reds', direction = 1
  ) + 
  own_theme +
  labs(
    title = "Per capita acute care bed capacity (per 1,000), England. Ceremonial County level" %>% str_wrap(width = 48),
    caption = 'Source: Mid-18 Pop. Estimates, Office for National Statistics and NHS England' %>% str_wrap(width = 80), 
    fill = "Beds per\n1,000"
  )

ccounty_acute_capacity <- last_plot()



## ------ Expected Hospitalisation plots ------ ##

# Expected Hospitalisation regions
agg_region_s %>% 
  ggplot() + 
  geom_sf(aes(fill = pc_hosp), color = NA)+
  geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = pc_hosp %>%  round(1)), 
               size = 5,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'YlGnBu', direction = 1,
    breaks = seq(6.5, 8.5, .5)
  ) + 
  own_theme +
  labs(
    title = "Expected hospitalizations (per 1,000), England. Regional level" %>% str_wrap(width = 45),
    caption = 'Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate' %>% str_wrap(width = 75), 
    fill = "Cases per\n1,000"
  )

region_expected_hosp_demand <- last_plot()


# Expected Hospitalisation acute regions
agg_region_s %>% 
  ggplot() + 
  geom_sf(aes(fill = pc_hosp_acute), color = NA)+
  geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = pc_hosp_acute %>%  round(1)), 
               size = 5,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'RdPu', direction = 1,
    breaks = seq(1.5, 3.5, .5)
  ) + 
  own_theme +
  labs(
    title = "Expected hospitalizations requiring critical care (per 1,000), England. Regional level" %>% str_wrap(width = 45),
    caption = 'Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate' %>% str_wrap(width = 75), 
    fill = "Cases per\n1,000"
  )

region_expected_hosp_acute_demand <- last_plot()


# Expected Hospitalisation ccounties
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = pc_hosp %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'YlGnBu', direction = 1
  ) + 
  own_theme +
  labs(
    title = "Expected hospitalizations (per 1,000), England. County level" %>% str_wrap(width = 45),
    caption = 'Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate' %>% str_wrap(width = 75), 
    fill = "Cases per\n1,000"
  )

ccounty_expected_hosp_demand <- last_plot()


# Expected Hospitalisation acute ccounties
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = pc_hosp_acute %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'RdPu', direction = 1
  ) + 
  own_theme +
  labs(
    title = "Expected hospitalizations requiring critical care (per 1,000), England. County level" %>% str_wrap(width = 45),
    caption = 'Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate' %>% str_wrap(width = 75), 
    fill = "Cases per\n1,000"
  )

ccounty_expected_hosp_acute_demand <- last_plot()


## ------ Excess demand per 1,000 plots based on 10% ------ ##

# Excess demand regions
agg_region_s %>% 
  ggplot() + 
  geom_sf(aes(fill = abs_excess_demand_hosp), color = NA)+
  geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = abs_excess_demand_hosp %>%  round(1)), 
               size = 5,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'PuBuGn', direction = 1
  ) + 
  own_theme +
  labs(
    title = "Excess demand hospital beds (per 1,000), England. Regional level" %>% str_wrap(width = 45),
    caption = 'Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate' %>% str_wrap(width = 75), 
    fill = "Cases per\n1,000"
  )

region_abs_diff_hosp_demand <- last_plot()


# Excess demand acute regions
agg_region_s %>% 
  ggplot() + 
  geom_sf(aes(fill = abs_excess_demand_hosp_acute), color = NA)+
  geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = abs_excess_demand_hosp_acute %>%  round(1)), 
               size = 5,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'PuRd', direction = 1,
    breaks = seq(1.75, 2.75, .25)
  ) + 
  own_theme +
  labs(
    title = "Excess demand hospital beds, critical care (per 1,000), England. Regional level" %>% str_wrap(width = 45),
    caption = 'Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate' %>% str_wrap(width = 75), 
    fill = "Cases per\n1,000"
  )

region_abs_diff_hosp_acute_demand <- last_plot()


# Excess demand ccounties
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = abs_excess_demand_hosp), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = abs_excess_demand_hosp %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'PuBuGn', direction = 1
  ) + 
  own_theme +
  labs(
    title = "Excess demand hospital beds (per 1,000), England. County level" %>% str_wrap(width = 45),
    caption = 'Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate' %>% str_wrap(width = 75), 
    fill = "Cases per\n1,000"
  )

ccounty_abs_diff_hosp_demand <- last_plot()


# Excess demand acute ccounties
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = abs_excess_demand_hosp_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = abs_excess_demand_hosp_acute %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'RdPu', direction = 1
  ) + 
  own_theme +
  labs(
    title = "Excess demand hospital beds, critical care (per 1,000), England. County level" %>% str_wrap(width = 45),
    caption = 'Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 10% infection rate' %>% str_wrap(width = 75), 
    fill = "Cases per\n1,000"
  )

ccounty_abs_diff_hosp_acute_demand <- last_plot()

## ------ Per county excesse demand for 0.25% infection ------ ##

# Excess demand ccounties
agg_ccounty_s_0025 %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = abs_excess_demand_hosp), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = abs_excess_demand_hosp %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'PRGn', direction = -1,
    breaks = seq(-2, 2, 1)
  ) + 
  own_theme +
  labs(
    title = "Excess demand hospital beds (per 1,000), England. County level" %>% str_wrap(width = 45),
    caption = 'Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 25% infection rate' %>% str_wrap(width = 75), 
    fill = "Cases per\n1,000"
  )

ccounty_abs_diff_hosp_demand_0025 <- last_plot()


# Excess demand acute ccounties
agg_ccounty_s_0025 %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = abs_excess_demand_hosp_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .5, color = "#fafafa")+
  geom_sf_text(aes(label = abs_excess_demand_hosp_acute %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    palette = 'RdPu', direction = 1,
    breaks = seq(.4, .8, .1)
  ) + 
  own_theme +
  labs(
    title = "Excess demand hospital beds, critical care (per 1,000), England. County level" %>% str_wrap(width = 45),
    caption = 'Source: Leverhume Center for Demographic Science (ONS and NHS data). Assuming 25% infection rate' %>% str_wrap(width = 75), 
    fill = "Cases per\n1,000"
  )

ccounty_abs_diff_hosp_acute_demand_0025 <- last_plot()


# export figures as per the draft -----------------------------------------


fig_one <-  own_plot_grid(region_general_capacity, 
                          region_acute_capacity)

ggsave(filename = "figs-upd/one.pdf", 
       fig_one,
       width = 12, height = 7, 
       device = cairo_pdf)


fig_two <- own_plot_grid(ccounty_general_capacity, 
                         ccounty_expected_hosp_demand)

ggsave(filename = "figs-upd/two.pdf", 
       fig_two,
       width = 12, height = 7, 
       device = cairo_pdf)

fig_three <- own_plot_grid(ccounty_abs_diff_hosp_demand, 
                           ccounty_abs_diff_hosp_acute_demand)

ggsave(filename = "figs-upd/three.pdf", 
       fig_three,
       width = 12, height = 7, 
       device = cairo_pdf)



fig_s_one <-  own_plot_grid(ccounty_general_capacity, ccounty_acute_capacity)

ggsave(filename = "figs-upd/s_one.pdf", 
       fig_s_one,
       width = 12, height = 7, 
       device = cairo_pdf)


fig_s_two <-  own_plot_grid(ccounty_expected_hosp_demand, 
                            ccounty_expected_hosp_acute_demand)

ggsave(filename = "figs-upd/s_two.pdf", 
       fig_s_two,
       width = 12, height = 7, 
       device = cairo_pdf)

fig_s_three <-  own_plot_grid(ccounty_abs_diff_hosp_demand_0025, 
                              ccounty_abs_diff_hosp_acute_demand_0025)

ggsave(filename = "figs-upd/s_three.pdf", 
       fig_s_three,
       width = 12, height = 7, 
       device = cairo_pdf)
