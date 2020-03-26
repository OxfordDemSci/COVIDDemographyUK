rm(list = ls())
## Script for Covid-19 geographical risk analysis
## Author: Mark Verhagen (LCDS)
## Created: 16-03-2020
## Code Review:


# Load packages
packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "sp", "gpclib", "maptools", "spdep",
              "maptools", "RColorBrewer", "lattice", "gridExtra", "sf", "reshape2", "spData", "rgeos")
lapply(packages, require, character.only = TRUE)

# Load functions
source("src/graphing_functions.R")

# Load Data
agg_uk_h <- readRDS("data/final/UK_2018_all_rates_01.rds")  # Data at 10% infection uniform

# Crosswalk from LDA to Region
cw_lda_region <- read.csv("data/geo_crosswalks/CW_LDA_Region.csv") %>%
  mutate(LAD19NM = gsub(", City of.*|, County.*", "", LAD19NM))  # this crosswalk is LDA to Region (we have both LSOA and LDA in the raw data)
cw_lsoa_ccounty <- read.csv("data/geo_crosswalks/CW_LSOA_CCounty.csv") %>%
  mutate(NAME = gsub("\\&", "and", NAME))  # this crosswalk is generated using a multipolygon matching algorithm to fit LSOA to county
cw_lsoa_CCG <- read.csv("data/geo_crosswalks/CW_LSOA_CCG_UK.csv")


# Shapefiles for Region and Ceremonial County
region_shape <- sf::st_read("shapefiles/UK/Regions/Regions_December_2017_Generalised_Clipped_Boundaries_in_England.shp")
ccounty_shape <- sf::st_read("shapefiles/UK/CCounties/Boundary-line-ceremonial-counties_region.shp") %>%
  mutate(NAME = gsub(" Cer", "", NAME),
         NAME = gsub("\\&", "and", NAME),
         NAME = toupper(NAME))
lsoa_shape <- sf::st_read("shapefiles/UK/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC.shp") %>%
  merge.data.frame(cw_lsoa_ccounty[, c("LSOA11CD", "NAME")], by="LSOA11CD")  # Not necessary yet
CCG_shape <- sf::st_read("shapefiles/UK/CCG/Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BFC.shp") %>%
  merge.data.frame(cw_lsoa_CCG[, c("LSOA11CD", "CCG19CD")], by="CCG19CD")  # Not necessary yet


# Capacity data for hospital beds (Region and Ceremonial County level)
hospital_region <- read_region_hospital()
hospital_ccounty <- rbind(read_county_hospital(), read_wales_hospital(ccounty_shape)) %>%
  mutate(general_cap = as.numeric(general_cap),
         acute_cap = as.numeric(acute_cap))
hospital_CCG <- readRDS("data/for graphs/ccg_capacity.rds") %>%
  as.data.frame() %>%
  dplyr::select(-CCG19NM, -LAT, -LONG) %>%
  group_by(CCG19CD) %>%
  summarise_all(list(sum))

# Read the original census file and clean the Local Authority District names to 2019 versions
uk <- read.csv("data/census UK/UK_2018_all.csv") %>%
  mutate(LSOA = gsub(" \\d{1}.*", "", LSOA)) %>%
  change_LDA()

# Create a region and county aggregated count of fatalities, hospitalizations etc.
region_df <- regional_agg(agg_uk_h, uk, cw_lda_region, hospital_region)
ccounty_df <- county_agg(agg_uk_h, uk, cw_lsoa_ccounty, hospital_ccounty) %>%
  mutate(general_cap = ifelse(is.na(general_cap), 0, general_cap),
         acute_cap = ifelse(is.na(acute_cap), 0, acute_cap))
lsoa_df <- agg_uk_h %>%
  group_by(AreaCodes) %>%
  summarise_all(list(sum))
CCG_df <- agg_uk_h %>%
  merge.data.frame(cw_lsoa_CCG, by.x="AreaCodes", by.y="LSOA11CD") %>%
  dplyr::select(-LSOA11NM, -CCG19CDH, -CCG19NM, -LAD19NM, -LAD19CD, -FID, -AreaCodes) %>%
  group_by(CCG19CD) %>%
  summarise(pop = sum(value),
            fatalities = sum(fatalities),
            hospitalizations = sum(hospitalization),
            hospitalizations_acute = sum(hospitalization_acute))


CCG_df <- sp::merge(CCG_df, hospital_CCG, by="CCG19CD", all.x=TRUE) %>%
  mutate(general_cap = ifelse(is.na(general_cap), 0, general_cap),
         acute_cap = ifelse(is.na(acute_cap), 0, acute_cap))

# merge city of london into greater london
ccounty_df[grepl("GREATER LONDON", ccounty_df$CCTY19NM), 2:7] <- ccounty_df[grepl("GREATER LONDON", ccounty_df$CCTY19NM), 2:7] + ccounty_df[grepl("CITY OF LONDON", ccounty_df$CCTY19NM), 2:7]
ccounty_df <- ccounty_df %>%
  filter(!grepl("CITY OF LONDON", CCTY19NM))

# Address that there are only 7 NHS regions, whereas there are 9 administrative regions. Decompose capacity by pop
dup1 <- c("E12000004", "E12000005")
dup2 <- c("E12000001", "E12000003")

region_df <- region_df %>%
  weight_capacity(dup1) %>%
  weight_capacity(dup2)

# # Make a regional capacity file for use elsewhere
# write_csv(region_df %>%
#   merge.data.frame(cw_lda_region[!duplicated(cw_lda_region$RGN19NM), ]) %>%
#   select(-LAD19CD, -LAD19NM), "data/hospital beds/region_capacity.csv")


## ---- Generate a set of data for some London zoom-ins ---- ##
London_codes <- lsoa_shape$LSOA11CD[grepl("London", lsoa_shape$NAME)]

# plot differences in age structure between two selected LSOA's
London_specific <- readRDS("data/census UK/UK_2018_wide.rds") %>%
  filter(AreaCodes %in% c("E01033583", "E01002225"))
London_specific[, 4:22] <- London_specific[, 4:22] / rowSums(London_specific[, 4:22])
saveRDS(London_specific, "data/for graphs/london_highlight.rds")

# Include demographics and rename variables for ArcGis
uk_wide <- readRDS("data/census UK/UK_2018_wide.rds") %>%
  rename(LSOA11CD = AreaCodes) %>%
  merge.data.frame(cw_lsoa_CCG[, c("LSOA11CD", "CCG19CD", "CCG19NM")], by="LSOA11CD")

head(uk_wide)

uk_wide_ccg <- uk_wide %>%
  mutate(AllAges = as.numeric(gsub("\\,", "", as.character(AllAges)))) %>%
  select(-LSOA11CD, -LSOA) %>%
  group_by(CCG19CD, CCG19NM) %>%
  summarise_all(list(sum))

lsoa_df_final <- lsoa_df %>%
  merge.data.frame(uk_wide, by.x = "AreaCodes", by.y = "LSOA11CD") %>%
  select(-AllAges)
CCG_df_final <- CCG_df %>%
  merge.data.frame(uk_wide_ccg, by = "CCG19CD") %>%
  mutate(CCG19NM = gsub(" CCG", "", CCG19NM))

# Save data part
saveRDS(region_df, "data/for graphs/region_data.rds")
saveRDS(ccounty_df, "data/for graphs/ccounty_data.rds")
saveRDS(lsoa_df, "data/for graphs/lsoa_data.rds")
saveRDS(CCG_df, "data/for graphs/ccg_data.rds")

# Save data part inc. demographics
saveRDS(region_df, "data/for graphs/region_data.rds")
saveRDS(ccounty_df, "data/for graphs/ccounty_data.rds")
saveRDS(lsoa_df_final, "data/for graphs/lsoa_data_dem.rds")
saveRDS(CCG_df_final, "data/for graphs/ccg_data_dem.rds")
