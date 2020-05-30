## Script for Covid-19 geographical risk analysis
## Author: Mark Verhagen (LCDS)
## Created: 16-03-2020

# Load packages
packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "sp", "gpclib", "maptools", "spdep",
              "maptools", "RColorBrewer", "lattice", "gridExtra", "sf", "reshape2", "spData", "rgeos")
lapply(packages, require, character.only = TRUE)

# Load functions
source("src/graphing_functions.R")

# Load Data
load("data/uk_census_data.rda")  # census data on UK, 2018
load("data/crosswalks.rda")  # crosswalks between geographic aggregation levels
load("data/shapefiles.rda") # shapefiles

# Capacity data for hospital beds (Region and Ceremonial County level)
hospital_region <- read_region_hospital()
hospital_ccounty <- rbind(read_county_hospital(), read_wales_hospital(ccounty_shape)) %>%
  mutate(general_cap = as.numeric(general_cap),
         acute_cap = as.numeric(acute_cap))
hospital_CCG <- readRDS("data/hospital beds/ccg_capacity.rds") %>%
  as.data.frame() %>%
  dplyr::select(-CCG19NM, -LAT, -LONG) %>%
  group_by(CCG19CD) %>%
  summarise_all(list(sum))

# Read the original census file and clean the Local Authority District names to 2019 versions
uk <- readRDS("data/census UK/UK_2018_all.rds") %>%
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

## ---- Generate a set of data for some London zoom-ins ---- ##

# plot differences in age structure between two selected LSOA's
London_specific <- uk_wide %>%
  filter(AreaCodes %in% c("E01033583", "E01002225"))
London_specific[, 4:22] <- London_specific[, 4:22] / rowSums(London_specific[, 4:22])

# Include demographics and rename variables for ArcGis
uk_wide_cw <- uk_wide %>%
  merge.data.frame(cw_lsoa_CCG[, c("LSOA11CD", "CCG19CD", "CCG19NM")], by.x = "AreaCodes", by.y="LSOA11CD") %>%
  mutate(AllAges = as.numeric(gsub(",", "", AllAges)))

uk_wide_ccg <- uk_wide_cw %>%
  mutate(AllAges = as.numeric(gsub("\\,", "", as.character(AllAges)))) %>%
  dplyr::select(-LSOA, -AreaCodes) %>%
  group_by(CCG19CD, CCG19NM) %>%
  summarise_all(list(sum))

lsoa_df_final <- lsoa_df %>%
  merge.data.frame(uk_wide, by = "AreaCodes") %>%
  dplyr::select(-AllAges)

CCG_df_final <- CCG_df %>%
  merge.data.frame(uk_wide_ccg, by = "CCG19CD") %>%
  mutate(CCG19NM = gsub(" CCG", "", CCG19NM))

# Save data
save(region_df, ccounty_df, lsoa_df_final, CCG_df_final, London_specific, file = "data/agg_data.rda")
