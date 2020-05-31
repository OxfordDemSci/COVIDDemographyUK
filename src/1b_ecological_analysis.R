## Script for Covid-19 geographical risk analysis, final data
## Author: Mark Verhagen (LCDS)
## Created: 30-04-2020

## Load packages
packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "sp", "gpclib", "maptools", "spdep", "raster", "rgdal",
              "maptools", "RColorBrewer", "lattice", "gridExtra", "sf", "reshape2", "spData", "rgeos",
              "maps")
lapply(packages, require, character.only = TRUE)

## load old data
load("data/agg_data.rda")
load("data/crosswalks.rda")

lsoa_df <- lsoa_df_final
ccg_df <- CCG_df_final

CW_lsoa_ccg <- cw_lsoa_CCG %>%
  dplyr::select(LSOA11CD, CCG19NM) %>%
  rename(LSOA = LSOA11CD,
         NAME = CCG19NM)
CW_lsoa_cc <- cw_lsoa_ccounty %>%
  dplyr::select(LSOA11CD, NAME) %>%
  rename(LSOA = LSOA11CD) %>%
  mutate(NAME = toupper(NAME)) %>%
  mutate(NAME = gsub("\\&", "AND", NAME),
         NAME = gsub("CITY AND COUNTY OF THE CITY OF LONDON", "GREATER LONDON", NAME))

## -- LSOA level -- ##

## deprivation
depriv_eng <- read.csv("data/deprivation/LSOA_Deprivation.csv") %>%
  rename(LSOA = LSOA.code..2011.,
         depriv = Index.of.Multiple.Deprivation..IMD..Decile) %>%
  mutate(depriv = abs(depriv - 11))

depriv_wales <- read.csv("data/deprivation/WLSOA_Deprivation.csv") %>%
  rename(LSOA = LSOA.Code,
         depriv = WIMD.2019)
depriv_wales$depriv <- as.numeric(as.character(depriv_wales$depriv))

deciles_wales <- round(quantile(depriv_wales$depriv, seq(0, 1, 0.1)))
deciles_wales[length(deciles_wales)] <- deciles_wales[length(deciles_wales)] + 1

for (i in 1 : 10) {
  depriv_wales$depriv <- ifelse((depriv_wales$depriv >= deciles_wales[i]) & (depriv_wales$depriv < deciles_wales[i + 1]),
                                i, depriv_wales$depriv)
}

lsoa_depriv <- rbind(depriv_eng, depriv_wales)

lsoa_depriv$depriv <- as.numeric(as.character(lsoa_depriv$depriv))


## lsoa_ethnicity
lsoa_eth <- read.csv("data/ethnicity/LSOA_eth.csv")

all(lsoa_eth$pop  == rowSums(lsoa_eth[, c(3:12)]))

lsoa_eth$White <- rowSums(lsoa_eth[, c(3:4)]) / lsoa_eth$pop
lsoa_eth$Mixed <- lsoa_eth$mixed / lsoa_eth$pop
lsoa_eth$Asian <- rowSums(lsoa_eth[, c(6:10)]) / lsoa_eth$pop
lsoa_eth$Black <- lsoa_eth$black / lsoa_eth$pop
lsoa_eth$Other <- lsoa_eth$other / lsoa_eth$pop

lsoa_eth <- lsoa_eth %>%
  dplyr::select(LSOA, pop, White, Mixed, Asian, Black, Other)

all(round(rowSums(lsoa_eth[, c(3:7)]), 2) == 1)

lsoa_eth$Non_White <- lsoa_eth$Mixed + lsoa_eth$Asian + lsoa_eth$Black + lsoa_eth$Other
lsoa_eth$Risk <- lsoa_eth$Asian + lsoa_eth$Black

## density

lsoa_dens <- read.csv("data/density/LSOA_density.csv") %>%
  rename(LSOA = Code,
         pop = Mid.2018.population,
         area = Area.Sq.Km,
         dens = People.per.Sq.Km) %>%
  dplyr::select(LSOA, pop, area, dens)

lsoa_dens <- lsoa_dens %>%
  mutate(area = as.numeric(area),
         pop = as.numeric(gsub("\\,", "", pop))) %>%
  mutate(dens = pop / area)

sum(lsoa_dens$pop) == round(sum(lsoa_dens$area * lsoa_dens$dens))

## -- CCG level -- ##

lsoa_depriv <- lsoa_depriv %>%
  left_join(lsoa_dens[, c("LSOA", "pop")])

## deprivation
all(lsoa_depriv$LSOA %in% CW_lsoa_ccg$LSOA)

ccg_depriv <- lsoa_depriv %>%
  left_join(CW_lsoa_ccg) %>%
  group_by(NAME) %>%
  summarise(depriv = weighted.mean(depriv, pop),
            pop = sum(pop))

## ethnicity

ccg_eth <- lsoa_eth %>%
  left_join(CW_lsoa_ccg) %>%
  group_by(NAME) %>%
  summarise(White = weighted.mean(White, pop),
            Mixed = weighted.mean(Mixed, pop),
            Asian = weighted.mean(Asian, pop),
            Black = weighted.mean(Black, pop),
            Other = weighted.mean(Other, pop),
            Non_White = weighted.mean(Non_White, pop),
            Risk = weighted.mean(Risk, pop))

all(round(rowSums(ccg_eth[, c(2:6)]), 2) == 1)

ccg_dens <- lsoa_dens %>%
  left_join(CW_lsoa_ccg) %>%
  group_by(NAME) %>%
  summarise(pop = sum(pop),
            area = sum(area)) %>%
  mutate(dens = pop / area)

sum(ccg_dens$pop) == round(sum(ccg_dens$area) * weighted.mean(ccg_dens$dens, ccg_dens$area))

## -- CC level -- ##

## deprivation

cc_depriv <- lsoa_depriv %>%
  left_join(CW_lsoa_cc) %>%
  group_by(NAME) %>%
  summarise(depriv = weighted.mean(depriv, pop),
            pop = sum(pop))
sum(cc_depriv$pop) == sum(lsoa_depriv$pop)

## ethnicity
cc_eth <- lsoa_eth %>%
  left_join(CW_lsoa_cc) %>%
  group_by(NAME) %>%
  summarise(White = weighted.mean(White, pop),
            Mixed = weighted.mean(Mixed, pop),
            Asian = weighted.mean(Asian, pop),
            Black = weighted.mean(Black, pop),
            Other = weighted.mean(Other, pop),
            Non_White = weighted.mean(Non_White, pop),
            Risk = weighted.mean(Risk, pop))

## density

cc_dens <- lsoa_dens %>%
  left_join(CW_lsoa_cc) %>%
  group_by(NAME) %>%
  summarise(pop = sum(pop),
            area = sum(area)) %>%
  mutate(dens = pop / area)

## -- Combine sets -- ##
CCG_eco_vars <- ccg_depriv %>%
  left_join(ccg_eth) %>%
  left_join(ccg_dens)

lsoa_eth$pop <- NULL  # 2011 census
LSOA_eco_vars <- lsoa_depriv %>%
  left_join(lsoa_eth) %>%
  left_join(lsoa_dens)

CC_eco_vars <- cc_depriv %>%
  left_join(cc_eth) %>%
  left_join(cc_dens)

save(LSOA_eco_vars,
     CCG_eco_vars,
     CC_eco_vars,
     file = "data/eco_vars.rda")
# save(LSOA_eco_vars,
#      CCG_eco_vars,
#      CC_eco_vars,
#      file = "app/data/eco_vars.rda")
