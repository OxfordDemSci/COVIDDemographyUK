# Join together the results from the parameter grid search for the ArcGIS dashboard

library(tidyverse)
library(geojsonsf)
library(sf)

ccg_fns <- c(
  'data/final/ccg_df_0.5_1.geojson',
  'data/final/ccg_df_0.5_1.5.geojson',
  'data/final/ccg_df_0.5_2.geojson',
  'data/final/ccg_df_1_1.geojson',
  'data/final/ccg_df_1_1.5.geojson',
  'data/final/ccg_df_1_2.geojson',
  'data/final/ccg_df_2_1.geojson',
  'data/final/ccg_df_2_1.5.geojson',
  'data/final/ccg_df_2_2.geojson'
  )

ccg_infection_rates <- c('5%', '5%', '5%', '10%', '10%', '10%', '20%', '20%', '20%') %>% as.character()
ccg_hosp_cap <- rep(c('100%', '150%', '200%'), 3)

get_geojson <- function(fn, infection_rate, hosp_cap) {
  df <- geojson_sf(fn)
  df$infection_rate <- infection_rate
  df$hosp_cap <- hosp_cap
  return(df)
}

ccgs <- pmap(list(ccg_fns, ccg_infection_rates, ccg_hosp_cap), get_geojson) %>%
  reduce(rbind)

ccgs <- select(
  ccgs,
  CCG19CD,
  ccg_name = CCG19NM.x,
  infection_rate,
  hosp_cap,
  Population,
  CCG_Usual_GeneralCapacity,
  CCG_Usual_CriticalCareCapacity,
  pc_capacity,
  pc_capacity_acute,
  Tipping_Point_Hospitalization_UsualCapacity,
  Tipping_Point_CriticalCare_UsualCapacity,
  Expected_Hospitalizations,
  Expected_Fatalities,
  Expected_Critical_Care,
  Expected_Hospitalizations_per1000,
  Expected_CriticalCare_per1000,
  abs_excess_demand_hosp,
  abs_excess_demand_hosp_acute,
  BNG_N,
  BNG_E,
  Age_0.4,
  Age_5.9,
  Age_10.14,
  Age_15.19,
  Age_20.24,
  Age_25.29,
  Age_30.34,
  Age_35.39,
  Age_40.44,
  Age_45.49,
  Age_50.54,
  Age_55.59,
  Age_60.64,
  Age_65.69,
  Age_70.74,
  Age_75.79,
  Age_80.84,
  Age_85.89,
  Age_90_plus
  )

lsoa_fns <- c(
  'data/final/lsoa_df_0.5_None.geojson',
  'data/final/lsoa_df_1_None.geojson',
  'data/final/lsoa_df_2_None.geojson'
)

lsoa_infection_rates <- c('5%', '10%', '20%')
lsoa_hosp_cap <- rep(NA, 3)

lsoas <- pmap(list(lsoa_fns, lsoa_infection_rates, lsoa_hosp_cap), get_geojson) %>%
  reduce(rbind)

lsoas <- select(
  lsoas,
  AreaCodes,
  LSOA_Name,
  infection_rate,
  hosp_cap,
  Population,
  Expected_Hospitalizations,
  Expected_Critical_Care,
  Expected_Fatalities,
  Expected_Hospitalizations_per1000,
  Expected_CriticalCare_per1000,
  Expected_Fatalities_per1000 = pc_fatailties,
  `Pop_0-4`,
  `Pop_5-9`,
  `Pop_10-14`,
  `Pop_15-19`,
  `Pop_20-24`,
  `Pop_25-29`,
  `Pop_30-34`,
  `Pop_35-39`,
  `Pop_40-44`,
  `Pop_45-49`,
  `Pop_50-54`,
  `Pop_55-59`,
  `Pop_60-64`,
  `Pop_65-69`,
  `Pop_70-74`,
  `Pop_75-79`,
  `Pop_80-84`,
  `Pop_85-89`,
  Pop_90_plus
)

ccgs <- sf_geojson(ccgs)
lsoas <- sf_geojson(lsoas)

write_lines(ccgs, 'data/final/ccgs_grid.geojson')
write_lines(lsoas, 'data/final/lsoas_grid.geojson')
