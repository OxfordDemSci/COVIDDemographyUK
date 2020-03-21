
read_region_hospital <- function() {
  ### Read information on hospital capacity on the regional level
  acute <- read.csv("data/hospital beds/hospital_accute.csv") %>%
    mutate(Region = gsub(" Comm.*", "", Region)) %>%
    mutate(Region = toupper(gsub(" And York.*| Of Eng.*", "", Region))) %>%
    dplyr::select(Region, Acute) %>%
    group_by(Region) %>%
    summarise(Acute = sum(Acute))
  
  total <- read.csv("data/hospital beds/hospital_beds.csv") %>%
    dplyr::select(Region.Code, AT.Name, General...Acute) %>%
    mutate(AT.Name = gsub(" COMM.*", "", AT.Name),
           AT.Name = trimws(AT.Name),
           AT.Name = gsub(" OF ENGLAND", "", AT.Name)) %>%
    filter(AT.Name != "England") %>%
    merge.data.frame(acute, by.x = "AT.Name", by.y="Region") %>%
    mutate(general_cap = as.numeric(gsub("\\,", "", General...Acute))) %>%
    rename(acute_cap = Acute) %>%
    dplyr::select(-General...Acute)
  return(total)
}

read_county_hospital <- function(path="data/hospital beds/all_hospitals_county.csv") {
  ### Read information on hospital capacity on the county level
  hospital_county <- read.csv(path) %>%
    left_join(read.csv("data/hospital beds/hospital_accute.csv") %>%
                dplyr::select(Org, Acute)) %>%
           mutate(X.1 = as.numeric(gsub("\\,", "", X.1)),
                  X.1 = ifelse(is.na(X.1), 0, X.1),
                  Acute = ifelse(is.na(Acute), 0, Acute)) %>%
           rename(general_cap = X.1,
                  county = A5,
                  acute_cap = Acute) %>%
           dplyr::select(general_cap, county, acute_cap) %>%
           group_by(county) %>%
           summarise(general_cap = sum(general_cap),
                     acute_cap = sum(acute_cap))
  hospital_county$county <- as.character(hospital_county$county)
  hospital_county$county[grepl("AVON", hospital_county$county)] <- "BRISTOL"
  hospital_county$county[grepl("MIDDLESEX", hospital_county$county)] <- "GREATER LONDON"
  hospital_county$county[grepl("NORTH HUMBER", hospital_county$county)] <- "EAST RIDING OF YORKSHIRE"
  hospital_county$county[grepl("SOUTH HUMBER", hospital_county$county)] <- "LINCOLNSHIRE"
  hospital_county$county[grepl("CLEVELAND", hospital_county$county)] <- "DURHAM"
  hospital_county$county[grepl("COUNTY DURHAM", hospital_county$county)] <- "DURHAM"
  hospital_county <- hospital_county %>%
    group_by(county) %>%
    summarise_all(list(sum))

  return(hospital_county)
}


regional_agg <- function(agg_uk, uk, cw_lsoa_region, hospital_region) {
  ### Generate regionally aggregated set with fatalities, general capacity, acute capacity and expected hospitalizations
  
  agg_uk_c <- merge.data.frame(agg_uk, uk, by.x = "AreaCodes", by.y="Area.Codes")
  agg_uk_r <- merge.data.frame(agg_uk_c, cw_lda_region, by.x = "LSOA", by.y ="LAD19NM")
  
  agg_uk_f <- agg_uk_r
  agg_uk_f$RGN19NM <- as.character(agg_uk_f$RGN19NM)
  agg_uk_f$RGN19NM[grepl("Midlands", agg_uk_f$RGN19NM)] <- "Midlands"
  agg_uk_f$RGN19NM[grepl("Yorkshire", agg_uk_f$RGN19NM)] <- "North East"
  agg_uk_f$RGN19NM <- gsub("of England", "", agg_uk_f$RGN19NM)
  agg_uk_f$RGN19NM <- trimws(agg_uk_f$RGN19NM)
  agg_uk_f$RGN19NM <- toupper(agg_uk_f$RGN19NM)
  
  agg_uk_beds <- merge.data.frame(agg_uk_f, hospital_region, by.x ="RGN19NM", by.y = "AT.Name") %>%
    dplyr::select(-LSOA, -LAD19CD, -RGN19NM, -FID, -Region.Code)
  
  agg_region <- agg_uk_beds %>%
    dplyr::select(value, fatalities, RGN19CD, general_cap, acute_cap, hospitalization,
                  hospitalization_acute) %>%
    group_by(RGN19CD) %>%
    summarise(pop = sum(value),
              fatalities = sum(fatalities),
              general_cap = mean(general_cap),
              acute_cap = mean(acute_cap),
              hospitalizations = sum(hospitalization),
              hospitalizations_acute = sum(hospitalization_acute),
              geo_code = unique(RGN19CD))
  return(agg_region)
}

county_agg <- function(agg_uk, uk, cw_lsoa_ccounty, hospital_county) {
  ### Generate regionally aggregated set with fatalities, general capacity, acute capacity and expected hospitalizations
  
  agg_uk_c <- merge.data.frame(agg_uk, cw_lsoa_ccounty[, c("LSOA11CD", "NAME")],
                               by.x = "AreaCodes", by.y="LSOA11CD")  %>%
    rename(CCTY19NM = NAME)  # connect ceremonial counties to LSOA
  
  agg_uk_c$CCTY19NM <- toupper(agg_uk_c$CCTY19NM)
  
  # Small unit test
  print(paste0("All Hospital IDS can be found in census data?: ", all(hospital_county$county %in% agg_uk_c$CCTY19NM)))
  
  agg_uk_beds <- merge.data.frame(agg_uk_c, hospital_county, by.x ="CCTY19NM", by.y = "county", all.x=T)
  
  agg_county <- agg_uk_beds %>%
    dplyr::select(value, fatalities, CCTY19NM, general_cap, acute_cap, hospitalization,
                  hospitalization_acute) %>%
    group_by(CCTY19NM) %>%
    summarise(pop = sum(value),
              fatalities = sum(fatalities),
              general_cap = mean(general_cap),
              acute_cap = mean(acute_cap),
              hospitalizations = sum(hospitalization),
              hospitalizations_acute = sum(hospitalization_acute),
              geo_name = unique(CCTY19NM))
  
  return(agg_county)
}

change_LDA <- function(uk) {
  ### Rename some columns to 2019 administrative levels
  uk$LSOA[grepl("Heath", uk$LSOA)] <- "West Suffolk"
  uk$LSOA[grepl("Edmundsbury", uk$LSOA)] <- "West Suffolk"
  uk$LSOA[grepl("West Somerset", uk$LSOA)] <- "Somerset West and Taunton"
  uk$LSOA[grepl("Taunton", uk$LSOA)] <- "Somerset West and Taunton"
  uk$LSOA[grepl("Suffolk Coastal|Waveney", uk$LSOA)] <- "East Suffolk"
  uk$LSOA[grepl("Shepway", uk$LSOA)] <- "Folkestone and Hythe"
  uk$LSOA[grepl("Shepway", uk$LSOA)] <- "Folkestone and Hythe"
  uk$LSOA[grepl("Poole|Bournemouth|Christchurch", uk$LSOA)] <- "Bournemouth, Christchurch and Poole"
  uk$LSOA[grepl("East Dorset|North Dorset|West Dorset|Weymouth|Purbec", uk$LSOA)] <- "Dorset"
  return(uk)
}

weight_capacity <- function(df, dup) {
  ### Disaggregate regions into sub-regions for plotting. Weighin capacity by population
  capacity <- df$general_cap[df$RGN19CD %in% dup][1]
  acute_capacity <- df$acute_cap[df$RGN19CD %in% dup][1]
  weighted_population <- df$pop[df$RGN19CD %in% dup] / sum(df$pop[df$RGN19CD %in% dup])
  df$general_cap[df$RGN19CD %in% dup] <-  round(capacity * weighted_population )
  df$acute_cap[df$RGN19CD %in% dup] <-  round(acute_capacity * weighted_population )
  return(df)
}

read_wales_hospital <- function(ccounty_shape) {
  # Need to merge two of the polygons to one in the ceremonial counties list
  wales <- readRDS("data/wales_bed_data/nhs_wales_beds_cleaned.rds") %>%
    dplyr::select(name, beds, intensive_care_beds)
  wales <- st_join(wales, ccounty_shape, largest = TRUE) %>%
    dplyr::select(NAME, beds, intensive_care_beds)
  
  wales <- `st_geometry<-`(wales, NULL) %>%
    rename(general_cap = beds,
           acute_cap = intensive_care_beds,
           county = NAME) %>%
    mutate(acute_cap = round(acute_cap))
  
  ## In the Welsh data, Gwynedd and Clwyd are merged together. Unmerge and divide capacity along population.
  ## Total population in Gwynedd and Clwyd is 698K, 504K live in Gwynedd, 194K live in Clwyd.
  wales$general_cap[wales$county == "GWYNEDD"] <- round(wales$general_cap[wales$county == "GWYNEDD"]) * (198 / 698)
  wales$acute_cap[wales$county == "GWYNEDD"] <- round(wales$acute_cap[wales$county == "GWYNEDD"]) * (198 / 698)
  wales <- rbind(wales, c("CLWYD", 2221 * (504 / 689), 31 * (504/689)))
  return(wales)
}
