

app_aggregate_data_h <- function(uk_wide, rates) {
  agg_uk <- uk_wide %>%
    dplyr::select(-AllAges) %>%  # Remove the original total count
    melt(id.vars = c("AreaCodes", "LSOA")) %>%  # Melt along areacodes, with
    rename(age = variable) %>%  # rename to connect to the rates
    mutate(age = gsub("Age_", "", age)) %>%  # reformate the new age column to be in line with the rate age column
    mutate(age = gsub("_plus", "\\+", age)) %>%
    left_join(rates, by="age") %>%  # join census and rates
    mutate(fatalities = value * lethality_rate * infection_rate,
           hospitalization = value * rate * infection_rate,
           hospitalization_acute = value * rate * infection_rate * acute_rate) %>%  # calculate fatalities expected per age column per district
    dplyr::select(AreaCodes, value, fatalities, hospitalization, hospitalization_acute) %>%  # retain only geo-codes, total pop and fatalities
    group_by(AreaCodes) %>%  # groupby geo ID
    summarise_all(list(sum)) %>%  # sum all to get total pop and total expected fatalities
    rename(population = value)
    return(agg_uk)
}

app_get_rates <- function(path = "data/overall_rates.xlsx", custom, interpolate = TRUE, max=10) {
  input <- read_excel(path)  # read an excell with lethality_rates and infection_rates per 10-year interval
  input$min_age <- as.numeric(gsub("-.*|\\+", "", input$age))  # define min age in bin
  input$max_age <- input$min_age + 9  # define max age in bin (not neede for 90+, addressed later)
  input$infection_rate <- custom$inf_rate
  input$rate <- custom$hosp_rate
  
  if (interpolate) {  # Interpolation assuming to 5 year intervals from 10 year intervals, ten bins assumed including 90+
    new_input <- input[1, ]  # first row remains the same
    new_input$age <- '0-4'  # will be 0-4 age
    
    for (i in 2 : max) {  # for each secondary row
      row1 <- input[i, ]  # make two new rows based on the selected row
      row2 <- input[i, ]  # make two new rows based on the selected row
      
      # update lethality along the following rationale. current rates are for age averages (so the 20-30 rate is the 25 year old's rate)
      # this assumes uniform distribution within bin, which is unreasonable >> can be addressed later. Pseudo-code:
      # 1. Take difference between current age rate (e.g. at 35) and last rate (e.g. at 25). We are interested in the 27.5 and 32.5 rates
      # 2. Divide the delta into four parts.
      # 3. Add 1/4 delta to the i-1 rate for the first row (at 27.5) and 3/4 delta to the i-1 rate for the second row (at 32.5)
      delta <- input$lethality_rate[i] - input$lethality_rate[i - 1]
      row1$lethality_rate <- input$lethality_rate[i - 1] + delta / 4
      row2$lethality_rate <- input$lethality_rate[i - 1] +  3 / 4 * delta
      
      delta <- input$rate[i] - input$rate[i - 1]
      delta_acute <- input$acute_rate[i] - input$acute_rate[i - 1]
      row1$rate <- input$rate[i - 1] + delta / 4
      row2$rate <- input$rate[i - 1] +  3 / 4 * delta
      row1$acute_rate <- input$acute_rate[i - 1] + delta_acute / 4
      row2$acute_rate <- input$acute_rate[i - 1] +  3 / 4 * delta_acute
      
      row1$min_age <- input$min_age[i] - 5  # update new age
      row1$max_age <- input$min_age[i] - 1
      row1$age <- paste0(row1$min_age, "-", row1$max_age)
      row2$min_age <- input$min_age[i]
      row2$max_age <- input$min_age[i] + 4
      row2$age <- paste0(row2$min_age, "-", row2$max_age)
      
      # concatenate new rows
      new_input <- rbind(new_input, row1, row2)
    }
    new_input <- rbind(new_input, input[max, ])
    new_input <- new_input[c(1:((max - 1)*2), (max * 2)), ]
    
    input <- new_input  %>%
      dplyr::select(age, rate, infection_rate, acute_rate, lethality_rate)
  }
  return(input)
}


app_regional_agg <- function(agg_uk, uk, cw_lda_region, hospital_region) {
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
    dplyr::select(population, fatalities, RGN19CD, general_cap, acute_cap, hospitalization,
                  hospitalization_acute) %>%
    group_by(RGN19CD) %>%
    summarise(pop = sum(population),
              fatalities = sum(fatalities),
              general_cap = mean(general_cap),
              acute_cap = mean(acute_cap),
              hospitalizations = sum(hospitalization),
              hospitalizations_acute = sum(hospitalization_acute),
              geo_code = unique(RGN19CD))
  return(agg_region)
}

app_county_agg <- function(agg_uk, uk, cw_lsoa_ccounty, hospital_county) {
  ### Generate regionally aggregated set with fatalities, general capacity, acute capacity and expected hospitalizations
  
  agg_uk_c <- merge.data.frame(agg_uk, cw_lsoa_ccounty[, c("LSOA11CD", "NAME")],
                               by.x = "AreaCodes", by.y="LSOA11CD")  %>%
    rename(CCTY19NM = NAME)  # connect ceremonial counties to LSOA
  
  agg_uk_c$CCTY19NM <- toupper(agg_uk_c$CCTY19NM)
  
  # Small unit test
  print(paste0("All Hospital IDS can be found in census data?: ", all(hospital_county$county %in% agg_uk_c$CCTY19NM)))
  
  agg_uk_beds <- merge.data.frame(agg_uk_c, hospital_county, by.x ="CCTY19NM", by.y = "county", all.x=T)
  
  agg_county <- agg_uk_beds %>%
    dplyr::select(population, fatalities, CCTY19NM, general_cap, acute_cap, hospitalization,
                  hospitalization_acute) %>%
    group_by(CCTY19NM) %>%
    summarise(pop = sum(population),
              fatalities = sum(fatalities),
              general_cap = mean(general_cap),
              acute_cap = mean(acute_cap),
              hospitalizations = sum(hospitalization),
              hospitalizations_acute = sum(hospitalization_acute),
              geo_name = unique(CCTY19NM))
  
  return(agg_county)
}

app_create_map_stats <- function(df) {
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

app_create_map_stats_lsoa <- function(df) {
  return(df %>%
           mutate(pc_hosp = hospitalization / population * 1000,
                  pc_hosp_acute = hospitalization_acute / population * 1000))
}
