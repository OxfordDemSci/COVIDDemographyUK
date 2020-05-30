get_rates2 <- function(path = "data/mortality/overall_rates.xlsx", interpolate = TRUE, max=10) {
  input <- read_excel(path)  # read an excell with lethality_rates and infection_rates per 10-year interval
  input$min_age <- as.numeric(gsub("-.*|\\+", "", input$age))  # define min age in bin
  input$max_age <- input$min_age + 9  # define max age in bin (not neede for 90+, addressed later)

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


aggregate_years <- function (uk, five = TRUE) {
  if (five) {
    steps <- seq(0, 85, 5)
    step_size <- 4  #delta aggregation step, e.g. 0 to 4
  } else{
    steps <- seq(0, 80, 10)
    step_size <- 9  #delta aggregation step, e.g. 0 to 9
  }

  for (i in steps) {
    col_name <- paste0('Age_', i, '-', (i + step_size))  # Name the nem column to be created
    uk[, col_name] <- 0  # Assign zero as starting value
    for (j in (i: (i + step_size))) {  # loop from minimum year to maximum year of the age bin
      uk[, as.character(j)] <- gsub("\\,", "", as.character(uk[, as.character(j)]))  # take away commas in thousands
      uk[, col_name] <- uk[, col_name] + as.numeric(uk[, as.character(j)])  # transform to numeric, add each year count to the new column
    }
    uk[, col_name] <- as.numeric(uk[, col_name])  # transform to numeric (should be unnecessary)
  }
  uk$Age_90_plus <- as.numeric(as.character(gsub("\\,", "", uk$`90`)))  # explicitly make the 90+ column
  return(uk %>%
           dplyr::select(AreaCodes, LSOA, contains("Age")))
}

test_aggregates <- function(uk) {
  total_pop_2018 <- sum(as.numeric(gsub("\\,", "", uk$AllAges)))
  print(paste0("Total population mid-2018 in England and Wales: ", total_pop_2018))
  if (all(as.numeric(gsub("\\,", "", uk$AllAges)) == rowSums(uk %>% dplyr::select(starts_with("Age"))))) {
    print("Age-binned columns add up to the total ")
  } else {
    warning("Age-binned columns DO NOT add up to the total ")
  }
}

aggregate_data_h <- function(uk_wide, rates) {
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
    summarise_all(list(sum))  # sum all to get total pop and total expected fatalities
  return(agg_uk)
}


get_h_rates2 <- function(path = "data/mortality/hospital_infection_rate.xlsx", interpolate = TRUE, max=10) {
  input <- read_excel(path) %>%
    rename(rate = hospital_rate)  # read an excell with lethality_rates and infection_rates per 10-year interval
  input$min_age <- as.numeric(gsub("-.*|\\+", "", input$age))  # define min age in bin
  input$max_age <- input$min_age + 9  # define max age in bin (not neede for 90+, addressed later)

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
      dplyr::select(age, rate, infection_rate, acute_rate)
  }
  return(input)
}

generate_agg <- function(uk_wide, h_multiplier = 1, i_multiplier = 1) {
  rates <- get_rates2() %>%
    mutate(rate = rate * h_multiplier,
           infection_rate = infection_rate * i_multiplier)
  return(aggregate_data_h(uk_wide, rates))
}

rename_arcgis_ccg <- function(df) {
  return(df %>%
    rename(Population = pop,
           Expected_Fatalities = fatalities,
           Expected_Hospitalizations = hospitalizations,
           Expected_Critical_Care = hospitalizations_acute,
           CCG_Usual_GeneralCapacity = general_cap,
           CCG_Usual_CriticalCareCapacity = acute_cap,
           Expected_Hospitalizations_per1000 = pc_hosp,
           Expected_CriticalCare_per1000= pc_hosp_acute,
           Tipping_Point_Hospitalization_UsualCapacity = tipping_point_capacity,
           Tipping_Point_CriticalCare_UsualCapacity = tipping_point_capacity_acute))
}

rename_arcgis_lsoa <- function(df) {
  return(df %>%
    rename(Expected_Fatalities = fatalities,
           Expected_Hospitalizations = hospitalization,
           Expected_Critical_Care = hospitalization_acute,
           Expected_Hospitalizations_per1000 = pc_hosp,
           Expected_CriticalCare_per1000= pc_hosp_acute))
}
