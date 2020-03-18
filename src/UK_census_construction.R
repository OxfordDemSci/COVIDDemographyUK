rm(list = ls())
## Script to generate UK census at five or ten year intervals, non-sex disaggregated
## Author: Mark Verhagen (LCDS)
## Created: 16-03-2020
## Code Review:


# Load packages
packages <- c("tidyverse", "tidycensus", "magrittr", "readxl", "lattice", "reshape2", "dplyr")
lapply(packages, require, character.only = TRUE)

# Load functions
source("src/functions.r")

# Load Data
uk <- read.csv("data/census UK/UK_2018_all.csv") %>%
  filter(LA..2019.boundaries. == "")  # Drops the county aggregated numbers
names(uk) <- gsub("X|\\.", "", names(uk))  # change column names of the form X\\d{1} to just the digit

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
           dplyr::select(AreaCodes, contains("Age")))
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

# Turn data to wide, aggregated by age bin
uk_wide <- aggregate_years(uk)
test_aggregates(uk_wide)  # unit test

get_rates2 <- function(path = "data/mortality/fatality_infection_rates.xlsx", interpolate = TRUE) {
  input <- read_excel(path)  # read an excell with lethality_rates and infection_rates per 10-year interval
  input$min_age <- as.numeric(gsub("-.*|\\+", "", input$age))  # define min age in bin
  input$max_age <- input$min_age + 9  # define max age in bin (not neede for 90+, addressed later)
  
  if (interpolate) {  # Interpolation assuming to 5 year intervals from 10 year intervals, ten bins assumed including 90+
    new_input <- input[1, ]  # first row remains the same
    new_input$age <- '0-4'  # will be 0-4 age
    
    for (i in 2 : 10) {  # for each secondary row
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
      
      row1$min_age <- input$min_age[i] - 5  # update new age
      row1$max_age <- input$min_age[i] - 1
      row1$age <- paste0(row1$min_age, "-", row1$max_age)
      row2$min_age <- input$min_age[i]
      row2$max_age <- input$min_age[i] + 4
      row2$age <- paste0(row2$min_age, "-", row2$max_age)
      
      # concatenate new rows
      new_input <- rbind(new_input, row1, row2)  
    }
    new_input <- rbind(new_input, input[10, ])
    new_input <- new_input[c(1:18, 20), ]
    
    input <- new_input  %>%
      dplyr::select(age, lethality_rate, infection_rate)
  }
  return(input)
}


# Get rates
# rates <- get_rates2(interpolate = FALSE)  ## for 10-year intervals
rates <- get_rates2()

aggregate_data <- function(uk_wide, rates) {
  agg_uk <- uk_wide %>%
    dplyr::select(-AllAges) %>%  # Remove the original total count
    melt(id.vars = "AreaCodes") %>%  # Melt along areacodes, with
    rename(age = variable) %>%  # rename to connect to the rates
    mutate(age = gsub("Age_", "", age)) %>%  # reformate the new age column to be in line with the rate age column
    mutate(age = gsub("_plus", "\\+", age)) %>%
    left_join(rates, by="age") %>%  # join census and rates
    mutate(fatalities = value * lethality_rate * infection_rate) %>%  # calculate fatalities expected per age column per district
    dplyr::select(AreaCodes, value, fatalities) %>%  # retain only geo-codes, total pop and fatalities
    group_by(AreaCodes) %>%  # groupby geo ID
    summarise_all(list(sum))  # sum all to get total pop and total expected fatalities
  return(agg_uk)
}

agg_uk <- aggregate_data(uk_wide, rates)

## Connect shapefile and plot
plot_libs <- c("gridExtra", "sf", "sp", "gpclib", "maptools", "spdep",
               "maptools", "RColorBrewer")
lapply(packages, require, character.only = TRUE)

shape_uk <- sf::st_read("shapefiles/UK/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC.shp") %>%
  rename(AreaCodes = LSOA11CD)
shape_data_uk <- sp::merge(shape_uk, agg_uk)
shape_data_uk$relative_hazard <- shape_data_uk$fatalities / shape_data_uk$value

city <- "Liverpool"
shape_data_city <- shape_data_uk %>%
  filter(grepl(city, LSOA11NMW))

png(paste0("figs/maps/", city, ".png"))
ggplot(data = shape_data_city[, c("relative_hazard", "geometry")], aes(fill = relative_hazard, geometry = geometry)) + geom_sf() +
  theme_linedraw() + ggtitle(paste0("Percentage of population at risk ", city)) +
  scale_fill_distiller(palette = 'PRGn') +
  labs(caption='Source: Mid-18 Population Estimates, Office for National Staistics. Assuming 40% infection rate across age groups', fill = "Percentage at risk")
dev.off()
