rm(list = ls())

# setwd("/Users/markverhagen/Dropbox/Academic Work/Cooperative work/COVIDDemographyUK/app")
source("app_functions.R")
load("data/app.rda")
# load("data/graphing.rda")
# load("data/cw_app.rda")
# load("data/hosp.rda")
save(uk_wide, rates, cw_lsoa_CCG, cw_lsoa_ccounty, hospital_ccounty, agg_ccounty_s, file="data/app.rda")
agg_lsoa_shape <- sf::st_read("data/LSOA_complex_alldata_final_simplified.geojson")

packages <- c("tidyverse", "magrittr", "sf", "reshape2", "readxl")
lapply(packages, require, character.only = TRUE)

library(shiny)
library(tidyverse)
library(reshape2)
library(leaflet)

# Define UI ----
ui <- fluidPage(
  titlePanel("Oxford Demographic Science COVID-19"),
  sidebarLayout(
    sidebarPanel(
      h3("Assumed overall infection and hospital capacity"),
      p("Infection rates are assumed constant across age groups. Hospital capacity is calculated relative to the number of hospital beds available under normal circumstancse (measured December 2019)."),
      sliderInput("overall_inf", "Overall Infection Rate",
                  min = 0, max = 1, value=0.10),
      br(),
      sliderInput("overall_hosp_cap", "Hospital Capacity Relative to Normal Circumstances",
                  min = 0.5, max = 5, value=1),
      h4("Use age specific infection / hospitalization rates"),
      p("Input age-specific infection and hospitalization rates. The default values assume a 10% infection rate across all age groups and hospitalization rates as calculated in Ferguson et al. 2020."),
      checkboxInput("custom_inf", "Use age-specific infection rates"),
      conditionalPanel(
        condition = "input.custom_inf == true",
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("inf_0_9", h6("Infection rate: 0-9"),
                                 min = 0, max = 1, value=0.10),
                    numericInput("hosp_0_9", h6("Hospitalization rate: 0-9"),
                                 min = 0, max = 1, value=0.001)),
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("inf_10_19", h6("Infection rate: 10-19"),
                                 min = 0, max = 1, value=0.10),
                    numericInput("hosp_10_19", h6("Hospitalization rate: 10-19"),
                                 min = 0, max = 1, value=0.003)),
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("inf_20_29", h6("Infection rate: 20-29"),
                                 min = 0, max = 1, value=0.10),
                    numericInput("hosp_20_29", h6("Hospitalization rate: 20-29"),
                                 min = 0, max = 1, value=0.003)),
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("inf_30_39", h6("Infection rate: 30-39"),
                                 min = 0, max = 1, value=0.10),
                    numericInput("hosp_30_39", h6("Hospitalization rate: 30-39"),
                                 min = 0, max = 1, value=0.003)),
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("inf_40_49", h6("Infection rate: 40-49"),
                                 min = 0, max = 1, value=0.10),
                    numericInput("hosp_40_49", h6("Hospitalization rate: 40-49"),
                                 min = 0, max = 1, value=0.003)),
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("inf_50_59", h6("Infection rate: 50-59"),
                                 min = 0, max = 1, value=0.10),
                    numericInput("hosp_50_59", h6("Hospitalization rate: 50-59"),
                                 min = 0, max = 1, value=0.003)),
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("inf_60_69", h6("Infection rate: 60-69"),
                                 min = 0, max = 1, value=0.10),
                    numericInput("hosp_60_69", h6("Hospitalization rate: 60-69"),
                                 min = 0, max = 1, value=0.003)),
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("inf_70_79", h6("Infection rate: 70-79"),
                                 min = 0, max = 1, value=0.10),
                    numericInput("hosp_70_79", h6("Hospitalization rate: 70-79"),
                                 min = 0, max = 1, value=0.003)),
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("inf_80_89", h6("Infection rate: 80-89"),
                                 min = 0, max = 1, value=0.10),
                    numericInput("hosp_80_89", h6("Hospitalization rate: 80-89"),
                                 min = 0, max = 1, value=0.003)),
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("inf_90p", h6("Infection rate: 90+"),
                                 min = 0, max = 1, value=0.10),
                    numericInput("hosp_90p", h6("Hospitalization rate: 90+"),
                                 min = 0, max = 1, value=0.003))),
      
      "For a discussion of the methodology, see the paper at ", 
      span("DOI", style = "color:blue")
    ),
    mainPanel(
      h3("Hospitalization risk in England & Wales"),
      leafletOutput("mymap", height=650, width = 550),
      h3("LSOA Level"),
      selectInput("z1", "Choose CCG to zoom-in",
                  sort(unique(cw_lsoa_CCG$CCG19NM)),
                  selected=sort(unique(cw_lsoa_CCG$CCG19NM))[1]),
      leafletOutput("z1_map", height=550, width = 550),
      # selectInput("z2", "Choose CCG to zoom-in",
      #             sort(unique(cw_lsoa_CCG$CCG19NM)),
      #             selected=sort(unique(cw_lsoa_CCG$CCG19NM))[2]),
      # leafletOutput("z2_map", height=550, width = 550),
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  lsoa <- reactive({
    if (input$custom_inf) {
      custom_inf <- c(input$inf_0_9, input$inf_10_19, input$inf_20_29,
                      input$inf_30_39, input$inf_40_49, input$inf_50_59,
                      input$inf_60_69, input$inf_70_79, input$inf_80_89,
                      input$inf_90p)
      custom_hosp <- c(input$hosp_0_9, input$hosp_10_19, input$hosp_20_29,
                       input$hosp_30_39, input$hosp_40_49, input$hosp_50_59,
                       input$hosp_60_69, input$hosp_70_79, input$hosp_80_89,
                       input$hosp_90p)
      custom_rates <- data.frame(inf_rate = custom_inf, hosp_rate = custom_hosp)
      rates <- app_get_rates(custom=custom_rates)
    } else {
      rates$infection_rate <- input$overall_inf
    }
      return(app_aggregate_data_h(uk_wide, rates))
  })
  
  ccg <- reactive({
    lsoa <- lsoa()
    ccg <- lsoa %>%
      merge.data.frame(cw_lsoa_CCG, by.x = "AreaCodes", by.y = "LSOA11CD") %>%
      dplyr::select(-LSOA11NM, -CCG19CDH, -CCG19NM, -LAD19NM, -LAD19CD, -FID, -AreaCodes) %>%
      group_by(CCG19CD) %>%
      summarise(pop = sum(population),
                fatalities = sum(fatalities),
                hospitalizations = sum(hospitalization),
                hospitalizations_acute = sum(hospitalization_acute))
    return(ccg)
  })
  
  region <- reactive({
    lsoa <- lsoa()
    region <- app_regional_agg(lsoa, uk, cw_lda_region, hospital_region)
    return(region)
  })
  
  ccounty <- reactive({
    lsoa <- lsoa()
    ccounty <- app_county_agg(lsoa, uk, cw_lsoa_ccounty, hospital_ccounty) %>%
      mutate(general_cap = ifelse(is.na(general_cap), 0, general_cap),
             acute_cap = ifelse(is.na(acute_cap), 0, acute_cap))
    
    return(ccounty)
  })
  
  agg_ccounty <- reactive({
    ccounty <- ccounty()
    print(head(ccounty))
    print(head(agg_ccounty_s[c("CCTY19NM", "geometry")]))
    agg_ccounty_shape <- sp::merge(agg_ccounty_s[c("CCTY19NM", "geometry")],
                                   ccounty,
                                   by="CCTY19NM", all.x=T) %>%
      app_create_map_stats()
    return(agg_ccounty_shape)
  })
  
  zoom1 <- reactive({
    lsoa <- lsoa()
    lsoa_rel <- cw_lsoa_CCG %>%
      filter(CCG19NM == input$z1)
    z1 <- lsoa %>%
      filter(AreaCodes %in% lsoa_rel$LSOA11CD)
    z1_shape <- agg_lsoa_shape %>%
      filter(AreaCodes %in% lsoa_rel$LSOA11CD)
    
    agg_z1 <- sp::merge(z1_shape[c("AreaCodes", "geometry", "LSOA_Name")],
                        z1, by="AreaCodes", all.x=T) %>%
      app_create_map_stats_lsoa()
    print(head(agg_z1))
    return(agg_z1)
  })

  # zoom2 <- reactive({
  #   lsoa <- lsoa()
  #   lsoa_rel <- cw_lsoa_CCG %>%
  #     filter(CCG19NM == input$z2)
  #   z2 <- lsoa %>%
  #     filter(AreaCodes %in% lsoa_rel$LSOA11CD)
  #   z2_shape <- agg_lsoa_shape %>%
  #     filter(AreaCodes %in% lsoa_rel$LSOA11CD)
  #   
  #   agg_z2 <- sp::merge(z2_shape[c("AreaCodes", "geometry", "LSOA")],
  #                       z2, by="AreaCodes", all.x=T) %>%
  #     app_create_map_stats_lsoa()
  #   print(head(agg_z2))
  #   return(agg_z2)
  # })
  
  output$mymap <- renderLeaflet({
    agg_ccounty_s <- agg_ccounty()
    mytext <- paste(
      "Name", ": <b>", agg_ccounty_s$CCTY19NM, 
      "</b><br/>",
      "Population", ": <b>", agg_ccounty_s$pop, 
      "</b><br/>",
      "Hospitalizations per 1,000", ": <b>", round(agg_ccounty_s$pc_hosp, 1), 
      "</b><br/>",
      "Hospitcal bed capacity", ": <b>", agg_ccounty_s$general_cap, 
      sep="") %>%
      lapply(htmltools::HTML)
    
    leaflet(agg_ccounty_s %>%
              sf::st_transform(crs="+init=epsg:4326")) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = 52.35, lng = -1.25, zoom=5.7) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity=0.5,
                  fillColor = ~colorQuantile("YlOrRd", pc_hosp)(pc_hosp),
                  label=mytext)
  })
  
  output$z1_map <- renderLeaflet({
    z1 <- zoom1() %>% sf::st_as_sf() %>%
      sf::st_transform(crs="+init=epsg:4326")
    loc <- as.data.frame(st_coordinates(z1[1]))
    mytext <- paste(
      "Name", ": <b>", z1$LSOA, 
      "</b><br/>",
      "Population", ": <b>", z1$pop, 
      "</b><br/>",
      "Hospitalizations per 1,000", ": <b>", round(z1$pc_hosp, 1),
      sep="") %>%
      lapply(htmltools::HTML)
    
    leaflet(z1) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = mean(loc$Y), lng = mean(loc$X), zoom=11) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity=0.5,
                  fillColor = ~colorQuantile("YlOrRd", pc_hosp)(pc_hosp),
                  label=mytext)
  })

  # output$z2_map <- renderLeaflet({
  #   z2 <- zoom2() %>% sf::st_as_sf() %>%
  #     sf::st_transform(crs="+init=epsg:4326")
  #   loc <- as.data.frame(st_coordinates(z2[1]))
  #   mytext <- paste(
  #     "Name", ": <b>", z2$LSOA, 
  #     "</b><br/>",
  #     "Population", ": <b>", z2$pop, 
  #     "</b><br/>",
  #     "Hospitalizations per 1,000", ": <b>", round(z2$pc_hosp, 1),
  #     sep="") %>%
  #     lapply(htmltools::HTML)
  #   
  #   leaflet(z2) %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     setView(lat = mean(loc$Y), lng = mean(loc$X), zoom=11) %>%
  #     addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
  #                 opacity = 1.0, fillOpacity=0.5,
  #                 fillColor = ~colorQuantile("YlOrRd", pc_hosp)(pc_hosp),
  #                 label=mytext)
  # })
}


# Run the app ----
shinyApp(ui = ui, server = server)
