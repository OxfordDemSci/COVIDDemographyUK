rm(list = ls())

source("app_functions.R")
load("data/app.rda")
# load("app/data/graphing.rda")
# load("app/data/cw_app.rda")
# load("data/hosp.rda")
# save(uk_wide, uk, rates, cw_lsoa_CCG, cw_lsoa_ccounty, hospital_ccounty, agg_region_s, agg_ccounty_s, file="app/data/app.rda")
agg_lsoa_shape <- sf::st_read("data/LSOA_complex_alldata_final_simplified.geojson")

packages <- c("tidyverse", "magrittr", "sf", "reshape2", "readxl")
lapply(packages, require, character.only = TRUE)

library(shiny)
library(tidyverse)
library(reshape2)
library(leaflet)

# Define UI ----
ui <- navbarPage("DemSci COVID-19 Geo Risk Tracker",
    tabPanel("Main page",
      sidebarLayout(
        sidebarPanel(width=3,
          h3("Assumed overall infection and hospital capacity"),
          p("Infection rates are assumed constant across age groups. Hospital capacity is calculated relative to the number of hospital beds available under normal circumstancse (measured December 2019)."),
          sliderInput("overall_inf", "Overall Infection Rate",
                      min = 0, max = 1, value=0.10),
          br(),
          sliderInput("overall_hosp_cap", "Hospital Capacity Relative to Normal Circumstances",
                      min = 0.5, max = 5, value=1),
          p("Age-specific hospitalization rates as calculated by Ferguson et al. (2020). Default values assume a 10% infection rate across all age groups and geographies."),
          p("For a discussion of the methodology, see the paper at:"), tags$a(href="https://osf.io/g8s96/", "Paper DOI")),
        mainPanel(
          fluidRow(
            
            column(6, h3("England & Wales Risk Map"),
                   p("Various risk measures and hospital capacity measures can be evaluated for England & Wales. User can also specify which geography to show: the Adminstrative Region, Ceremonial County or Clinical Commisioning Group."),
                   splitLayout(cellWidths = c("50%", "50%"), selectInput("main_geo", "Geography", choices = c("Region", "Ceremonial County", "Clinical Commisioning Group")),
                               selectInput("main_var", "Measure", choices = c("Hospitalization per 1,000", "Hospital bed capacity"))),
                   leafletOutput("main", height=700, width = "100%")),
            column(6, h3("Regional comparison on LSOA level"),
                   p("To zoom in on specific regions, either a Ceremonial County or Clinical Commisioning Group can be selected to view risk measures at the granular LSOA level."),
                   selectInput("z1", "Choose CCG to zoom-in",
                               sort(unique(cw_lsoa_CCG$CCG19NM)),
                               selected=sort(unique(cw_lsoa_CCG$CCG19NM))[1]),
                   br(),
                   leafletOutput("z1_map", height=300, width = "70%"),
                   selectInput("z2", "Choose CCG to zoom-in",
                               sort(unique(cw_lsoa_CCG$CCG19NM)),
                               selected=sort(unique(cw_lsoa_CCG$CCG19NM))[2]),
                   br(),
                   leafletOutput("z2_map", height=300, width = "70%"),
                   ))
          #             ,
          # h3("LSOA Level"),
,
          # 
          # leafletOutput("z2_map", height=550, width = 550),
        )
      )
    ),
    tabPanel("Age-specific rates",
             sidebarLayout(
               sidebarPanel(
                 h3("Age-specific infection and hositalization rates"),
                 p("Given that our knowledge regarding COVID-19 is rapidly evolving, we include the possibility for users to input custom rates per age group. The default values are set to an infection rate of 10% and hospitalization rates as calculated by Ferguson et al. (2020)."),
                 br(),
                 h5("Custom age-specific rates"),
                 p("input custom rates below."),
                 br(),
                 splitLayout(cellWidths = c("30%", "35%", "35%"),
                             HTML("<b>Age</b>"), HTML("<b>Infection rate</b>"),
                             HTML("<b>Hosp. rate</b>")),
                 br(),
                 splitLayout(cellWidths = c("30%", "35%", "35%"),
                             HTML("0 - 9"),
                             numericInput("inf_0_9", NULL,
                                          min = 0, max = 1, value=0.10),
                             numericInput("hosp_0_9", NULL,
                                          min = 0, max = 1, value=0.003)),
                 splitLayout(cellWidths = c("30%", "35%", "35%"),
                             HTML("10 - 19"),
                             numericInput("inf_10_19", NULL,
                                          min = 0, max = 1, value=0.10),
                             numericInput("hosp_10_19", NULL,
                                          min = 0, max = 1, value=0.003)),
                 splitLayout(cellWidths = c("30%", "35%", "35%"),
                             HTML("20 - 29"),
                             numericInput("inf_20_29", NULL,
                                          min = 0, max = 1, value=0.10),
                             numericInput("hosp_20_29", NULL,
                                          min = 0, max = 1, value=0.012)),
                 splitLayout(cellWidths = c("30%", "35%", "35%"),
                             HTML("30 - 39"),
                             numericInput("inf_30_39", NULL,
                                          min = 0, max = 1, value=0.10),
                             numericInput("hosp_30_39", NULL,
                                          min = 0, max = 1, value=0.032)),
                 splitLayout(cellWidths = c("30%", "35%", "35%"),
                             HTML("40 - 49"),
                             numericInput("inf_40_49", NULL,
                                          min = 0, max = 1, value=0.10),
                             numericInput("hosp_40_49", NULL,
                                          min = 0, max = 1, value=0.049)),
                 splitLayout(cellWidths = c("30%", "35%", "35%"),
                             HTML("50 - 59"),
                             numericInput("inf_50_59", NULL,
                                          min = 0, max = 1, value=0.10),
                             numericInput("hosp_50_59", NULL,
                                          min = 0, max = 1, value=0.102)),
                 splitLayout(cellWidths = c("30%", "35%", "35%"),
                             HTML("60 - 69"),
                             numericInput("inf_60_69", NULL,
                                          min = 0, max = 1, value=0.10),
                             numericInput("hosp_60_69", NULL,
                                          min = 0, max = 1, value=0.166)),
                 splitLayout(cellWidths = c("30%", "35%", "35%"),
                             HTML("70 - 79"),
                             numericInput("inf_70_79", NULL,
                                          min = 0, max = 1, value=0.10),
                             numericInput("hosp_70_79", NULL,
                                          min = 0, max = 1, value=0.243)),
                 splitLayout(cellWidths = c("30%", "35%", "35%"),
                             HTML("80 - 89"),
                             numericInput("inf_80_89", NULL,
                                          min = 0, max = 1, value=0.1),
                             numericInput("hosp_80_89", NULL,
                                          min = 0, max = 1, value=0.273)),
                 splitLayout(cellWidths = c("30%", "35%", "35%"),
                             HTML("90+"),
                             numericInput("inf_90p", NULL,
                                          min = 0, max = 1, value=0.10),
                             numericInput("hosp_90p", NULL,
                                          min = 0, max = 1, value=0.273))),
               mainPanel(
                 fluidRow(
                   
                   column(6, h3("England & Wales Risk Map"),
                          p("Various risk measures and hospital capacity measures can be evaluated for England & Wales using custom rates. User can also specify which geography to show: the Adminstrative Region, Ceremonial County or Clinical Commisioning Group."),
                          selectInput("main_geo_custom", "Geography", choices = c("Region", "Ceremonial County", "Clinical Commisioning Group")),
                          leafletOutput("main_custom", height=700, width = "100%")),
                   column(6, h3("Regional comparison on LSOA level"),
                          p("To zoom in on specific regions, either a Ceremonial County or Clinical Commisioning Group can be selected to view risk measures using custom rates at the granular LSOA level."),
                          selectInput("z1", "Choose CCG to zoom-in",
                                      sort(unique(cw_lsoa_CCG$CCG19NM)),
                                      selected=sort(unique(cw_lsoa_CCG$CCG19NM))[1]),
                          br(),
                          leafletOutput("z1_map_custom", height=300, width = "70%"),
                          selectInput("z2", "Choose CCG to zoom-in",
                                      sort(unique(cw_lsoa_CCG$CCG19NM)),
                                      selected=sort(unique(cw_lsoa_CCG$CCG19NM))[2]),
                          br(),
                          leafletOutput("z2_map_custom", height=300, width = "70%"),
                   ))
               )
             )
           ),
    tabPanel("Bivariate Maps",
             sidebarLayout(
               sidebarPanel(width=3,
                            h3("Bivariate maps including ecological variabless"),
                            p("In addition to age-based differentials in risk, a number of additional potential risk factors have been identified among which social deprivation and ethnic background. In order to give a graphical presentation of both age-based risk measures in addition to these typess of risk factors, we include bivariate risk maps. These include age-based risk on one axis and some ecological variable on the other. Due to the complexity of generating bivariate colourmaps, we restrict both variables to three levels, leading to nine categories in total."),
                            h5("Ecological variables"),
                            p("Currently, three ecological measures are supported: social deprivation (IoD2019), the proportion of citizens from Black or Asian descent (ONS, 2011) and the population density (ONS, 2018)."),
                            selectInput("eco_var", "Measure:", choices = c("Social Deprivation", "Percentage Black or Asian", "Population density"))),
               mainPanel())),
    tabPanel("About",
             sidebarLayout(
               sidebarPanel(width=3),
               mainPanel()))
  )



server <- function(input, output) {
  
  lsoa_custom <- reactive({
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
    return(app_aggregate_data_h(uk_wide, rates))
  })
  
  ccg_custom <- reactive({
    lsoa <- lsoa_custom()
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
  
  region_custom <- reactive({
    lsoa <- lsoa_custom()
    region <- app_regional_agg(lsoa, uk, cw_lda_region, hospital_region)
    return(region)
  })
  
  ccounty_custom <- reactive({
    lsoa <- lsoa_custom()
    ccounty <- app_county_agg(lsoa, uk, cw_lsoa_ccounty, hospital_ccounty) %>%
      mutate(general_cap = ifelse(is.na(general_cap), 0, general_cap),
             acute_cap = ifelse(is.na(acute_cap), 0, acute_cap))
    
    return(ccounty)
  })
  
  lsoa <- reactive({
    rates$infection_rate <- input$overall_inf
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
  
  ## -- plottable output
  agg_region <- reactive({
    region <- region()
    agg_region_shape <- sp::merge(agg_region_s[c("RGN19CD", "geometry")],
                                   region,
                                   by="RGN19CD", all.x=T) %>%
      app_create_map_stats() %>%
      left_join(unique(cw_lda_region[c("RGN19CD", "RGN19NM")]))
    return(agg_region_shape)
  })

  agg_region_custom <- reactive({
    region <- region_custom()
    agg_region_shape <- sp::merge(agg_region_s[c("RGN19CD", "geometry")],
                                  region,
                                  by="RGN19CD", all.x=T) %>%
      app_create_map_stats() %>%
      left_join(unique(cw_lda_region[c("RGN19CD", "RGN19NM")]))
    return(agg_region_shape)
  })
    
  agg_ccounty <- reactive({
    ccounty <- ccounty()
    agg_ccounty_shape <- sp::merge(agg_ccounty_s[c("CCTY19NM", "geometry")],
                                   ccounty,
                                   by="CCTY19NM", all.x=T) %>%
      app_create_map_stats()
    return(agg_ccounty_shape)
  })
  
  agg_ccounty_custom <- reactive({
    ccounty <- ccounty_custom()
    agg_ccounty_shape <- sp::merge(agg_ccounty_s[c("CCTY19NM", "geometry")],
                                   ccounty,
                                   by="CCTY19NM", all.x=T) %>%
      app_create_map_stats()
    return(agg_ccounty_shape)
  })
  
  agg_ccg <- reactive({
    ccg <- ccg()
    agg_ccg_shape <- sp::merge(agg_ccg_s[c("CCG19CD", "CCG19NM", "geometry", "general_cap", "acute_cap")],
                               ccg, by="CCG19CD", all.x=T) %>%
      app_create_map_stats()
    return(agg_ccg_shape)
  })

  agg_ccg_custom <- reactive({
    ccg <- ccg_custom()
    agg_ccg_shape <- sp::merge(agg_ccg_s[c("CCG19CD", "geometry")],
                               ccg, by="CCG19CD", all.x=T) %>%
      app_create_map_stats()
    return(agg_ccg_shape)
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
    return(agg_z1)
  })
  
  zoom1_custom <- reactive({
    lsoa <- lsoa_custom()
    lsoa_rel <- cw_lsoa_CCG %>%
      filter(CCG19NM == input$z1)
    z1 <- lsoa %>%
      filter(AreaCodes %in% lsoa_rel$LSOA11CD)
    z1_shape <- agg_lsoa_shape %>%
      filter(AreaCodes %in% lsoa_rel$LSOA11CD)
    
    agg_z1 <- sp::merge(z1_shape[c("AreaCodes", "geometry", "LSOA_Name")],
                        z1, by="AreaCodes", all.x=T) %>%
      app_create_map_stats_lsoa()
    return(agg_z1)
  })

  zoom2 <- reactive({
    lsoa <- lsoa()
    lsoa_rel <- cw_lsoa_CCG %>%
      filter(CCG19NM == input$z2)
    z2 <- lsoa %>%
      filter(AreaCodes %in% lsoa_rel$LSOA11CD)
    z2_shape <- agg_lsoa_shape %>%
      filter(AreaCodes %in% lsoa_rel$LSOA11CD)

    agg_z2 <- sp::merge(z2_shape[c("AreaCodes", "geometry", "LSOA_Name")],
                        z2, by="AreaCodes", all.x=T) %>%
      app_create_map_stats_lsoa()
    return(agg_z2)
  })

  zoom2_custom <- reactive({
    lsoa <- lsoa_custom()
    lsoa_rel <- cw_lsoa_CCG %>%
      filter(CCG19NM == input$z2)
    z2 <- lsoa %>%
      filter(AreaCodes %in% lsoa_rel$LSOA11CD)
    z2_shape <- agg_lsoa_shape %>%
      filter(AreaCodes %in% lsoa_rel$LSOA11CD)
    
    agg_z2 <- sp::merge(z2_shape[c("AreaCodes", "geometry", "LSOA_Name")],
                        z2, by="AreaCodes", all.x=T) %>%
      app_create_map_stats_lsoa()
    return(agg_z2)
  })
  
  ## -- maps
  
  output$main <- renderLeaflet({
    if (length(input$main_geo) == 0) {
      shape_df <- agg_ccounty() %>%
        rename(Name = CCTY19NM)
    } else if (input$main_geo == "Ceremonial County") {
      shape_df <- agg_ccounty() %>%
        rename(Name = CCTY19NM)
    } else if (input$main_geo == "Clinical Commisioning Group") {
      shape_df <- agg_ccg() %>%
        rename(Name = CCG19NM)
    } else if (input$main_geo == "Region") {
      shape_df <- agg_region() %>%
        rename(Name = RGN19NM)
    }

    mytext <- paste(
      "Name", ": <b>", shape_df$Name, 
      "</b><br/>",
      "Population", ": <b>", shape_df$pop, 
      "</b><br/>",
      "Hospitalizations per 1,000", ": <b>", round(shape_df$pc_hosp, 1), 
      "</b><br/>",
      "Hospitcal bed capacity", ": <b>", shape_df$general_cap, 
      sep="") %>%
      lapply(htmltools::HTML)
    
    leaflet(shape_df %>%
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
      setView(lat = mean(loc$Y), lng = mean(loc$X), zoom=9) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity=0.5,
                  fillColor = ~colorQuantile("YlOrRd", pc_hosp)(pc_hosp),
                  label=mytext)
  })

  output$z2_map <- renderLeaflet({
    z2 <- zoom2() %>% sf::st_as_sf() %>%
      sf::st_transform(crs="+init=epsg:4326")
    loc <- as.data.frame(st_coordinates(z2[1]))
    mytext <- paste(
      "Name", ": <b>", z2$LSOA,
      "</b><br/>",
      "Population", ": <b>", z2$pop,
      "</b><br/>",
      "Hospitalizations per 1,000", ": <b>", round(z2$pc_hosp, 1),
      sep="") %>%
      lapply(htmltools::HTML)

    leaflet(z2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = mean(loc$Y), lng = mean(loc$X), zoom=9) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity=0.5,
                  fillColor = ~colorQuantile("YlOrRd", pc_hosp)(pc_hosp),
                  label=mytext)
  })
  
  output$main_custom <- renderLeaflet({
    if (length(input$main_geo_custom) == 0) {
      shape_df <- agg_ccounty_custom() %>%
        rename(Name = CCTY19NM)
    } else if (input$main_geo_custom == "Ceremonial County") {
      shape_df <- agg_ccounty_custom() %>%
        rename(Name = CCTY19NM)
    } else if (input$main_geo_custom == "Clinical Commisioning Group") {
      shape_df <- agg_ccg_custom() %>%
        rename(Name = CCG19NM)
    } else if (input$main_geo_custom == "Region") {
      shape_df <- agg_region_custom() %>%
        rename(Name = RGN19NM)
    }
    
    mytext <- paste(
      "Name", ": <b>", shape_df$Name, 
      "</b><br/>",
      "Population", ": <b>", shape_df$pop, 
      "</b><br/>",
      "Hospitalizations per 1,000", ": <b>", round(shape_df$pc_hosp, 1), 
      "</b><br/>",
      "Hospitcal bed capacity", ": <b>", shape_df$general_cap, 
      sep="") %>%
      lapply(htmltools::HTML)
    
    leaflet(shape_df %>%
              sf::st_transform(crs="+init=epsg:4326")) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = 52.35, lng = -1.25, zoom=5.7) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity=0.5,
                  fillColor = ~colorQuantile("YlOrRd", pc_hosp)(pc_hosp),
                  label=mytext)
  })
  
  output$z1_map_custom <- renderLeaflet({
    z1 <- zoom1_custom() %>% sf::st_as_sf() %>%
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
      setView(lat = mean(loc$Y), lng = mean(loc$X), zoom=9) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity=0.5,
                  fillColor = ~colorQuantile("YlOrRd", pc_hosp)(pc_hosp),
                  label=mytext)
  })
  
  output$z2_map_custom <- renderLeaflet({
    z2 <- zoom2_custom() %>% sf::st_as_sf() %>%
      sf::st_transform(crs="+init=epsg:4326")
    loc <- as.data.frame(st_coordinates(z2[1]))
    mytext <- paste(
      "Name", ": <b>", z2$LSOA,
      "</b><br/>",
      "Population", ": <b>", z2$pop,
      "</b><br/>",
      "Hospitalizations per 1,000", ": <b>", round(z2$pc_hosp, 1),
      sep="") %>%
      lapply(htmltools::HTML)
    
    leaflet(z2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = mean(loc$Y), lng = mean(loc$X), zoom=9) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity=0.5,
                  fillColor = ~colorQuantile("YlOrRd", pc_hosp)(pc_hosp),
                  label=mytext)
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)
