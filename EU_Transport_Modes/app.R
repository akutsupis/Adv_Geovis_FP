# To load keys to environment:
# source("shiny_keys.R")
# connect()
# Source:
# https://ec.europa.eu/eurostat/databrowser/view/nrg_ind_ren/default/table?lang=en

# Imports
library(readr)
library(sf)
library(rsconnect)
library(leaflet)
library(shiny)

# This file is small enough to host and import straight from GitHub.
data <- read_csv("nrg_ind_ren_page_linear.csv")
nuts_sf <- st_read('nuts_shapefile.shp')
countrycodes <- read_csv("CountryCodes.csv")

# Filter to the country level
nuts_sf_filtered <- nuts_sf[nuts_sf$STAT_LEVL_ == 1, ]

nuts_sf_filtered <- st_transform(nuts_sf_filtered, "+proj=longlat +datum=WGS84")

# Select the first two characters as merge keys
nuts_sf_filtered$CountryCode <- substr(nuts_sf_filtered$NUTS_ID, 1, 2)

# Merge on country code keys
merged_data <- merge(nuts_sf_filtered, data, by.x = "CountryCode", by.y = "geo", all.x = TRUE)

merged_data <- merge(merged_data, countrycodes, by.x = "CountryCode", by.y = "CountryCode", all.x = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("EU Share of Energy from Renewable Sources"),
  tags$h4("Percent of Gross Final Energy Consumption"), ## Header
  checkboxInput("show_change", "Show change between years", FALSE),
  uiOutput("year_input"),  ## Slider or range input
  leafletOutput("map", width = "100%", height = "800px"),
  tags$hr(),  ## Horizontal line
  tags$p("Sources:"),  ## Paragraph
  tags$ul(
    tags$li(tags$a(href = "https://ec.europa.eu/eurostat/databrowser/view/nrg_ind_ren/default/table?lang=en", "Eurostat Data")),
    tags$li(tags$a(href = "https://github.com/akutsupis/Adv_Geovis_FP", "GitHub"))
  )
)

# Define Server
server <- function(input, output, session) {
  # Update the slider based on checkbox
  output$year_input <- renderUI({
    if(!input$show_change) { 
      sliderInput("year_slider", "Select Year:",
                  min = min(merged_data$TIME_PERIOD, na.rm = TRUE),
                  max = max(merged_data$TIME_PERIOD, na.rm = TRUE),
                  value = min(merged_data$TIME_PERIOD, na.rm = TRUE),
                  step = 1, ticks = FALSE, sep = '')
    } else {
      sliderInput("year_range", "Select Years:", 
                  min = min(merged_data$TIME_PERIOD, na.rm = TRUE), 
                  max = max(merged_data$TIME_PERIOD, na.rm = TRUE), 
                  value = c(min(merged_data$TIME_PERIOD, na.rm = TRUE), min(merged_data$TIME_PERIOD, na.rm = TRUE)+9), 
                  step = 1, sep = '')
    }
  })
  
  ## Filter data based on the selected year or range
  filtered_data <- reactive({
    if(!input$show_change) { 
      merged_data[merged_data$TIME_PERIOD == input$year_slider, ]
    } else {
      req(input$year_range)
      start_year_data <- merged_data[merged_data$TIME_PERIOD == input$year_range[1], ] 
      end_year_data <- merged_data[merged_data$TIME_PERIOD == input$year_range[2], ] 
      percentage_change_data <- end_year_data
      percentage_change_data$OBS_VALUE <- (round(end_year_data$OBS_VALUE - start_year_data$OBS_VALUE,2))
      percentage_change_data
    }
  })
  
  colorpal <- reactive({
    if(!input$show_change) {
      colorBin("RdYlGn", filtered_data()$OBS_VALUE, bins = 10)
    } else {
      colorBin("RdYlGn", filtered_data()$OBS_VALUE, bins = 10)
    }
  })
  
  # Create leaflet map with static elements
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 11, lat = 52, zoom = 3.8) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = filtered_data(),
                  fillColor = ~colorpal()(OBS_VALUE),
                  fillOpacity = 0.1,
                  weight = 2,
                  color = "white",
                  popup = paste("Country: ", filtered_data()$CountryCode, "<br>",
                                if(!input$show_change) {"Percent: "} else {"Percent Change: "}, 
                                round(filtered_data()$OBS_VALUE,digits=2)))
  })
  
  # Observe changes in filtered_data and update the map
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~colorpal()(OBS_VALUE), fillOpacity = 0.6,
                  weight = 2, color = "white",
                  popup = paste("Country: ", filtered_data()$Name, "<br>",
                                if(!input$show_change) {"Percent: "} else {"Percent Change: "}, 
                                round(filtered_data()$OBS_VALUE,digits=2)))
  })
}

# Run the Shiny app
shinyApp(ui, server)
