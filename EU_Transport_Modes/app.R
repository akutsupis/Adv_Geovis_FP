
# Imports
library(readr)
library(sf)
library(rsconnect)
library(leaflet)
library(shiny)

# This file is small enough to host and import straight from GitHub.
data <- read_csv("nrg_ind_ren_page_linear.csv")
nuts_sf <- st_read('nuts_shapefile.shp')

source("shiny_keys.R")
connect()

# Filter to the country level
nuts_sf_filtered <- nuts_sf[nuts_sf$STAT_LEVL_ == 1, ]

nuts_sf_filtered <- st_transform(nuts_sf_filtered, "+proj=longlat +datum=WGS84")

# Select the first two characters as merge keys
nuts_sf_filtered$CountryCode <- substr(nuts_sf_filtered$NUTS_ID, 1, 2)

# Merge on country code keys
merged_data <- merge(nuts_sf_filtered, data, by.x = "CountryCode", by.y = "geo", all.x = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("EU Freight Movement Visualization"),
  sliderInput("year_slider", "Select Year:",
              min = min(merged_data$TIME_PERIOD, na.rm = TRUE),
              max = max(merged_data$TIME_PERIOD, na.rm = TRUE),
              value = min(merged_data$TIME_PERIOD, na.rm = TRUE),
              step = 1, ticks = FALSE, sep = ','),
  leafletOutput("map")
)

# Define Server
server <- function(input, output, session) {
  # Filter data based on the selected year and drop NAs
  filtered_data <- reactive({
    req(input$year_slider)
    na.omit(merged_data[merged_data$TIME_PERIOD == input$year_slider, ])
  })
  
  colorpal <- reactive({
    colorNumeric("Blues", merged_data$OBS_VALUE)
  })
  
  # Create leaflet map with static elements
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = filtered_data(),
                  fillColor = colorpal()(filtered_data()$OBS_VALUE),
                  fillOpacity = 0.6,
                  weight = 2,
                  color = "white",
                  popup = ~paste("Country: ", filtered_data()$CountryCode, "<br>",
                                 "Freight Mode: ", filtered_data()$tra_mode, "<br>",
                                 "Percent: ", filtered_data()$OBS_VALUE)
      )
  })
  
  
  
  # Observe changes in filtered_data and update the map
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filtered_data()) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~pal(OBS_VALUE), fillOpacity = 0.6,
                  weight = 2, color = "white",
                  popup = ~paste("Country: ", CountryCode, "<br>",
                                 "Freight Mode: ", tra_mode, "<br>",
                                 "Percent: ", OBS_VALUE)
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)


