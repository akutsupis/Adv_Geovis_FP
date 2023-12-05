
# Imports
library(readr)
library(sf)
library(rsconnect)
library(leaflet)
library(shiny)

# This file is small enough to host and import straight from GitHub.
data <- read_csv("estat_sdg_09_60_en.csv")
nuts_sf <- st_read('nuts_shapefile.shp')

rsconnect::setAccountInfo(name='akutsupis',
                          token='E500DAACDB2AC397B9286C1670C5B485',
                          secret='8be5/z9H1v9W4fqbGe3w7HFb3nc0LOn3XiOyjHs/')

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
              step = 1, ticks = FALSE, sep = ''),
  leafletOutput("map")
)

# Define Server
server <- function(input, output) {
  # Filter data based on the selected year
  filtered_data <- reactive({
    req(input$year_slider)
    
    merged_data[merged_data$TIME_PERIOD == input$year_slider, ]
  })
  
  # observe({
  #   print(filtered_data())  # Print the filtered data for debugging
  # })
  
  output$map <- renderLeaflet({
    # Create a leaflet map
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data = filtered_data(),
                  fillColor = colorNumeric("Blues", domain = NULL)(filtered_data()$OBS_VALUE),
                  fillOpacity = 0.6,
                  weight = 2,
                  color = "white",
                  popup = ~paste("Country: ", filtered_data()$CountryCode, "<br>",
                                 "Freight Mode: ", filtered_data()$tra_mode,"<br>",
                                 "Percent: ", filtered_data()$OBS_VALUE
                  )
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)

