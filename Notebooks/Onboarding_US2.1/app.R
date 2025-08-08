#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)
library(geosphere) 
library(DBI)
library(RMariaDB)

# Establish connection to database
con <- dbConnect(
  RMariaDB::MariaDB(),
  host = "104.131.14.255",
  user = "malone",
  password = "malone",
  dbname = "mpa"
)

dbListTables(con) # Show all existing data

zone_df <- dbReadTable(con, "zones")
zone_segment_df <- dbReadTable(con, "zone_segment_bridge")

zone_info <- zone_df %>%
  select(zone_number = id, restriction_display) %>%
  left_join(zone_segment_df %>% select(zone_number = zone_id, on_street), by = "zone_number")

dbDisconnect(con)



# Get real time sensor data
get_avaiable_spots <- function(limit = 100){
  url <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/on-street-parking-bay-sensors/records?where=status_description%20%3D%20%22Unoccupied%22&limit=100"
  
  res <- GET(url)
  
  content_json <- content(res, as = "text", encoding = "UTF-8")
  data <- fromJSON(content_json, flatten = TRUE)$results
  
  # Extract latitude and longitude
  data <- data %>%
    mutate(
      Latitude = as.numeric(location.lat),
      Longitude = as.numeric(location.lon)
    ) %>%
    select(status_description, zone_number, kerbsideid, Latitude, Longitude, lastupdated)
  
  return(data)
}
avaiable_parking <- get_avaiable_spots()



# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
      column(
        width = 12,
        align = "center",
        leafletOutput("CBD_map", height = "800px", width = "800px")
      )
    ),
    fluidRow(
      column(
        width = 12,
        align = "center",
        tableOutput("spot_info")
      )
    )
    
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Initial empty map
    output$CBD_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
        setView(lng = 144.9631, lat = -37.8136, zoom = 15)
    })
    
    
    # Add a marker at the clicked location
    observeEvent(input$CBD_map_click, {
      click <- input$CBD_map_click
      leafletProxy("CBD_map") %>%
        clearMarkers() %>%
        clearShapes() %>% # clear previous circle
        addMarkers(lng = click$lng, lat = click$lat)
    })
    
    spot_data <- reactiveVal(NULL)
    
    # Add 5 closest unoccupied parking spots when clicked
    observeEvent(input$CBD_map_click, {
      click <- input$CBD_map_click
      click_coords<- c(click$lng, click$lat)
      
      # Calculate distances
      avaiable_parking$distance <- distHaversine(
        matrix(c(avaiable_parking$Longitude, avaiable_parking$Latitude), ncol = 2),
        click_coords
      )
      
      # Get 5 closest locations
      closest <- avaiable_parking %>% arrange(distance) %>% slice_head(n = 5)
      
      # The table showing relevant info
      table_data <- closest %>%
        left_join(zone_info, by = c("zone_number")) %>%
        select(
          `Last Updated` = lastupdated,
          `Zone Number` = zone_number,
          `Restriction` = restriction_display,
          `Street` = on_street
        )
      spot_data(table_data)
      
      # Label for spot info
      label <- paste0(
        "Zone Number: ", ifelse(is.na(closest$zone_number), "NA", closest$zone_number),
        "<br/>Kerbside ID: ", ifelse(is.na(closest$kerbsideid), "NA", closest$kerbsideid)
      )
      
      # Update map
      leafletProxy("CBD_map") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 16) %>%
        clearMarkers() %>%
        clearShapes() %>%
        addMarkers(lng = click$lng, lat = click$lat) %>%
        addCircles(
          lng = closest$Longitude,
          lat = closest$Latitude,
          radius = 10,
          fillOpacity = 0.5,
          color = "green",
          label = lapply(label, htmltools::HTML)
        )
      
      output$spot_info <- renderTable({
        df <- spot_data()
        if (is.null(df)) return()
        
        df %>% mutate_all(~ ifelse(is.na(.), "Not Applicable", .))
      })
      
      
      
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
