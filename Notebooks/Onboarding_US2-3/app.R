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
library(readxl)
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

hist_df <- dbReadTable(con, "sensor_historical")

dbDisconnect(con)



prob_df <- hist_df %>%
  group_by(KerbsideID, Zone_Number, Latitude, Longitude, Status_Description) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Status_Description, values_from = Count, values_fill = 0) %>%
  mutate(
    Zone_Number = as.integer(Zone_Number),
    Total = rowSums(across(c(Present, Unoccupied))),
    Prob_Unoccupied = ifelse(Total > 0, Unoccupied / Total, NA_real_),
    Prob_Unoccupied = sprintf("%.2f%%", Prob_Unoccupied * 100),
    Status_Description = "Unoccupied",
    Latitude = as.numeric(str_trim(Latitude)),
    Longitude = as.numeric(str_trim(Longitude))
  ) %>%
  select(Zone_Number, KerbsideID, Latitude, Longitude, Status_Description, Prob_Unoccupied)


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
  
  # Show lat/lon on when clicked
  output$click_coords <- renderPrint({
    click <- input$CBD_map_click
    if (is.null(click)){
      cat("Drop a pin to find the nearby historical parking availability trends.")
    } else{
      cat("Pinned Location:", 
          "\nLatitude: ", round(click$lat, 5), 
          "\nLongitude: ", round(click$lng, 5))
    }
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
    prob_df$distance <- distHaversine(
      matrix(c(prob_df$Longitude, prob_df$Latitude), ncol = 2),
      click_coords
    )
    
    # Get 5 closest locations
    closest <- prob_df %>% arrange(distance) %>% slice_head(n = 5)
    
    # The table showing relevant info
    table_data <- closest %>%
      left_join(zone_info, by = c("Zone_Number" = "zone_number")) %>%
      mutate(
        Latitude = ifelse(is.na(Latitude), "Not Applicable", as.character(Latitude)),
        Longitude = ifelse(is.na(Longitude), "Not Applicable", as.character(Longitude)),
        Restriction = ifelse(is.na(restriction_display), "Not Applicable", restriction_display),
        Availability = ifelse(is.na(Prob_Unoccupied), "Not Applicable", Prob_Unoccupied),
        Location = ifelse(is.na(on_street), "Not Applicable", on_street)
      ) %>%
      select(Location, Restriction, Availability)
    
    spot_data(table_data)
    
    # Label for spot info
    label <- paste0(
      "Zone Number: ", ifelse(is.na(closest$Zone_Number), "NA", closest$Zone_Number),
      "<br/>Kerbside ID: ", ifelse(is.na(closest$KerbsideID), "NA", closest$KerbsideID),
      "<br/>Prob. of being available: ", ifelse(is.na(closest$Prob_Unoccupied), "NA", closest$Prob_Unoccupied)
    )
    
    # Update map
    leafletProxy("CBD_map") %>%
      setView(lng = click$lng, lat = click$lat, zoom = 18) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addMarkers(lng = click$lng, lat = click$lat) %>%
      addCircles(
        lng = closest$Longitude,
        lat = closest$Latitude,
        radius = 3,
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
