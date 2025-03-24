# server.R
# Marine Anything Mapper (MAM)
# This file contains the server-side logic for the app.
# It uses reactive values to store up to 10 user-added map points,
# simulates loading environmental raster data from a PostGIS database,
# and executes a placeholder linear model predicting similarity based on
# three environmental variables: bathymetry, chlorophyll concentration, and sea surface temperature.

library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)
library(terra)

# Define the server function
server <- function(input, output, session) {
  
  # -------------------------------
  # Reactive storage for user-added markers
  # -------------------------------
  rv <- reactiveValues(
    # Data frame to hold markers: id, latitude, and longitude
    markers = data.frame(id = numeric(0), lat = numeric(0), lng = numeric(0))
  )
  
  # -------------------------------
  # Pseudo-code: Connect to a PostGIS database
  # -------------------------------
  # In a production app, replace the following with real connection code:
  # con <- DBI::dbConnect(RPostgres::Postgres(),
  #                       dbname = "your_database",
  #                       host = "your_host",
  #                       port = 5432,
  #                       user = "your_username",
  #                       password = "your_password")
  # onStop(function() {
  #   DBI::dbDisconnect(con)
  # })
  
  # -------------------------------
  # Pseudo-code: Load raster layers for environmental variables
  # -------------------------------
  # Replace with real raster loading code using terra:
  # bathymetry_raster <- terra::rast("path_to_bathymetry.tif")
  # chlorophyll_raster <- terra::rast("path_to_chlorophyll.tif")
  # sst_raster <- terra::rast("path_to_sst.tif")
  # For simulation, we use a dummy function to generate environmental values.
  
  simulate_env_values <- function(lat, lng) {
    # Simulate environmental variable values as simple functions of lat/lng.
    # This is a placeholder for actual raster data extraction.
    bathymetry  <- 1000 - (lat * 10)         # Example: Decreases with latitude
    chlorophyll <- abs(sin(lng)) * 5 + 2        # Example: Varies with longitude
    sst         <- 20 + cos(lat / 10) * 3        # Example: Modulated by latitude
    return(list(bathymetry = bathymetry,
                chlorophyll = chlorophyll,
                sst = sst))
  }
  
  # -------------------------------
  # Initialize the Leaflet map with an ocean-friendly basemap
  # -------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.OceanBasemap") %>%  # Ocean-friendly basemap layer
      setView(lng = 0, lat = 0, zoom = 2)         # Global view as default
  })
  
  # -------------------------------
  # Handle user map clicks to add points (up to 10 points)
  # -------------------------------
  observeEvent(input$map_click, {
    click <- input$map_click
    if(nrow(rv$markers) < 10) {
      # Generate a new unique marker id
      new_id <- ifelse(nrow(rv$markers) == 0, 1, max(rv$markers$id) + 1)
      new_marker <- data.frame(id = new_id, lat = click$lat, lng = click$lng)
      rv$markers <- rbind(rv$markers, new_marker)
      
      # Add the marker to the map with a unique layerId (used later for removal)
      leafletProxy("map") %>%
        addMarkers(lng = click$lng, lat = click$lat, layerId = as.character(new_id))
    }
  })
  
  # -------------------------------
  # Allow removal of markers by clicking on an existing marker
  # -------------------------------
  observeEvent(input$map_marker_click, {
    marker <- input$map_marker_click
    # Remove the marker from the reactive data frame
    rv$markers <- rv$markers %>% filter(id != as.numeric(marker$id))
    # Remove the marker from the map display
    leafletProxy("map") %>% removeMarker(layerId = marker$id)
  })
  
  # -------------------------------
  # Button to clear all markers
  # -------------------------------
  observeEvent(input$clear, {
    rv$markers <- data.frame(id = numeric(0), lat = numeric(0), lng = numeric(0))
    leafletProxy("map") %>% clearMarkers()
  })
  
  # -------------------------------
  # Model execution: When the Submit button is clicked, simulate the latent space embedding model
  # -------------------------------
  observeEvent(input$submit, {
    if(nrow(rv$markers) == 0) {
      showModal(modalDialog(
        title = "No Points Selected",
        "Please add at least one point on the map before submitting.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    # For each marker, simulate extraction of environmental values
    env_data <- rv$markers %>%
      rowwise() %>%
      mutate(env = list(simulate_env_values(lat, lng))) %>%
      unnest_wider(env)
    
    # Debug output (can be removed in production)
    print(env_data)
    
    # -------------------------------
    # Build dummy training data for the placeholder linear model
    # -------------------------------
    # Using set.seed() ensures reproducibility during development.
    set.seed(123)
    training_data <- data.frame(
      bathymetry  = runif(50, min = 500, max = 1500),
      chlorophyll = runif(50, min = 0, max = 10),
      sst         = runif(50, min = 15, max = 30)
    )
    # Define similarity as a linear combination of the variables plus noise
    training_data$similarity <- 0.3 * training_data$bathymetry +
      0.4 * training_data$chlorophyll +
      0.3 * training_data$sst + rnorm(50, sd = 10)
    
    # Fit the placeholder linear model using the simulated training data
    placeholder_model <- lm(similarity ~ bathymetry + chlorophyll + sst, data = training_data)
    
    # Predict similarity for each user-added marker based on its simulated environmental values
    predictions <- predict(placeholder_model, newdata = env_data)
    results <- cbind(rv$markers,
                     env_data[, c("bathymetry", "chlorophyll", "sst")],
                     similarity = predictions)
    
    # -------------------------------
    # Display the model predictions in a modal dialog
    # -------------------------------
    showModal(modalDialog(
      title = "Model Predictions",
      renderTable({ results }, striped = TRUE, hover = TRUE),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  # End of server function
}
