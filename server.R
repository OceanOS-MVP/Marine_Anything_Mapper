library(shiny)
library(leaflet)
library(terra)
library(tidyverse)
library(viridis)

# --- Simulated Environmental Predictor Rasters ---

# Define an extent roughly covering the European continental shelf (e.g., North-East Atlantic/North Sea)
ext <- ext(-10, 10, 40, 65)
ncol <- 100
nrow <- 100

# Simulate bathymetry (e.g., depths from -5000m to 0)
bathymetry <- rast(
  nrows = nrow,
  ncols = ncol,
  ext = ext,
  crs = "EPSG:4326")
values(bathymetry) <- matrix(
  runif(ncol * nrow, min = -5000, max = 0),
  nrow = nrow,
  ncol = ncol)

# Simulate chlorophyll concentration (e.g., 0 to 10 mg/m^3)
chlorophyll <- rast(
  nrows = nrow,
  ncols = ncol,
  ext = ext,
  crs = "EPSG:4326")
values(chlorophyll) <- matrix(
  runif(ncol * nrow, min = 0, max = 10),
  nrow = nrow,
  ncol = ncol)

# Simulate salinity (e.g., 30 to 40 PSU)
salinity <- rast(
  nrows = nrow,
  ncols = ncol,
  ext = ext,
  crs = "EPSG:4326")
values(salinity) <- matrix(
  runif(ncol * nrow, min = 30, max = 40),
  nrow = nrow,
  ncol = ncol)

# Create a raster stack for later prediction
predictorsStack <- c(bathymetry, chlorophyll, salinity)
names(predictorsStack) <- c("Bathymetry", "Chlorophyll", "Salinity")

# --- Reactive Storage for User-Selected Points ---
# Each point will store its Latitude, Longitude and the values for each predictor.
selectedPoints <- reactiveVal(data.frame(
  lng = numeric(),
  lat = numeric(),
  Bathymetry = numeric(),
  Chlorophyll = numeric(),
  Salinity = numeric()
))

shinyServer(function(input, output, session) {
  
  # --- Initial Leaflet Map Setup ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView(lng = 0, lat = 52.5, zoom = 5)
  })
  
  # --- Update Raster Overlay Based on Predictor Selection ---
  observe({
    predictor <- input$predictor
    rasterToShow <- switch(
      predictor,
      "Bathymetry" = bathymetry,
      "Chlorophyll" = chlorophyll,
      "Salinity" = salinity)
    
    # Define a numeric color palette using the viridis scale
    pal <- colorNumeric(
      palette = viridis(256),
      domain = c(
        min(values(rasterToShow), na.rm = TRUE),
        max(values(rasterToShow), na.rm = TRUE)),
      na.color = "transparent")
    
    leafletProxy("map") %>%
      clearImages() %>%
      addRasterImage(
        rasterToShow,
        colors = pal,
        opacity = 0.8,
        project = TRUE) %>%
      clearControls() %>%
      addLegend(
        pal = pal,
        values = c(
          min(values(rasterToShow), na.rm = TRUE),
          max(values(rasterToShow), na.rm = TRUE)),
        title = predictor)
  })
  
  # --- Capture Map Clicks and Record Environmental Data ---
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lng <- click$lng
    
    # Extract predictor values at the clicked location using terra::extract
    b_val   <- terra::extract(bathymetry, cbind(lng, lat))[[1]]
    chl_val <- terra::extract(chlorophyll, cbind(lng, lat))[[1]]
    sal_val <- terra::extract(salinity, cbind(lng, lat))[[1]]
    
    # Append new point to the reactive data.frame
    df <- selectedPoints()
    newRow <- data.frame(
      lng = lng,
      lat = lat,
      Bathymetry = b_val,
      Chlorophyll = chl_val,
      Salinity = sal_val)
    selectedPoints(bind_rows(df, newRow))
  })
  
  # --- Display Selected Points in a Data Table ---
  output$locationTable <- DT::renderDataTable({
    selectedPoints() %>%
      DT::datatable() %>%
      DT::formatRound(columns = c(1:5), digits = 2)
  })
  
  # --- Run Lookalike GLM Model Upon Submission ---
  observeEvent(input$submit, {
    pos <- selectedPoints()
    if(nrow(pos) == 0){
      showNotification("No points selected", type = "error")
      return()
    }
    pos$Response <- 1  # Label positive observations
    
    # Generate 100 random negative points within the raster extent
    random_coords <- data.frame(
      Longitude = runif(100, xmin(bathymetry), xmax(bathymetry)),
      Latitude  = runif(100, ymin(bathymetry), ymax(bathymetry))
    )
    random_vals <- terra::extract(predictorsStack, random_coords)
    random_data <- cbind(random_coords, random_vals[,-1])
    random_data$Response <- 0  # Label negatives
    
    # Combine positive and negative observations
    model_data <- bind_rows(pos, random_data)
    
    # Fit a binary classifier using glm
    model <- glm(Response ~ Bathymetry + Chlorophyll + Salinity,
                 data = model_data, family = binomial)
    
    # Predict across the entire raster stack using the fitted model
    pred_raster <- terra::predict(predictorsStack, model, type = "response")
    
    # Define a palette for the prediction layer using the magma scheme
    pal_pred <- colorNumeric(
      palette = viridis::magma(256),
      domain = c(min(values(pred_raster), na.rm = TRUE),
                 max(values(pred_raster), na.rm = TRUE)),
      na.color = "transparent")
    
    leafletProxy("map") %>%
      clearImages() %>%
      addRasterImage(
        pred_raster,
        colors = pal_pred,
        opacity = 0.8,
        project = TRUE) %>%
      clearControls() %>%
      addLegend(pal = pal_pred,
                values = c(min(values(pred_raster), na.rm = TRUE),
                           max(values(pred_raster), na.rm = TRUE)),
                title = "Prediction")
  })
})
