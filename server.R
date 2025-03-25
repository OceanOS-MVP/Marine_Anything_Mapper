library(shiny)
library(leaflet)
library(terra)
library(tidyverse)
library(viridis)
library(shinyjs)      # Added shinyjs to allow running JavaScript

# --- Simulated Environmental Predictor Rasters ---

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
selectedPoints <- reactiveVal(data.frame(
  lng = numeric(),
  lat = numeric(),
  Bathymetry = numeric(),
  Chlorophyll = numeric(),
  Salinity = numeric()
))

# --- Reactive Storage for Model Predictions and Layer Order ---
predRaster <- reactiveVal(NULL)

shinyServer(function(input, output, session) {
  
  # --- Initial Leaflet Map Setup ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = providers$Esri.OceanBasemap) %>%
      addLayersControl(
        baseGroups = c(
          "Bathymetry",
          "Chlorophyll",
          "Salinity",
          "Prediction"),
        overlayGroups = c("Markers"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addRasterImage(
        bathymetry,
        colors = viridis::viridis(256),
        opacity = 0.8,
        group = "Bathymetry",
        project = TRUE) %>%
      addRasterImage(
        chlorophyll,
        colors = viridis::viridis(256),
        opacity = 0.8,
        group = "Chlorophyll",
        project = TRUE) %>%
      addRasterImage(
        salinity,
        colors = viridis::viridis(256),
        opacity = 0.8,
        group = "Salinity",
        project = TRUE) %>%
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(
          maxWidth = 100,
          metric = TRUE,
          imperial = TRUE,
          updateWhenIdle = FALSE)) %>%
      setView(lng = 0, lat = 52.5, zoom = 5)
  })
  
  # --- Capture Map Clicks: Record Environmental Data and Add Marker ---
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lng <- click$lng
    
    b_val   <- terra::extract(bathymetry, cbind(lng, lat))[[1]]
    chl_val <- terra::extract(chlorophyll, cbind(lng, lat))[[1]]
    sal_val <- terra::extract(salinity, cbind(lng, lat))[[1]]
    
    df <- selectedPoints()
    newRow <- data.frame(
      lng = lng,
      lat = lat,
      Bathymetry = b_val,
      Chlorophyll = chl_val,
      Salinity = sal_val)
    
    # Update reactive value
    df %>%
      bind_rows(newRow) %>%
      selectedPoints()
    
    # Add a minimalistic circle marker at the clicked location
    leafletProxy("map") %>% addCircleMarkers(
      lng = lng, lat = lat,
      radius = 5,
      color = "black",
      fill = TRUE,
      fillOpacity = 1,
      group = "Markers")
  })
  
  # --- File Upload Functionality ---
  observeEvent(input$upload_csv, {
    file <- input$upload_csv
    if(is.null(file)) return()
    
    data <- read.csv(file$datapath)
    # Accept CSVs with columns "lng" and "lat" or "Longitude" and "Latitude"
    if(all(c("lng", "lat") %in% names(data))){
      data$lng <- as.numeric(data$lng)
      data$lat <- as.numeric(data$lat)
    } else if(all(c("Longitude", "Latitude") %in% names(data))){
      data$lng <- as.numeric(data$Longitude)
      data$lat <- as.numeric(data$Latitude)
    } else {
      showNotification("CSV file must contain lng and lat columns", type = "error")
      return()
    }
    
    # Extract environmental predictor values for each location
    data$Bathymetry <- terra::extract(bathymetry, cbind(data$lng, data$lat))[,2]
    data$Chlorophyll <- terra::extract(chlorophyll, cbind(data$lng, data$lat))[,2]
    data$Salinity <- terra::extract(salinity, cbind(data$lng, data$lat))[,2]
    
    # Append new locations to the reactive data.frame
    selectedPoints(bind_rows(selectedPoints(), data[, c("lng", "lat", "Bathymetry", "Chlorophyll", "Salinity")]))
    
    # Add markers for the uploaded locations
    leafletProxy("map") %>% addCircleMarkers(
      lng = data$lng, lat = data$lat,
      radius = 5,
      color = "black",
      fill = TRUE,
      fillOpacity = 1)
  })
  
  # --- Run Lookalike GLM Model Upon Submission and Store Prediction Raster ---
  observeEvent(input$submit, {
    pos <- selectedPoints()
    if (nrow(pos) == 0) {
      showNotification("No points selected", type = "error")
      return()
    }
    pos_complete <- pos %>% filter(complete.cases(.))
    if (nrow(pos_complete) < nrow(pos)) {
      showNotification("Points with missing values ignored", type = "warn")
    }
    if (nrow(pos_complete) == 0) {
      showNotification("No points with complete data", type = "error")
      return()
    }
    pos <- pos_complete
    # Label positive observations
    pos$Response <- 1
    
    # Generate 1000 random negative points within the raster extent
    random_coords <- data.frame(
      Longitude = runif(1000, xmin(bathymetry), xmax(bathymetry)),
      Latitude  = runif(1000, ymin(bathymetry), ymax(bathymetry))
    )
    random_vals <- terra::extract(predictorsStack, random_coords)
    random_data <- cbind(random_coords, random_vals[,-1])
    random_data$Response <- 0  # Label negatives
    
    # Combine positive and negative observations
    model_data <- bind_rows(pos, random_data)
    
    # Fit a binary classifier using glm
    model <- glm(
      formula = Response ~ Bathymetry + Chlorophyll + Salinity,
      data = model_data,
      family = binomial)
    
    # Predict across the entire raster stack using the fitted model
    pred_raster <- terra::predict(predictorsStack, model, type = "response")
    predRaster(pred_raster)
    
    # Define a numeric color palette using the viridis scale
    pal <- colorNumeric(
      palette = viridis::magma(256),
      domain = c(
        min(values(pred_raster), na.rm = TRUE),
        max(values(pred_raster), na.rm = TRUE)),
      na.color = "transparent")
    
    leafletProxy("map") %>%
      addRasterImage(
        pred_raster,
        colors = pal,
        opacity = 0.8,
        group = "Prediction",
        project = TRUE)
    
    # Programmatically update the layers control to select the "Prediction" group:
    runjs("
      var radios = document.getElementsByClassName('leaflet-control-layers-selector');
      for (var i = 0; i < radios.length; i++) {
        if(radios[i].nextSibling.textContent.trim() === 'Prediction'){
          radios[i].click();
          break;
        }
      }
    ");
  })
  
  # --- Display Selected Points in a Data Table ---
  output$locationTable <- DT::renderDataTable({
    selectedPoints() %>%
      DT::datatable() %>%
      DT::formatRound(columns = c(1:5), digits = 2)
  })
})
