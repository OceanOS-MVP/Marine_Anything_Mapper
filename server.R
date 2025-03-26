library(shiny)
library(leaflet)
library(terra)
library(tidyverse)
library(viridis)
library(shinyjs)
library(randomForest)

set.seed(42)

# Load predictor variables
rasters <- terra::rast("data/predictor_rasters.tif")
# Generate negative points sample within the raster extent
sample_background <- rasters %>%
  terra::spatSample(3000, method = "regular", na.rm = TRUE, xy = TRUE) %>%
  mutate(Response = 0)
                  
# --- Reactive Storage for User-Selected Points ---
selectedPoints <- reactiveVal(data.frame(
  x = numeric(),
  y = numeric(),
  bathymetry = numeric(),
  chl = numeric(),
  thetao = numeric(),
  so = numeric(),
  mlotst = numeric(),
  uo = numeric(),
  attn = numeric(),
  diato = numeric(),
  phyc = numeric(),
  no3 = numeric(),
  o2 = numeric(),
  ph = numeric(),
  po4 = numeric(),
  nppv = numeric()
))

# --- Reactive Storage for Model Predictions and Layer Order ---
predRaster <- reactiveVal(NULL)

shinyServer(function(input, output, session) {
  
  # --- Initial Leaflet Map Setup ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        provider = providers$Esri.OceanBasemap,
        group = "Basemap") %>%
      addLayersControl(
        baseGroups = c(
          "Basemap",
          "Prediction"),
        overlayGroups = c("Markers"),
        options = layersControlOptions(collapsed = FALSE)) %>%
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
    
    pred_vals <- terra::extract(rasters, cbind(lng, lat))
    
    df <- selectedPoints()
    newRow <- data.frame(
      x = lng,
      y = lat,
      bathymetry = pred_vals["bathymetry"],
      chl = pred_vals["chl"],
      thetao = pred_vals["thetao"],
      so = pred_vals["so"],
      mlotst = pred_vals["mlotst"],
      uo = pred_vals["uo"],
      attn =  pred_vals["attn"],
      diato = pred_vals["diato"],
      phyc = pred_vals["phyc"],
      no3 = pred_vals["no3"],
      o2 = pred_vals["o2"],
      ph = pred_vals["ph"],
      po4 = pred_vals["po4"],
      nppv = pred_vals["nppv"])
    
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
    pred_vals <- terra::extract(rasters, cbind(data$lng, data$lat))

    # Append new locations to the reactive data.frame
    #TODO: FIX THIS
    selectedPoints(bind_rows(selectedPoints(), data[, c("lng", "lat", "Bathymetry", "Chlorophyll", "Salinity")]))
    
    # Add markers for the uploaded locations
    leafletProxy("map") %>% addCircleMarkers(
      lng = data$lng, lat = data$lat,
      radius = 5,
      color = "black",
      fill = TRUE,
      fillOpacity = 1)
  })
  
  # --- Run Lookalike Model Upon Submission and Store Prediction Raster ---
  observeEvent(input$submit, {
    sample_selected <- selectedPoints()
    if (nrow(sample_selected) == 0) {
      showNotification("No points selected", type = "error")
      return()
    }
    sample_selected_complete <- sample_selected %>% filter(complete.cases(.))
    if (nrow(sample_selected_complete) < nrow(sample_selected)) {
      showNotification("Points with missing values ignored", type = "warn")
    }
    if (nrow(sample_selected_complete) == 0) {
      showNotification("No points with complete data", type = "error")
      return()
    }
    sample_selected <- sample_selected_complete
    # Label positive observations
    sample_selected["Response"] <- 1
    
    # Combine positive and negative observations
    model_data <- sample_selected %>%
      bind_rows(sample_background) %>%
      select(-any_of(c("lng", "lat", "Longitude", "Latitude", "x", "y")))

    set.seed(42)
    model <- model_data %>%
      mutate(Response = as.factor(Response)) %>%
      randomForest::randomForest(
      formula = Response ~ .,
      data = .,
      ntree = 500,
      mtry = 3,
      importance = TRUE)
    
    # Predict across the entire raster stack using the fitted model
    pred_raster <- terra::predict(rasters, model, type = "prob")
    
    log_transform <- TRUE
    # log scale prediction if enabled
    if (log_transform == TRUE) {
      pred_raster <- (log10(pred_raster[[2]] + 0.00001) + 5) / 5
    } else {
      pred_raster <- pred_raster[[2]]
    }
    predRaster(pred_raster)
    
    # Define a numeric color palette using the viridis scale
    pal <- colorNumeric(
      palette = viridis::viridis(256),
      domain = c(0, 1),
      na.color = "black")
    
    leafletProxy("map") %>%
      addRasterImage(
        pred_raster,
        colors = pal,
        opacity = 1,
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
