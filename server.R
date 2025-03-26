
server <- shinyServer(function(input, output, session) {
  
  # Initial Leaflet Map Setup
  output$map <- renderLeaflet({
    render_map()
  })
  
  # Capture Map Clicks: Record Environmental Data and Add Marker ---------------
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
  
  # File Upload Functionality --------------------------------------------------

  observeEvent(input$upload_csv, {
    file <- input$upload_csv
    if (is.null(file)) return()
    
    data <- read.csv(file$datapath)
    # Accept CSVs with columns "lng" and "lat" or "Longitude" and "Latitude"
    if (all(c("x", "y") %in% names(data))){
      data$lng <- as.numeric(data$lng)
      data$lat <- as.numeric(data$lat)
    # } else if (all(c("Longitude", "Latitude") %in% names(data))){
    #   data$lng <- as.numeric(data$Longitude)
    #   data$lat <- as.numeric(data$Latitude)
    # } else if (all(c("lng", "lat") %in% names(data))){
    #   data$lng <- as.numeric(data$Longitude)
    #   data$lat <- as.numeric(data$Latitude)
    } else {
      showNotification("CSV file must contain x and y columns", type = "error")
      return()
    }
    
    # Extract environmental predictor values for each location
    pred_vals <- terra::extract(rasters, cbind(data["x"], data["y"]))

    # Append new locations to the reactive data.frame
    selectedPoints(bind_rows(
      selectedPoints(),
      data[, c("x", "y")]))
    
    # Add markers for the uploaded locations
    leafletProxy("map") %>%
      addCircleMarkers(
        lng = data["x"], lat = data["y"],
        radius = 5,
        color = "black",
        fill = TRUE,
        fillOpacity = 1)
  })
  
  # Run Lookalike Model Upon Submission and Store Prediction Raster ------------
  observeEvent(input$submit, {
    sample_selected <- selectedPoints()
    # Warnings and errors for NA values
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
    
    model <- fit_model(model_data)
    
    prediction_raster <- make_prediction(model, raster)
    predictionRaster(prediction_raster)
    plot_prediction(prediction_raster)
  })
  
  # --- Display Selected Points in a Data Table ---
  output$locationTable <- DT::renderDataTable({
    selectedPoints() %>%
      DT::datatable() %>%
      DT::formatRound(columns = c(1:5), digits = 2)
  })
})
