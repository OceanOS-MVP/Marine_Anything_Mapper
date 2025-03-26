server <- shinyServer(function(input, output, session) {
  # Reactive values
  
  # for User-Selected Points
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
  
  # for Model Predictions
  predictionRaster <- reactiveVal(NULL)
  
  # Initial Leaflet Map Setup
  output$map <- renderLeaflet({
    render_map()
  })
  
  # Capture Map Clicks: Record Environmental Data and Add Marker ---------------
  observeEvent(input$map_click, {
    
    # Extract values for new point
    click <- input$map_click
    x <- click$lng
    y <- click$lat
    pred_vals <- rasters %>%
      terra::extract(cbind(x, y)) %>%
      bind_cols(x = x, y = y, .)

    # Update reactive value
    selectedPoints() %>%
      bind_rows(pred_vals) %>%
      selectedPoints()
    
    # Add a circle marker at the clicked location
    leafletProxy("map") %>%
      addCircleMarkers(
        lng = x,
        lat = y,
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
    
    data <- read.csv(file["datapath"][[1]])
    # Accept CSVs with columns "lng" and "lat" or "Longitude" and "Latitude"
    if (!all(c("x", "y") %in% names(data))){
      showNotification("CSV file must contain x and y columns", type = "error")
      return()
    }
    
    # Extract environmental predictor values for each location
    pred_vals <- rasters %>%
      terra::extract(data[, c("x", "y")]) %>%
      bind_cols(data[, c("x", "y")], .) %>%
      select(-any_of("ID"))

    # Update reactive value
    selectedPoints() %>%
      bind_rows(pred_vals) %>%
      selectedPoints()
    
    # Add markers for the uploaded locations
    leafletProxy("map") %>%
      addCircleMarkers(
        lng = data[["x"]],
        lat = data[["y"]],
        radius = 5,
        color = "black",
        fill = TRUE,
        fillOpacity = 1)
  })
  
  # Run Lookalike Model Upon Submission and Store Prediction Raster ------------
  observeEvent(input$submit, {
    
    withProgress(
      message = 'Processing...',
      value = 0, {
        
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
        incProgress(0.2)
        
        model <- fit_model(model_data)
        incProgress(0.5)
        
        prediction_raster <- make_prediction(
          model = model,
          raster = raster,
          log_transform = TRUE)
        predictionRaster(prediction_raster)
        plot_prediction(prediction_raster)
        incProgress(0.3)
      })
  })
  
  # --- Display Selected Points in a Data Table ---
  output$locationTable <- DT::renderDataTable({
    selectedPoints() %>%
      select(x, y) %>%
      DT::datatable(options = list(searching = FALSE)) %>%
      DT::formatRound(columns = c(1:2), digits = 2)
  })
})
