render_map <- function() {
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
}


fit_model <- function(model_data) {
  set.seed(42)
  model <- model_data %>%
    mutate(Response = as.factor(Response)) %>%
    randomForest::randomForest(
      formula = Response ~ .,
      data = .,
      ntree = 500,
      mtry = 3,
      importance = TRUE)
  return(model)
}


make_prediction <- function(model, raster) {
  # Predict across the entire raster stack using the fitted model
  pred_raster <- terra::predict(rasters, model, type = "prob")
  
  log_transform <- TRUE
  # log scale prediction if enabled
  if (log_transform == TRUE) {
    pred_raster <- (log10(pred_raster[[2]] + 0.00001) + 5) / 5
  } else {
    pred_raster <- pred_raster[[2]]
  }
  return(pred_raster)
}


plot_prediction <- function(prediction_raster) {
  # Define a numeric color palette using the viridis scale
  pal <- colorNumeric(
    palette = viridis::viridis(256),
    domain = c(0, 1),
    na.color = "black")
  
  leafletProxy("map") %>%
    addRasterImage(
      prediction_raster,
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
}