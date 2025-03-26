library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet)
library(terra)
library(viridis)
library(randomForest)
library(DT)

source("utils.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

set.seed(42)

# Input data -------------------------------------------------------------------
# Load predictor variables
rasters <- terra::rast("data/predictor_rasters.tif")
# Generate negative points sample within the raster extent
sample_background <- rasters %>%
  terra::spatSample(3000, method = "regular", na.rm = TRUE, xy = TRUE) %>%
  mutate(Response = 0)

# Reactive values --------------------------------------------------------------

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
