# TODO:
# - set maximum number of points
# - add extent bounding box



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
