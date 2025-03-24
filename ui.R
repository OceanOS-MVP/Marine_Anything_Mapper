# ui.R
# Marine Anything Mapper (MAM)
# This file defines the user interface of the app.
# It provides an interactive map (using Leaflet) that allows users to:
#  - Add up to 10 points by clicking on the map.
#  - Remove points by clicking on the markers.
#  - Trigger the latent space embedding simulation by clicking "Submit".
#  - Clear all points with a dedicated button.
#
# Advanced prompt engineering concepts (chain-of-thought, meta-prompting) have been
# applied in the design and commenting for clarity.

library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)

# Define UI for Marine Anything Mapper
ui <- fluidPage(
  # Application title
  titlePanel("Marine Anything Mapper (MAM)"),
  
  # Sidebar layout with controls and main panel for the map
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),
      p("Click on the map to add points (maximum 10)."),
      p("Click on an existing marker to remove it."),
      # Submit button to run the placeholder linear model simulation
      actionButton("submit", "Submit"),
      # Clear button to remove all added points
      actionButton("clear", "Clear All Points")
    ),
    mainPanel(
      # Leaflet map output for interactive point selection
      leafletOutput("map", height = "600px")
    )
  )
)
