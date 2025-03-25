# ui.R
library(shiny)
library(leaflet)
library(DT)

shinyUI(fluidPage(
  titlePanel("Marine Anything Mapper"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("predictor", "Select Environmental Predictor:",
                   choices = c("Bathymetry", "Chlorophyll", "Salinity"),
                   selected = "Bathymetry"),
      actionButton("submit", "Submit"),
      br(), br(),
      h4("Selected Locations"),
      DT::dataTableOutput("locationTable")
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
))
