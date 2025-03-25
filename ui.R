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
      br(), br(),
      h4("Selected Locations"),
      DT::dataTableOutput("locationTable"),
      br(), br(),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
))
