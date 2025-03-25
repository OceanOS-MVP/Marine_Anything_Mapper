library(shiny)
library(leaflet)
library(DT)

shinyUI(fluidPage(
  titlePanel("Marine Anything Mapper"),
  sidebarLayout(
    sidebarPanel(
      p("Please selections on the web map in the main panel on the right"),
      h4("--- Or ---"),
      p("Upload a CSV with location coordinates.
        The CSV should contain two columns, `lng` and `lat`."),
      # checkboxGroupInput("predictors", "Select Environmental Predictor Layers:",
      #                    choices = c("Bathymetry", "Chlorophyll", "Salinity"),
      #                    selected = "Bathymetry"),
      # checkboxInput("show_prediction", "Show Model Predictions", value = FALSE),
      br(),
      br(),
      fileInput("upload_csv", "Upload CSV File", accept = c(".csv")),
      br(),
      h4("Selected Locations"),
      DT::dataTableOutput("locationTable"),
      br(),
      br(),
      h4("When you are done, press \"Submit\""),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
))
