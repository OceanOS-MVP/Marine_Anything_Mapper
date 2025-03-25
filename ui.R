library(shiny)
library(leaflet)
library(DT)

shinyUI(fluidPage(
  titlePanel("Marine Anything Mapper"),
  sidebarLayout(
    sidebarPanel(
      p("Please select locations by clicking on the web map in the main panel on the right"),
      h4("--- Or ---"),
      p("Upload a CSV with location coordinates.
        The CSV should contain two numeric columns, lng and lat."),
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
      leafletOutput("map", height = "60pc")
    )
  )
))
