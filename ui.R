
ui <- shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Marine Anything Mapper"),
  sidebarLayout(
    sidebarPanel(
      p("Please click the map on the right to select locations"),
      br(),
      br(),
      h4("--- Or ---"),
      br(),
      br(),
      fileInput(
        "upload_csv",
        "Upload a CSV with lng and lat columns.",
        accept = c(".csv")),
      br(),
      br(),
      h4("Selected Locations:"),
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
