ui <- shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Marine Anything Mapper"),
  sidebarLayout(
    sidebarPanel(
      strong("Please click the map on the right to select locations"),
      br(),
      br(),
      h4("--- Or ---"),
      br(),
      fileInput(
        "upload_csv",
        "Upload a CSV with \"x\" and \"y\" columns",
        accept = c(".csv")),
      br(),
      br(),
      br(),
      h4("Your selected locations:"),
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
