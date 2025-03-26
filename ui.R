ui <- shinyUI(fluidPage(
  useShinyjs(),
  # Header Banner
  div(
    style = "display: flex; align-items: center; justify-content: space-between; background-color: #FFFFFF; padding: 10px; margin-bottom: 15px;",
    # Left side: OceanOS logo and Title
    div(
      style = "display: flex; align-items: center;",
      tags$img(
        src = "logo_OceanOS.png",
        height = "70px",
        style = "margin-right: 20px;"),
      h1("Marine Anything Mapper",
         style = "margin: 0;")
      ),
    # Right side: Hackathon statement and organizer logos
    div(
      style = "text-align: right;",
      p("Developed as part of the OSL 4.0 hackathon",
        style = "margin: 0; font-size: 16px;"),
      div(
        style = "display: flex; align-items: center; justify-content: flex-end; margin-top: 5px;",
        tags$img(
          src = "logo_EMODnet.png",
          height = "80px",
          style = "margin-right: 10px;"),
        tags$img(
          src = "logo_EDITO.png",
          height = "80px",
          style = "margin-right: 10px;"),
        tags$img(
          src = "logo_EUCommission.png",
          height = "80px")
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      strong("Please click the map on the right to select locations"),
      br(),
      br(),
      strong("--- Or ---"),
      br(),
      br(),
      fileInput(
        "upload_csv",
        "Upload a CSV with lng and lat columns.",
        accept = c(".csv")
      ),
      br(),
      h4("Selected Locations:"),
      DT::dataTableOutput("locationTable"),
      br(),
      h4("When you are done, press \"Submit\""),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      leafletOutput("map", height = "60pc")
    )
  )
))
