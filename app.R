#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          leafletOutput("map", width = "100%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  coord <- reactive({
    F_map(Samp(), colSite(), colYear(), DefinLevel())
  })

  mapa1 <- reactive({
    tryCatch({
      F_MapParam(coord())
    },
    error = function(e){
      message("Ocurrió un error.")
    })
  })

  # m <- leaflet() |>
  #   addTiles() |>
  #   addMarkers(lng = 174.768, lat = -36.852, popup = "The birthplace of R")
  # m



  output$map <- renderLeaflet(mapa1())

  userMap <- reactive({
    mapa1() |>
      setView(lng = input$map_center$lng,
              lat = input$map_center$lat,
              zoom = input$map_zoom)
  })

  # install phantomJs in case it is missing

  phantomjs_path <- reactive({
    req(input$WorkFile)
    phantomjs_path2 <- webshot::is_phantomjs_installed()
    if (!isTRUE(phantomjs_path2)) {
      webshot::install_phantomjs()
      FlgJS <- F
    } else {
      FlgsJS <- T
    }
    phantomjs_path2
  }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
