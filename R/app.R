library(shiny)
library(dplyr)
library(leaflet)
library(stringr)
library(tm)
library(ggplot2)
library(shinycssloaders)

source("utils.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(column(3,
                  selectInput("complejo1", label = "Complejo racial",
                              choices = c("Chapalote", "Cónico", "Dentados tropicales",
                                          "Tropicales tardíos","Ocho hileras", "Sierra de Chihuahua",
                                          "Tropicales precoces", "No asociadas a un complejo racial"),
                              selected = "Chapalote")),
           column(3,
                  uiOutput("razaInput")),
           column(3,
                  uiOutput("estadoInput")),
           column(3,
                  actionButton("actualizar", label = "DO NOT CLICK"))
  ),
  fluidRow(column(12, withSpinner(leafletOutput("mapaF"), type = 1))),

  fluidRow(tabsetPanel(
    tabPanel("Granos", withSpinner(plotOutput("grafGranos"), type = 1)),
    tabPanel("Mazorcas", withSpinner(plotOutput("grafMazorcas"), type = 1)),
    tabPanel("Agricultores", withSpinner(plotOutput("grafAgricultores"), type = 1)),
    tabPanel("Pros y contras", withSpinner(plotOutput("grafProCon"), type = 1))
    )
    ),


  fluidRow(column(12, dataTableOutput("maices")))
)

# Define server logic required to draw a histogram
server <- function(input, output, session){

  output$razaInput <- renderUI({
    req(input$complejo1)
    selectInput("razaInput", label = "Raza primaria", choices = "Todos",
                selected = "Todos", multiple = TRUE)
  })

  output$estadoInput <- renderUI({
    req(input$complejo1)
    selectInput("estadoInput", label = "Estado", choices = "Todos",
                selected = "Todos", multiple = TRUE)
  })

  complejoRctv <- reactive(input$complejo1)

  listaCompRaza <- reactive({
    listaCR <- bdMaiz %>%
      filter(ComplejoRacial == complejoRctv()) %>%
      select(ComplejoRacial, RazaPrimaria)
    listaCR <- c("Todos", sort(unique(listaCR$RazaPrimaria)))
  })

  listaCompEsta <- reactive({
    listaCE <- bdMaiz %>%
      filter(ComplejoRacial == complejoRctv()) %>%
      select(ComplejoRacial, Estado)
    listaCE <- c("Todos", sort(unique(listaCE$Estado)))
  })

  observeEvent(complejoRctv(), {
    updateSelectInput(session,
                      "razaInput",
                      label = "Raza primaria",
                      choices = listaCompRaza())
    updateSelectInput(session,
                      "estadoInput",
                      label = "Estado",
                      choices = listaCompEsta())
  })

  maizSelecto <- reactive({
      selecta(database = bdMaiz, complejo = input$complejo1,
              raza = input$razaInput, estado = input$estadoInput)
  }
  )

  mapa1 <- reactive(mapea(maizSelecto()))

  output$mapaF <- renderLeaflet(mapa1())

  output$grafGranos <- renderPlot(plotGrano(maizSelecto()))

  output$grafMazorcas <- renderPlot(plotMazorca(maizSelecto()))

  output$grafAgricultores <- renderPlot(plotAgricultor(maizSelecto()))

  output$grafProCon <- renderPlot(plotProCon(maizSelecto()))

  output$maices <- renderDataTable(maizSelecto())
}


# Run the application
shinyApp(ui = ui, server = server)
