library(shiny)
library(dplyr)
library(leaflet)
library(stringr)
library(tm)
library(ggplot2)
library(shinycssloaders)
library(fullPage)

source("utils.R")
ComplRaci <- c("Chapalote", "Cónico", "Dentados tropicales", "Ocho hileras",
               "Sierra de Chihuahua", "Tropicales precoces", "Tropicales tardíos",
               "No asociadas a un complejo racial")

# Define UI for application that draws a histogram
ui <- pagePiling(
  sections.color = c("#FFF380", "#FFF380", "#800080", "#FFF380", "#800080"),
  opts = options,
  menu = c(
    "Home" = "home",
    "Mapa" = "map",
    "Detalles" = "plots",
    "Info" = "description",
    "Acerca de" = "about"
  ),
  pageSectionImage(
    center = TRUE,
    img = "www/img/cover_rs.jpg",
    menu = "home",
    h1("Hola, este es el título"),
    h3("y mi subtítulo")
  ),
  pageSection(
    center = TRUE,
    menu = "map",
    pageContainer(
      h2("Maíces nativos en México"),
      br(),
      fluidRow(column(3,
                      selectInput("complejo1", label = "Complejo racial",
                                  choices = ComplRaci,
                                  selected = sample(ComplRaci, 1))),
               column(3,
                      uiOutput("razaInput")),
               column(3,
                      uiOutput("estadoInput")),
               column(3,
                      actionButton("actualizar", label = "DO NOT CLICK"))
      ),
      withSpinner(leafletOutput("mapaF"), type = 1)
    )
  ),
  pageSection(
    center = TRUE,
    menu = "plots",
    pageContainer(
      h2("Detalles con respecto a la data"),
      tabsetPanel(
        tabPanel("Granos", withSpinner(plotOutput("grafGranos"), type = 1)),
        tabPanel("Mazorcas", withSpinner(plotOutput("grafMazorcas"), type = 1)),
        tabPanel("Agricultores", withSpinner(plotOutput("grafAgricultores"), type = 1)),
        tabPanel("Pros y contras", withSpinner(plotOutput("grafProCon"), type = 1))
      )
    )
  ),
  pageSection(
    center = TRUE,
    menu = "description",
    pageContainer(
      h2("Para sabe más"),
      dataTableOutput("maices")
    )
  ),
  pageSection(
    center = TRUE,
    menu = "about",
    h2("Acerca de"),
    h3("Proyecto desarrollado por Arturo Sanchez-Porras & Aline Romero-Natale."),
    h4("Una liga aquí para github"),
    h4("Una liga aquí para CONABIO")
  )
)


# ui <- fluidPage(
#   fluidRow(column(3,
#                   selectInput("complejo1", label = "Complejo racial",
#                               choices = ComplRaci,
#                               selected = sample(ComplRaci, 1))),
#            column(3,
#                   uiOutput("razaInput")),
#            column(3,
#                   uiOutput("estadoInput")),
#            column(3,
#                   actionButton("actualizar", label = "DO NOT CLICK"))
#   ),
#   fluidRow(column(12, withSpinner(leafletOutput("mapaF"), type = 1))),
#
#   fluidRow(tabsetPanel(
#     tabPanel("Granos", withSpinner(plotOutput("grafGranos"), type = 1)),
#     tabPanel("Mazorcas", withSpinner(plotOutput("grafMazorcas"), type = 1)),
#     tabPanel("Agricultores", withSpinner(plotOutput("grafAgricultores"), type = 1)),
#     tabPanel("Pros y contras", withSpinner(plotOutput("grafProCon"), type = 1))
#     )
#     ),
#
#
#   fluidRow(column(12, dataTableOutput("maices")))
# )

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
      filter(ComplejoRacial %in% complejoRctv()) %>%
      select(ComplejoRacial, RazaPrimaria)
    listaCR <- c("Todos", sort(unique(listaCR$RazaPrimaria)))
  })

  listaCompEsta <- reactive({
    listaCE <- bdMaiz %>%
      filter(ComplejoRacial %in% complejoRctv()) %>%
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
