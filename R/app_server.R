#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom plotly renderPlotly
#' @noRd

app_server <- function(input, output, session) {

  complejoRctv <- reactive(input$complejo1)

  listaCompRaza <- reactive({
    listaCR <- bdMaiz %>%
      filter(ComplejoRacial %in% complejoRctv()) %>%
      select(ComplejoRacial, RazaPrimaria)
    listaCR <- sort(unique(listaCR$RazaPrimaria))
  })

  observeEvent(complejoRctv(), {
    updateSelectInput(session,
                      "razaInput",
                      label = "Raza primaria",
                      choices = listaCompRaza(),
                      selected = sample(listaCompRaza(),2))
  })

  complejoRac <- eventReactive(input$complejo1, {
    selectComplejo(complejo = input$complejo1)
  })

  maizSelecto <- eventReactive(input$actualizar, {
    selecta(database = bdMaiz,
            complejo = input$complejo1,
            raza = input$razaInput)
  }
  )

  mapa1 <- eventReactive(input$actualizar, {mapea(maizSelecto())})

  output$mapaF <- renderLeaflet(mapa1())

  output$grafGranos <- renderPlot(plotGrano(maizSelecto()))

  output$grafMazorcas <- renderPlot(plotMazorca(maizSelecto()))

  output$grafAgricultores <- renderPlot(plotAgricultor(maizSelecto()))

  output$grafProCon <- renderPlot(plotProCon(maizSelecto()))

  output$grafUsos1 <- renderPlotly(plotUsos(maizSelecto())[[1]])

  output$grafUsos2 <- renderPlotly(plotUsos(maizSelecto())[[2]])

  output$complejoText1 <- renderText(complejoRac()[[1]])

  output$complejoText2 <- renderText(complejoRac()[[2]])

  output$complejoImag <- renderImage(complejoRac()[[3]], deleteFile = FALSE)

}
