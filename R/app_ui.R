#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinycssloaders
#' @import bslib
#' @import leaflet
#' @import echarts4r
#' @importFrom magrittr %>%
#'
#' @noRd

app_ui <- function(request) {
  bootstrapPage(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    fluidRow(
      div(
        class = "container-fluid p-5",
        style = "background-color: #FFF380;",
        # imagen y título
        h1("Maíces nativos en México",
           class = "header shadow-light",
           style = " #background-color: #800080;
           font-family: 'Ubuntu', sans-serif;")
      )
    ),
    fluidRow(
      div(
        class = "container p-5",
        style = "background-color: #ff8e7f;",
        # introducción
        h2("Proyecto Global de Maíces",
           class = "header shadow-light"),
        span("El"), strong("Proyecto Global de Maíces (PGM)"),
        span("fue un proyecto de investigación
          realizado por la Comisión Nacional para el Conocimiento y Uso de la
          Biodiversidad (CONABIO) en México. El objetivo del PGM fue actualizar
          la información sobre el maíz y sus parientes silvestres en México, con
          el fin de determinar los centros de origen y diversidad genética del
          maíz. Los resultados del PGM fueron un paso significativo hacia el
          cumplimiento de los requisitos de los artículos 86 a 88 de la LBOGM
          (Ley de Bioseguridad de Organismos Genéticamente Modificados). El PGM
          también permitió la recolección y actualización de información, que
          reveló que México tiene 64 razas de maíz, de las cuales 59 son razas
          nativas."),
        div(class="row",
            div(class="col-md-3",
                img(src="pgm_pixel.png", align = "center")),
            div(class="col-md-9",
                br(),
                p("Con esta aplicación web pretendemos dar a conocer con más
                  detalle algunos de los resultados del PGM. La aplicación
                  permite a los usuarios ver mapas de la diversidad del maíz en
                  México, aprender acerca de los distintos complejos raciales de
                  maíz y acceder a información sobre la diversidad genética del
                  maíz. Además, la aplicación proporciona a los usuarios recursos
                  sobre cómo conservar y promover el uso de las variedades
                  nativas de maíz."))
        )
      )
    ),
    fluidRow(
      div(
        class = "container p-5",
        style = "background-color: #ffce7f;",
        # selección complejo racial
        h2("Selecciona un complejo racial",
           class = "header dark shadow-dark"),
        fluidRow(
          column(8,
                 p(strong("Utiliza la caja de la derecha para escoger un complejo racial y
                     obtener información al respecto. Este complejo seguirá seleccionado en
                     las siguientes secciones de la app.")),
                 class = "dark",
                 style = "font-family: 'Ubuntu', sans-serif;"),
          column(4,
                 selectInput("complejo1", label = "",
                             choices = ComplRaci,
                             selected = sample(ComplRaci, 1)))
        ),
        fluidRow(
          column(8,
                 textOutput("complejoText1"),
                 br(),
                 textOutput("complejoText2")
          ),
          column(4,
                 imageOutput("complejoImag")
          )
        )
      )
    ),
    fluidRow(
      div(
        class = "container p-5",
        style = "background-color: #ff7fb0;",
        # mapa
        h2("Selecciona algunas razas",
           class = "header shadow-light"),
        br(),
        fluidRow(
          column(7,
                 p(strong("Da click en la caja para seleccionar otras razas,
                                   también puedes borrar alguna al seleccionarla y después
                                   borrar con el teclado. A continuación da click en el
                                   botón de \"Actualizar selección\".")),
                 class = "dark"),
          column(5,
                 selectInput("razaInput", label = "",
                             width = "auto",
                             choices = NULL,
                             selected = NULL,
                             multiple = TRUE),
                 actionButton("actualizar", label = "Actualizar selección"))
        ),
        withSpinner(leafletOutput("mapaF", height = "600px"), type = 1)
      )
    ),
    fluidRow(
      div(
        class = "container p-5",
        style = "background-color: #FFF380;",
        # gráficos
        h2("Conoce las variedades que seleccionaste",
           class = "header dark shadow-dark"),
        tabsetPanel(
          tabPanel("Color del grano",
                   withSpinner(echarts4r::echarts4rOutput("grafGranos1"),
                               type = 1)),
          tabPanel("Caracteristicas del grano",
                   withSpinner(echarts4r::echarts4rOutput("grafGranos2"),
                               type = 1)),
          tabPanel("Mazorcas",
                   withSpinner(echarts4r::echarts4rOutput("grafMazorcas"),
                               type = 1)),
          tabPanel("Agricultores",
                   withSpinner(echarts4r::echarts4rOutput("grafAgricultores"),
                               type = 1)),
          tabPanel("Usos del grano",
                   withSpinner(echarts4r::echarts4rOutput("grafUsos1"),
                               type = 1)),
          tabPanel("Usos  de la planta",
                   withSpinner(echarts4r::echarts4rOutput("grafUsos2"),
                               type = 1)),
          tabPanel("Opiniones a favor",
                   withSpinner(echarts4r::echarts4rOutput("grafProCon1"),
                               type = 1)),
          tabPanel("Opiniones en contra",
                   withSpinner(echarts4r::echarts4rOutput("grafProCon2"),
                               type = 1))
        )
      )
    ),
    fluidRow(
      div(
        class = "container p-5",
        style = "background-color: #FFFAC8;",
        # ending
        h2("¿Qué sigue?",
           class = "header dark shadow-dark"),
        fluidRow(column(12, includeMarkdown("./inst/app/www/Conclusion.md"))),
        p("Este proyecto fue desarrollado como una colaboración institucional entre",
          tags$a(href="http://enesmerida.unam.mx",
                 "ENES Mérida",
                 target = "_blank"),
          "y",
          tags$a(href="https://www.uaeh.edu.mx/",
                 "UAEH",
                 target = "_blank"),
          "como parte del programa de estancias postdoctorales de",
          tags$a(href="https://conahcyt.mx/",
                 "CONAHCYT",
                 target="_blank"),
          "para dar difusión a los resultados del Proyecto Global de Maíces
           Nativos liderado por ",
          tags$a(href="https://www.biodiversidad.gob.mx/diversidad/proyectoMaices",
                 "CONABIO",
                 target = "_blank"),
          ".\n",
          "Diseño, investigación y programación realizados por Dra. Aline
           Romero-Natale y Dr. Arturo Sanchez-Porras."
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", system.file('app/www', package = 'MaizNativo')
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MaizNativo"
    ),
    #css = "www/style.css" # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
