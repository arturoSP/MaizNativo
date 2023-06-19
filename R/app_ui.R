#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import fullPage
#' @import shinycssloaders
#' @import leaflet
#' @importFrom magrittr %>%
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    pagePiling(
      navigation = TRUE,
      touchSensitivity = 10,
      sections.color = c("#FFF380", "#ff8e7f", "#ffce7f", "#ff7fb0", "#FFF380", "#FFFAC8"),
      opts = options,
      menu = c(
        "Home" = "home",
        "Introducción" = "intro",
        "Complejos raciales" = "description1",
        "Mapa de razas" = "map",
        "Características" = "plots",
        "Más información" = "ending"
      ),
      pageSectionImage(
        center = TRUE,
        img = "./www/MaizPixel.jpeg",
        menu = "home",
        h1("Maíces nativos en México", class = "header shadow-light",
           style = "background-color: #800080; font-family: 'Ubuntu', sans-serif;")
      ),
      pageSection(
        center = FALSE,
        menu = "intro",
        pageContainer(
          h2("Proyecto Global de Maíces", class = "header shadow-light"),
          includeMarkdown("./inst/app/www/Intro.md"),
          #includeHTML("./inst/app/www/Intro.html")
          h4(class = "small footer",
             "CONABIO. 2011.",
             tags$a(href="https://www.biodiversidad.gob.mx/diversidad/proyectoMaices",
                    "Proyecto Global de Maíces Nativos.",
                    target = "_blank"),
             "Comisión Nacional para el Conocimiento y Uso de la Biodiversidad;
                   Instituto Nacional de Investigaciones Forestales, Agrícolas y Pecuarias;
                   Instituto Nacional de Ecología y cambio Climático. México.")
        )
      ),
      pageSection(
        center = FALSE,
        menu = "description1",
        pageRow(
          h2("Selecciona un complejo racial", class = "header dark shadow-dark"),
          fluidRow(
            column(1),
            column(11,
                   selectInput("complejo1", label = "",
                               choices = ComplRaci,
                               selected = sample(ComplRaci, 1)))
          ),
          pageColumn(width = 7,
                     textOutput("complejoText1"),
                     br(),
                     textOutput("complejoText2")),
          pageColumn(width = 5,
                     imageOutput("complejoImag")),
          fluidRow(
            column(12,
                   h3(class = "dark small footer",
                      "CONABIO. 2011.",
                      tags$a(href="https://www.biodiversidad.gob.mx/diversidad/proyectoMaices",
                             "Proyecto Global de Maíces Nativos.",
                             target = "_blank"),
                      "Comisión Nacional para el Conocimiento y Uso de la Biodiversidad;
                   Instituto Nacional de Investigaciones Forestales, Agrícolas y Pecuarias;
                   Instituto Nacional de Ecología y cambio Climático. México.")
                   )
          )
        )
      ),
      pageSection(
        center = FALSE,
        menu = "map",
        pageContainer(
          h2("Selecciona algunas razas", class = "header shadow-light"),
          br(),
          fluidRow(column(4,
                          selectInput("razaInput", label = "Raza primaria",
                                      choices = NULL,
                                      selected = NULL,
                                      multiple = TRUE)),
                   column(4,
                          actionButton("actualizar", label = "Actualizar selección"))
          ),
          withSpinner(leafletOutput("mapaF", height = "600px"), type = 1),
          h4(class = "small footer",
             "CONABIO. 2011.",
             tags$a(href="https://www.biodiversidad.gob.mx/diversidad/proyectoMaices",
                    "Proyecto Global de Maíces Nativos.",
                    target = "_blank"),
             "Comisión Nacional para el Conocimiento y Uso de la Biodiversidad;
                   Instituto Nacional de Investigaciones Forestales, Agrícolas y Pecuarias;
                   Instituto Nacional de Ecología y cambio Climático. México.")
        )
      ),
      pageSection(
        center = FALSE,
        menu = "plots",
        pageContainer(
          h2("Características de tu selección:", class = "header dark shadow-dark"),
          tabsetPanel(
            tabPanel("Granos",
                     withSpinner(plotOutput("grafGranos", height = "600px"),
                                 type = 1)),
            tabPanel("Mazorcas",
                     withSpinner(plotOutput("grafMazorcas", height = "600px"),
                                 type = 1)),
            tabPanel("Agricultores",
                     withSpinner(plotOutput("grafAgricultores", height = "600px"),
                                 type = 1)),
            tabPanel("Pros y contras",
                     withSpinner(plotOutput("grafProCon", height = "600px"),
                                 type = 1)),
            tabPanel("Usos principales",
                     withSpinner(plotOutput("grafUsos", height = "600px"),
                                 type = 1))
          ),
          h4(class = "small footer",
             "CONABIO. 2011.",
             tags$a(href="https://www.biodiversidad.gob.mx/diversidad/proyectoMaices",
                    "Proyecto Global de Maíces Nativos.",
                    target = "_blank"),
             "Comisión Nacional para el Conocimiento y Uso de la Biodiversidad;
                   Instituto Nacional de Investigaciones Forestales, Agrícolas y Pecuarias;
                   Instituto Nacional de Ecología y cambio Climático. México.")
        )
      ),
      pageSection(style = "color: #500050;",
        center = FALSE,
        menu = "ending",
        h2("¿Qué sigue?", class = "header dark shadow-dark"),
        fluidRow(column(12, includeMarkdown("./inst/app/www/Conclusion.md"))),
        #includeMarkdown("./inst/app/www/Conclusion.Rmd"),
        h3(class = "footer big",
           "Este proyecto fue desarrollado como una colaboración institucional entre",
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
           "La investigación y programación fueron realizadas por Dra. Aline
           Romero-Natale y Dr. Arturo Sanchez-Porras"
           )

           #![CONABIO](logo_conabio.png) ![CONAHCYT](logo_conacyt.png) ![ENES Mérida](logo_enes.png) ![UAEH](logo_uaeh.png))
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
