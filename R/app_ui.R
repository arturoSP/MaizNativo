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
      sections.color = c("#FFF380", "#800080", "#FFF380", "#800080", "#FFF380", "#800080"),
      opts = options,
      menu = c(
        "Home" = "home",
        "Introducción" = "intro",
        "Mapa" = "map",
        "Razas" = "plots",
        "Complejo Racial" = "description",
        "Acerca de" = "about"
      ),
      pageSectionImage(
        center = TRUE,
        img = "www/img/MaizPixel.jpeg",
        menu = "home",
        h1("Maíces nativos en México", class = "header highlight shadow-light")
      ),
      pageSection(
        center = TRUE,
        menu = "intro",
        pageContainer(
          h2("Proyecto Global de Maíces", class = "header shadow-dark"),
          #includeMarkdown("./inst/app/www/Intro.Rmd")
          includeHTML("./inst/app/www/Intro.html")
          #fluidRow(column(12, includeHTML("./www/BCG_Coral_Reef_App.html")))),
        )
      ),
      pageSection(
        center = TRUE,
        menu = "map",
        pageContainer(
          h2("Selecciona un complejo y sus razas", class = "header dark shadow-light"),
          br(),
          fluidRow(column(4,
                          selectInput("complejo1", label = "Complejo racial",
                                      choices = ComplRaci,
                                      selected = sample(ComplRaci, 1))),
                   column(4,
                          selectInput("razaInput", label = "Raza primaria",
                                      choices = NULL,
                                      selected = NULL,
                                      multiple = TRUE)),
                   column(4,
                          actionButton("actualizar", label = "Actualizar selección"))
          ),
          withSpinner(leafletOutput("mapaF", height = "600px"), type = 1)
        )
      ),
      pageSection(
        center = TRUE,
        menu = "plots",
        pageContainer(
          h2("Detalles con respecto a la data", class = "header shadow-dark"),
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
          )
        )
      ),
      pageSection(
        center = TRUE,
        menu = "description",
        pageContainer(
          h2("Complejo racial", class = "header dark shadow-light"),
          includeMarkdown("./inst/app/www/ComplejoConico2.Rmd")
          #includeHTML("./inst/app/www/ComplejoConico2.html")
          #fluidRow(column(12, includeHTML("./www/BCG_Coral_Reef_App.html")))),
        )
      ),
      pageSection(
        center = TRUE,
        menu = "about",
        h2("Acerca de", class = "header shadow-dark"),
        h3("Proyecto desarrollado por Arturo Sanchez-Porras & Aline Romero-Natale.",
           class = "shadow-light"),
        h4("Una liga aquí para github", class = "shadow-light"),
        h4("Una liga aquí para CONABIO", class = "shadow-light"),
        h4("Una liga aquí para CONACYT", class = "shadow-light")
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
