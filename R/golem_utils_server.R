#' lectura de datos ----
#'
#' @importFrom tidyr tibble
#' @noRd
#'
# lectura de bases de datos ----
GpoIndigena <- tidyr::tibble(RazaPrimaria = "Desconocido",
                          Chinanteco = 0, Chol = 0, Cuicateco = 0,
                          Huasteco = 0, Huichol = 0, Mam = 0, Maya = 0,
                          Mazateco = 0, Mestizo = 0, Mixteco = 0, Náhuatl = 0,
                          Otomí = 0, `Otomí tepehua` = 0, Pame = 0, Popoluca = 0,
                          Tarahumara = 0, Tlapaneco = 0, Tojolabal = 0, Totonaco = 0,
                          Tzeltal = 0, Tzotzil = 0, Zapoteco = 0, Zoque = 0)

bdMaiz <- read.csv("data/bdMaiz.csv")

GpoIndigena <- data.frame(#RazaPrimaria = "Desconocido",
                          GpoIndigenaAgricultor = c("Chinanteco", "Chol", "Cuicateco",
                                                    "Huasteco", "Huichol", "Mam", "Maya",
                                                    "Mazateco", "Mestizo", "Mixteco",
                                                    "Náhuatl", "Otomí", "Otomí tepehua",
                                                    "Pame", "Popoluca", "Tarahumara",
                                                    "Tlapaneco", "Tojolabal", "Totonaco",
                                                    "Tzeltal", "Tzotzil", "Zapoteco", "Zoque"))

ComplRaci <- c("Chapalote", "Cónico", "Dentados tropicales", "Ocho hileras",
               "Sierra de Chihuahua", "Tropicales precoces", "Tropicales tardíos",
               "No asociadas a un complejo racial")

ComplText <- readLines("./inst/app/www/ComplejoTodo.txt")

# Diseño de gráficos ----
#' propiedades gráficas
#'
#' @noRd
#' @import ggplot2
temaright <- ggplot2::theme(legend.position = "right",
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 14))

#' propiedades gráficas
#'
#' @noRd
#' @import ggplot2
temabottom <- ggplot2::theme(legend.position = "bottom",
                    axis.title.x = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_blank(),
                    legend.title = ggplot2::element_blank(),
                    text = ggplot2::element_text(size = 14))

coloresTransparencia <- c("#FBEC5D","#FFA542","#00BFFF","#800080","#8B0000","#000000",
                          "#FFF380","#FF4500","#000080","#FFA5FF","#A52A2A","#D1D1D1",
                          "#FFE66D","#FFA500","#B2B2FF","#A52AA5","#690000","#404040",
                          "#FFFF00","#FF6900","#00FF00","#FF00FF","#FF8040","#FFFAFA",
                          "#FAFAD2","#FF003D","#B2B280","#D10069","#FFA07A","#666666",
                          "#FFFFD1","#A50042","#FFE6A5","#A02020","#800026","#FFF5D1")

coloresMapa <- c("beige", "purple", "lightred", "darkred", "orange",
                 "cadetblue", "white", "lightgray", "darkgreen", "green",
                 "lightblue", "blue", "darkpurple", "pink", "gray",
                 "black", "lightgreen", "darkblue", "red")
coloresMapa2 <- c("#ffce96", "#d448b6", "#ff9081", "#a13437", "#f59b3f",
                  "#426776", "#fbfbfb", "#a3a3a3", "#70852e", "#6cb336",
                  "#88d8fd", "#38a3d7", "#5c3468", "#ff8be6", "#575757",
                  "#404040", "#b5fe7a", "#0c609f", "#d64331")

# Funciones ----
#' función para mapeo
#'
#' @noRd
#' @import leaflet
#' @import dplyr
#' @importFrom magrittr %>%
#'
## función para mapeo ----
mapea <- function(maizSelecto){
  maizSelecto <- maizSelecto[maizSelecto$ValidacionGeografica != "Inconsistente",]

  col1 <- as.data.frame(cbind(unique(maizSelecto$RazaPrimaria),
                              coloresMapa[1:length(unique(maizSelecto$RazaPrimaria))],
                              coloresMapa2[1:length(unique(maizSelecto$RazaPrimaria))]))
  colnames(col1) <- c("RazaPrimaria", "Color", "Leyenda")

  colorMapa <- left_join(as.data.frame(maizSelecto$RazaPrimaria), col1, by = c("maizSelecto$RazaPrimaria" = "RazaPrimaria"))

  icons <- awesomeIcons(
    icon = "ios-information",
    iconColor = "white",
    library = "ion",
    markerColor = colorMapa[,2]
  )

  maizSelecto %>%
    leaflet() %>%
    addProviderTiles(providers$Stamen.Terrain) %>%
    addAwesomeMarkers(lat = maizSelecto$Latitud, lng = maizSelecto$Longitud,
                      icon = icons,
                      group = "maizSelecto",
                      popup = maizSelecto$RazaPrimaria) %>%
    addLegend(position = "bottomright",
              opacity = 1,
              colors = unique(colorMapa[,3]),
              labels = unique(colorMapa[,1]))
}

#' función para filtrar
#'
#' @noRd
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stringr str_wrap
#'
## función para filtrar ----
selecta <- function(database, complejo = "Todos", raza = "Todos"){
  maizBD <- database %>%
    dplyr::select(Id, ComplejoRacial, RazaPrimaria, Estado,
                  ValidacionGeografica, Latitud, Longitud,
                  NumeroMazorcas, AlturaMazorca, LongitudMazorca, DiametroMazorca, HilerasMazorca,
                  ColorGrano, GranosHilera, GranoAnchura, GranoGrosor, GranoLongitud,
                  GpoIndigenaAgricultor, EdadAgricultor, AFavor, EnContra, PorMejorar,
                  SuperficieSembrada, EpocaSiembra1, EpocaSiembra2, EpocaFloracion,
                  EpocaMadurez, EpocaCosecha1, EpocaCosecha2, SistemaCultivo, CultivosAsociados,
                  Insectos, Malezas, ProblemasAlmacenamiento, ControlMecanico,
                  ControlFungicidaBactericida, ControlInsecticida,
                  Usos, UsosGrano
    )
  maizBD <- maizBD[maizBD$ComplejoRacial %in% complejo,]
  maizBD <- if(any("Todos" %in% raza)){maizBD} else{maizBD[maizBD$RazaPrimaria %in% raza,]}
  maizBD$RazaPrimaria <- stringr::str_wrap(maizBD$RazaPrimaria, width = 12)

  return(maizBD)
}

# maizSelecto <- selecta(bdMaiz, "Sierra de Chihuahua", c("Azul", "Cristalino de Chihuahua", "Gordo"))
# maizSelecto <- selecta(bdMaiz, "Tropicales tardíos", "Motozinteco")
# maizSelecto <- selecta(bdMaiz, "Tropicales tardíos", c("Motozinteco", "Olotillo"))
# maizSelecto <- selecta(bdMaiz, "Dentados tropicales", c("Tepecintle", "Zapalote Grande"))
# maizSelecto <- selecta(bdMaiz, "Cónico", c("Dulce", "Arrocillo Amarillo"))
