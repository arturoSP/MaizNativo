#' lectura de datos ----
#'
#' @noRd
#'
# lectura de base de datos ----
bdMaiz <- read.csv("data/bdMaiz.csv")

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

#
# pr19 <- unique(bdMaiz$RazaPrimaria)[11:29]
# [1] "Olotillo"                "Cónico Norteño"          "Tabloncillo Perla"
# [4] "Cacahuacintle"           "Azul"                    "Palomero Toluqueño"
# [7] "Cristalino de Chihuahua" "Tuxpeño Norteño"         "Tabloncillo"
# [10] "Celaya"                  "Reventador"              "Pepitilla"
# [13] "Elotes Occidentales"     "Dulcillo del Noroeste"   "Arrocillo Amarillo"
# [16] "Dulce"                   "Zamorano Amarillo"       "Mushito"
# [19] "Vandeño"
#
# bdMaiz[bdMaiz$RazaPrimaria == "Olotillo",c(1:3)] # 94
# head(bdMaiz[bdMaiz$RazaPrimaria == "Cónico Norteño",c(1:3)]) # 181
# head(bdMaiz[bdMaiz$RazaPrimaria == "Tabloncillo Perla",c(1:3)]) # 192
# head(bdMaiz[bdMaiz$RazaPrimaria == "Cacahuacintle",c(1:3)]) # 230
# head(bdMaiz[bdMaiz$RazaPrimaria == "Azul",c(1:3)]) # 236
# head(bdMaiz[bdMaiz$RazaPrimaria == "Palomero Toluqueño",c(1:3)]) # 247
# head(bdMaiz[bdMaiz$RazaPrimaria == "Cristalino de Chihuahua",c(1:3)]) # 255
# head(bdMaiz[bdMaiz$RazaPrimaria == "Tuxpeño Norteño",c(1:3)]) # 263
# head(bdMaiz[bdMaiz$RazaPrimaria == "Tabloncillo",c(1:3)]) # 277
# head(bdMaiz[bdMaiz$RazaPrimaria == "Celaya",c(1:3)]) # 285
# head(bdMaiz[bdMaiz$RazaPrimaria == "Reventador",c(1:3)]) # 299
# head(bdMaiz[bdMaiz$RazaPrimaria == "Pepitilla",c(1:3)]) # 319
# head(bdMaiz[bdMaiz$RazaPrimaria == "Elotes Occidentales",c(1:3)]) # 342
# head(bdMaiz[bdMaiz$RazaPrimaria == "Dulcillo del Noroeste",c(1:3)]) # 402
# head(bdMaiz[bdMaiz$RazaPrimaria == "Arrocillo Amarillo",c(1:3)]) # 529
# head(bdMaiz[bdMaiz$RazaPrimaria == "Dulce",c(1:3)]) # 589
# head(bdMaiz[bdMaiz$RazaPrimaria == "Zamorano Amarillo",c(1:3)]) # 613
# head(bdMaiz[bdMaiz$RazaPrimaria == "Mushito",c(1:3)]) # 621
# head(bdMaiz[bdMaiz$RazaPrimaria == "Vandeño",c(1:3)]) # 659
#
# maizSelecto <- bdMaiz[c(94, 181, 192, 230, 236, 249, 255, 263, 277, 285,
#          299, 319, 342, 2413, 524, 584, 608, 616, 654),]
#

#' función para mapeo
#'
#' @noRd
#' @import leaflet
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#'
# función para mapeo ----
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
              colors = unique(colorMapa[,3]), #unique(maizSelecto$RazaPrimaria),
              labels = unique(colorMapa[,1])) #unique(maizSelecto$RazaPrimaria))
}

#' función para filtrar
#'
#' @noRd
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#'
# función para filtrar ----
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

  return(maizBD)
}

ComplRaci <- c("Chapalote", "Cónico", "Dentados tropicales", "Ocho hileras",
               "Sierra de Chihuahua", "Tropicales precoces", "Tropicales tardíos",
               "No asociadas a un complejo racial")

ComplText <- readLines("./inst/app/www/ComplejoTodo.txt")


