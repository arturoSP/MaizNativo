# lectura de base de datos ----
bdMaiz <- read.csv("bdMaiz.csv")

# función para mapeo ----
getColor <- function(Selecto){
  nSel <- length(unique(Selecto))
  cbind(unique(Selecto), coloresTransparencia[1:nSel])
}

mapea <- function(maizSelecto){
  icons <- awesomeIcons(
    icon = "ios-information",
    iconColor = "black",
    library = "ion",
    markerColor = getColor(maizSelecto$RazaPrimaria)
  )

  maizSelecto <- maizSelecto[maizSelecto$ValidacionGeografica != "Inconsistente",]
  maizSelecto %>%
    leaflet() %>%
    addProviderTiles(providers$Stamen.Terrain) %>%
    addAwesomeMarkers(lat = maizSelecto$Latitud, lng = maizSelecto$Longitud,
                      #icon = icons,
                      color = maizSelecto$RazaPrimaria,
                      group = "maizSelecto",
                      popup = maizSelecto$RazaPrimaria) %>%
    addLegend(position = "bottomright",
              colors = unique(maizSelecto$RazaPrimaria),
              labels = unique(maizSelecto$RazaPrimaria))
}

# función para filtrar ----
selecta <- function(database, complejo = "Todos", raza = "Todos", estado = "Todos"){
  maizBD <- database %>%
  #maizBD <- bdMaiz %>%
    dplyr::select(Id, ComplejoRacial, RazaPrimaria, Estado,
                  ValidacionGeografica, Latitud, Longitud,
                  NumeroMazorcas, AlturaMazorca, LongitudMazorca, DiametroMazorca, HilerasMazorca,
                  ColorGrano, GranosHilera, GranoAnchura, GranoGrosor, GranoLongitud,
                  GpoIndigenaAgricultor, EdadAgricultor, AFavor, EnContra, PorMejorar,
                  SuperficieSembrada, EpocaSiembra1, EpocaSiembra2, EpocaFloracion,
                    EpocaMadurez, EpocaCosecha1, EpocaCosecha2, SistemaCultivo, CultivosAsociados,
                  Insectos, Malezas, ProblemasAlmacenamiento, ControlMecanico,
                    ControlFungicidaBactericida, ControlInsecticida
                  )
  maizBD <- if(complejo != "Todos"){maizBD[maizBD$ComplejoRacial == complejo,]} else{maizBD}
  maizBD <- if(raza != "Todos"){maizBD[maizBD$RazaPrimaria == raza,]} else{maizBD}
  maizBD <- if(estado != "Todos"){maizBD[maizBD$Estado == estado,]} else{maizBD}

  return(maizBD)
}

# funciones para generar gráficos ----
## data de mazorcas ----
plotMazorca <- function(maizSelecto){
  mazorcas <- maizSelecto %>%
    select(Id, RazaPrimaria, NumeroMazorcas:HilerasMazorca) %>%
    tidyr::pivot_longer(cols = c(NumeroMazorcas:HilerasMazorca),
                        names_to = "Metrica", values_to = "Valor") %>%
    filter(!is.na(Valor)) %>%
    mutate(Metrica = factor(Metrica,
                            levels = c("AlturaMazorca", "LongitudMazorca",
                                       "DiametroMazorca", "HilerasMazorca",
                                       "NumeroMazorcas"),
                            labels = c("Altura de la mazorca",
                                       "Longitud",
                                       "Diámetro",
                                       "Hileras por mazorca",
                                       "Número de mazorcas"),
                            ordered = T))
  p1 <- mazorcas %>%
    ggplot(aes(x = Metrica, y = Valor, color = RazaPrimaria))+
    geom_violin()+
    #geom_jitter(aes(color = RazaPrimaria))+
    facet_wrap(facets = ~Metrica, scales = "free", nrow = 1)+
    ylab("[cm]")+
    theme_classic()+
    theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    scale_color_manual(values = coloresTransparencia)+
    ggtitle("Características de la mazorca")

  return(p1)
}

## data de granos ----
plotGrano <- function(maizSelecto){
  granosCuant <- maizSelecto %>%
    select(Id, RazaPrimaria, GranosHilera:GranoLongitud) %>%
    tidyr::pivot_longer(cols = c(GranosHilera:GranoLongitud),
                        names_to = "Metrica", values_to = "Valor") %>%
    filter(!is.na(Valor)) %>%
    mutate(Metrica = factor(Metrica,
                            levels = c("GranoGrosor",
                                       "GranoLongitud", "GranoAnchura",
                                       "GranosHilera"),
                            labels = c("Grosor", "Longitud", "Anchura", "Granos por hilera"),
                            ordered = T))

  granosCuali <- maizSelecto %>%
    select(Id, RazaPrimaria, ColorGrano) %>%
    filter(ColorGrano != "ND")

  granosCuali$`amarillo` <- str_detect(granosCuali$ColorGrano, "amarillo \\(B\\)")
  granosCuali$`amarillo claro` <- str_detect(granosCuali$ColorGrano, "amarillo claro")
  granosCuali$`amarillo medio` <- str_detect(granosCuali$ColorGrano, "amarillo medio")
  granosCuali$`amarillo naranja` <- str_detect(granosCuali$ColorGrano, "amarillo naranja \\(F\\)")
  granosCuali$`azul` <- str_detect(granosCuali$ColorGrano, "azul \\(K\\)")
  granosCuali$`azul oscuro` <- str_detect(granosCuali$ColorGrano, "azul oscuro \\(L\\)")
  granosCuali$`blanco` <- str_detect(granosCuali$ColorGrano, "blanco \\(A\\)")
  granosCuali$`blanco cremoso` <- str_detect(granosCuali$ColorGrano, "blanco cremoso")
  granosCuali$`blanco puro` <- str_detect(granosCuali$ColorGrano, "blanco puro \\(H\\)")
  granosCuali$`café` <- str_detect(granosCuali$ColorGrano, "café")
  granosCuali$`crema` <- str_detect(granosCuali$ColorGrano, "crema")
  granosCuali$`jaspeado` <- str_detect(granosCuali$ColorGrano, "jaspeado")
  granosCuali$`morado` <- str_detect(granosCuali$ColorGrano, "morado")
  granosCuali$`naranja` <- str_detect(granosCuali$ColorGrano, "naranja")
  granosCuali$`negro` <- str_detect(granosCuali$ColorGrano, "negro")
  granosCuali$`rojo` <- str_detect(granosCuali$ColorGrano, "rojo \\(I")
  granosCuali$`rojo naranja` <- str_detect(granosCuali$ColorGrano, "rojo naranja \\(J")
  granosCuali$`rojo oscuro` <- str_detect(granosCuali$ColorGrano, "rojo oscuro")
  granosCuali$`rosa` <- str_detect(granosCuali$ColorGrano, "rosa")

  granosCuali <- granosCuali[,-3]

  granosColor <- granosCuali %>%
    group_by(RazaPrimaria) %>%
    summarise(across(.cols = c(amarillo:rosa), .fns = sum)) %>%
    tidyr::pivot_longer(cols = c(amarillo:rosa), names_to = "Color", values_to = "Valor") %>%
    group_by(RazaPrimaria) %>%
    reframe(Total = sum(Valor))

  coloresPaleta <- c("#FBEC5D", "#FFF380", "#FFE66D", "#FFA542", "#00BFFF",
                     "#0000FF", "#FFFFFF", "#FAFAD2", "#FFFAFA", "#A52A2A",
                     "#FFF8DC", "#D1D1D1", "#800080", "#FFA500", "#000000",
                     "#FF0000", "#FF4500", "#8B0000", "#FFC0CB")

  p1 <- granosCuali %>%
    group_by(RazaPrimaria) %>%
    summarise(across(.cols = c(amarillo:rosa), .fns = sum)) %>%
    tidyr::pivot_longer(cols = c(amarillo:rosa), names_to = "Color", values_to = "Valor") %>%
    left_join(granosColor) %>%
    mutate(Porcentaje = Valor / Total) %>%
    ggplot(aes(x = RazaPrimaria, y = Porcentaje, fill = Color))+
    geom_col(color = "gray80", alpha = 0.7)+
    scale_fill_manual(values = coloresPaleta)+
    theme_classic()+
    theme(legend.position = "bottom", axis.title.x = element_blank(),
          legend.title = element_blank())+
    ggtitle("Color del grano")+
    ylab("Composición")

  p2 <- granosCuant %>%
    ggplot(aes(x = Metrica, y = Valor, color = RazaPrimaria))+
    geom_violin()+
    #geom_boxplot()+
    facet_wrap(facets = ~Metrica, scales = "free", nrow = 1)+
    ylab("[cm]")+
    theme_classic()+
    theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    scale_color_manual(values = coloresTransparencia)+
    ggtitle("Características del grano")

  pf <- ggpubr::ggarrange(p1, p2)
  return(pf)
}

# data de agricultores ----
plotAgricultor <- function(maizSelecto){
  agric <- maizSelecto %>%
    select(Id, RazaPrimaria, GpoIndigenaAgricultor:PorMejorar)

  p1 <- agric %>%
    filter(!is.na(EdadAgricultor)) %>%
    ggplot(aes(x = RazaPrimaria, y = EdadAgricultor, color = RazaPrimaria))+
    geom_violin()+
    facet_wrap(facets = ~RazaPrimaria, scales = "free_x", nrow = 1)+
    ylab("Años")+
    theme_classic()+
    theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    scale_color_manual(values = coloresTransparencia)+
    ggtitle("Edad de los agricultores")

  p2 <- agric %>%
    filter(GpoIndigenaAgricultor != "" & GpoIndigenaAgricultor != "ND") %>%
    group_by(RazaPrimaria, GpoIndigenaAgricultor) %>%
    summarise(Cuenta = n()) %>%
    ggplot(aes(x = GpoIndigenaAgricultor, y = Cuenta, color = GpoIndigenaAgricultor))+
    geom_boxplot()+
    ylab("Productores")+
    facet_wrap(facets = ~RazaPrimaria, scales = "free_x", nrow = 1)+
    theme_classic()+
    theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    scale_color_manual(values = coloresTransparencia)+
    ggtitle("Pertenencia a grupos indígenas")

  pf <- ggpubr::ggarrange(p1, p2, nrow = 1)
  return(pf)
}

# data pros y contras ----
plotProCon <- function(maizSelecto){
  agric <- maizSelecto %>%
    select(Id, RazaPrimaria, GpoIndigenaAgricultor:PorMejorar)

  AFavor <- agric %>%
    filter(AFavor != "") %>%
    select(AFavor) %>%
    as.vector()

  CorpAFavor <- Corpus(VectorSource(AFavor))
  CorpAFavor[["1"]][["content"]]
  CorpAFavor <- tm_map(CorpAFavor, removePunctuation)
  CorpAFavor[["1"]][["content"]]
  CorpAFavor <- tm_map(CorpAFavor, tolower)
  CorpAFavor <- tm_map(CorpAFavor, removeWords, c("y", "que", "del", "el", "La",
                                                  "la", "en", "de", "por", "muy",
                                                  "para", "casi", "tiene", "esta",
                                                  "plagas", "gorgojo", "los",
                                                  "todo", "poco", "mucho", "las",
                                                  "no", "porque", "son", "cbuen"))
  CorpAFavor[["1"]][["content"]]
  MatrAFavor <- termFreq(CorpAFavor[["1"]][["content"]]) %>% as.data.frame()
  colnames(MatrAFavor) <- c("cuenta")
  p1 <- ggplot(MatrAFavor,
               aes(label = rownames(MatrAFavor), size = as.numeric(cuenta), color = as.numeric(cuenta)))+
    ggwordcloud::geom_text_wordcloud()+
    scale_size_area(max_size = 10)+
    scale_color_gradient2(low = "darkred", mid = "#FFDB48", high = "purple")+
    theme_classic()+
    ggtitle("A los productores les gusta:")

  EnContra <- agric %>%
    filter(EnContra != "") %>%
    select(EnContra) %>%
    as.vector()

  CorpEnContra <- Corpus(VectorSource(EnContra))
  CorpEnContra[["1"]][["content"]]
  CorpEnContra <- tm_map(CorpEnContra, removePunctuation)
  CorpEnContra[["1"]][["content"]]
  CorpEnContra <- tm_map(CorpEnContra, tolower)
  CorpEnContra <- tm_map(CorpEnContra, removeWords, c("y", "que", "del", "el", "Lo",
                                                      "la", "en", "de", "sin", "lo",
                                                      "nada", "bien", "ninguna",
                                                      "comentarios", "nada", "no", "negativo",
                                                      "todo", "hay", "con", "por",
                                                      "para", "casi", "tiene",
                                                      "plagas", "gorgojo", "los",
                                                      "poco", "mucho",
                                                      "las", "no", "para", "porque", "son",
                                                      "está", "muy", "más",
                                                      "algo", "veces", "a", "nd",
                                                      "le", "se", "gusta", "\\n",
                                                      "fácil", "cuando", "muchas", "muchos",
                                                      "cque"
  ))
  CorpEnContra[["1"]][["content"]]
  MatrEnContra <- termFreq(CorpEnContra[["1"]][["content"]]) %>% as.data.frame()
  colnames(MatrEnContra) <- c("cuenta")
  p2 <- ggplot(MatrEnContra,
               aes(label = rownames(MatrEnContra), size = as.numeric(cuenta), color = as.numeric(cuenta)))+
    ggwordcloud::geom_text_wordcloud()+
    scale_size_area(max_size = 10)+
    scale_color_gradient2(low = "darkred", mid = "#FFDB48", high = "purple")+
    theme_classic()+
    ggtitle("A los productores no les gusta:")

  pf <- ggpubr::ggarrange(p1, p2, nrow = 1)
  return(pf)
}

 coloresTransparencia <- c("#FBEC5D", "#FFA542", "#00BFFF", "#800080",
                           "#FF0000", "#FF4500", "#8B0000", "#000000",
                           "#FFC0CB", "#D1D1D1", "#A52A2A", "#FFF380",
                           "#0000FF", "#FFE66D", "#FFA500", "#FFFF00",
                           "#FAFAD2", "#00FF00", "#FF00FF", "#FFFAFA")
