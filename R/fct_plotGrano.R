#' plotGrano
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr
#' @import ggplot2
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @importFrom ggpubr ggarrange
#' @importFrom magrittr %>%
#'
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
  granosCuant <- if(dim(granosCuant)[1] > 0){
    granosCuant
  } else {
    data.frame(Id = 0,
               RazaPrimaria = unique(maizSelecto$RazaPrimaria)[1],
               Metrica = c("Grosor", "Longitud",
                           "Anchura", "Hilera"),
               Valor = 0)
  }


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
    facet_wrap(~RazaPrimaria, nrow = 1, scales = "free_x")+
    scale_fill_manual(values = coloresPaleta)+
    theme_classic()+
    temabottom+
    ggtitle("Color del grano")+
    ylab("Composición")

  p2 <- if(granosCuant[1,1] != 0){
    granosCuant %>%
      ggplot(aes(x = Metrica, y = Valor, fill = RazaPrimaria))+
      geom_violin(color = "gray80")+
      facet_wrap(facets = ~Metrica, scales = "free", nrow = 1)+
      ylab("[cm]")+
      theme_classic()+
      temabottom+
      scale_fill_manual(values = coloresTransparencia)+
      ggtitle("Características del grano")
  } else {
    granosCuant %>%
      ggplot(aes(x = Metrica, y = Valor, fill = RazaPrimaria))+
      geom_text(aes(label = "No hay datos"))+
      facet_wrap(facets = ~Metrica, scales = "free", nrow = 1)+
      theme_classic()+
      temabottom+
      theme(axis.text = element_blank(),
            axis.title = element_blank())+
      scale_fill_manual(values = coloresTransparencia)+
      ggtitle("Características del grano")
  }



  pf <- ggpubr::ggarrange(p1, p2)
  return(pf)
}
