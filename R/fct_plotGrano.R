#' plotGrano
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr
#' @import echarts4r
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_sentence
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#'

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
                            labels = c("Grosor", "Longitud", "Anchura", "Granos por\nhilera"),
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

  granosCuali$`amarillo` <- stringr::str_detect(granosCuali$ColorGrano, "amarillo \\(B\\)")
  granosCuali$`amarillo claro` <- stringr::str_detect(granosCuali$ColorGrano, "amarillo claro")
  granosCuali$`amarillo medio` <- stringr::str_detect(granosCuali$ColorGrano, "amarillo medio")
  granosCuali$`amarillo naranja` <- stringr::str_detect(granosCuali$ColorGrano, "amarillo naranja \\(F\\)")
  granosCuali$`azul` <- stringr::str_detect(granosCuali$ColorGrano, "azul \\(K\\)")
  granosCuali$`azul oscuro` <- stringr::str_detect(granosCuali$ColorGrano, "azul oscuro \\(L\\)")
  granosCuali$`blanco` <- stringr::str_detect(granosCuali$ColorGrano, "blanco \\(A\\)")
  granosCuali$`blanco cremoso` <- stringr::str_detect(granosCuali$ColorGrano, "blanco cremoso")
  granosCuali$`blanco puro` <- stringr::str_detect(granosCuali$ColorGrano, "blanco puro \\(H\\)")
  granosCuali$`café` <- stringr::str_detect(granosCuali$ColorGrano, "café")
  granosCuali$`crema` <- stringr::str_detect(granosCuali$ColorGrano, "crema")
  granosCuali$`jaspeado` <- stringr::str_detect(granosCuali$ColorGrano, "jaspeado")
  granosCuali$`morado` <- stringr::str_detect(granosCuali$ColorGrano, "morado")
  granosCuali$`naranja` <- stringr::str_detect(granosCuali$ColorGrano, "naranja")
  granosCuali$`negro` <- stringr::str_detect(granosCuali$ColorGrano, "negro")
  granosCuali$`rojo` <- stringr::str_detect(granosCuali$ColorGrano, "rojo \\(I")
  granosCuali$`rojo naranja` <- stringr::str_detect(granosCuali$ColorGrano, "rojo naranja \\(J")
  granosCuali$`rojo oscuro` <- stringr::str_detect(granosCuali$ColorGrano, "rojo oscuro")
  granosCuali$`rosa` <- stringr::str_detect(granosCuali$ColorGrano, "rosa")

  granosCuali <- granosCuali[,-3]

  granosCuali <- granosCuali %>%
    group_by(RazaPrimaria) %>%
    summarise(across(.cols = c(amarillo:rosa), .fns = sum)) %>%
    tidyr::pivot_longer(cols = c(amarillo:rosa), names_to = "Color", values_to = "Valor") |>
    mutate(Color = stringr::str_to_sentence(Color))
  granosCualiT <- granosCuali |>
    group_by(RazaPrimaria) |>
    summarise(Total = sum(Valor))

  granosCuali <- granosCuali |>
    inner_join(granosCualiT, by = "RazaPrimaria") %>%
    transmute(RazaPrimaria,
              Color,
              Valor = round(Valor / Total, 2))

  p1 <- granosCuali |>
    group_by(RazaPrimaria) |>
    echarts4r::e_chart(Color, timeline = TRUE) |>
    echarts4r::e_color(background = "#fffce2") |>
    echarts4r::e_pie(Valor,
          percentPrecision = 0,
          label = list(show = FALSE,
                       position = "outside",
                       formatter = '{b}: {d}%')) |>
    echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_choro_formatter("percent")) |>
    echarts4r::e_theme_custom('{"color":["#FBEC5D", "#FFF380", "#FFE66D", "#FFA542",
                              "#00BFFF", "#0000FF", "#FFFFFF", "#FAFAD2",
                              "#FFFAFA", "#A52A2A", "#FFF8DC", "#D1D1D1",
                              "#800080", "#FFA500", "#000000", "#FF0000",
                              "#FF4500", "#8B0000", "#FFC0CB"]}') |>
    echarts4r::e_toolbox_feature('saveAsImage') |>
    echarts4r::e_title("Color de los granos")


  p2 <- if(granosCuant[1,1] != 0){
    granosCuant |>
      group_by(RazaPrimaria) |>
      echarts4r::e_chart(Metrica, timeline = TRUE) |>
      echarts4r::e_color(background = "#fffce2") |>
      echarts4r::e_scatter(Valor, colorBy = 'data',
                symbol_size = 25,
                legend = FALSE) |>
      #echarts4r::e_theme_custom('{"color":["#FBEC5D", "#0000FF", "#800080","#FF4500"]}') |>
      echarts4r::e_toolbox_feature('saveAsImage') |>
      echarts4r::e_title("Características del grano")
  } else {
    granosCuant |>
      group_by(RazaPrimaria) |>
      echarts4r::e_chart(Metrica, timeline = TRUE) |>
      echarts4r::e_color(background = "#fffce2") |>
      echarts4r::e_graphic_g(type = 'text', rotation = 0,
                  left = 'center', top = 'center',
                  bounding = 'raw', right = 110,
                  bottom = 110, z = 100,
                  style = list(
                    fill = '#000',
                    text = 'No hay datos',
                    font = 'bold 36px sans-serif'
                  )
                  ) |>
      echarts4r::e_title("Características del grano")
  }

  p2 <- echarts4r::e_flip_coords(p2)

  pf <- list(p1, p2)
  return(pf)

}
