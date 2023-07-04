#' plotUsos
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr
#' @import echarts4r
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_lower
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#'
## data de usos ----
plotUsos <- function(maizSelecto){
  granosU <- maizSelecto %>%
    select(Id, RazaPrimaria, UsosGrano, Usos) %>%
    filter(UsosGrano != "ND") %>%
    filter(Usos != "ND") %>%
    mutate(UsosGrano = stringr::str_to_lower(UsosGrano),
           Usos = stringr::str_to_lower(Usos))

  granosU$Atole <- stringr::str_detect(granosU$UsosGrano, "atole")
  granosU$Corundas <- stringr::str_detect(granosU$UsosGrano, "corundas")
  granosU$Elote <- stringr::str_detect(granosU$UsosGrano, "elote")
  granosU$Esquites <- stringr::str_detect(granosU$UsosGrano, "esquites")
  granosU$Forraje <- stringr::str_detect(granosU$UsosGrano, "forraje")
  granosU$Gorditas <- stringr::str_detect(granosU$UsosGrano, "gorditas")
  granosU$Harina <- stringr::str_detect(granosU$UsosGrano, "harina")
  granosU$Hoja <- stringr::str_detect(granosU$UsosGrano, "hoja")
  granosU$Huacholes <- stringr::str_detect(granosU$UsosGrano, "huacholes")
  granosU$Nixtamal <- stringr::str_detect(granosU$UsosGrano, "nixtamal")
  granosU$Panuchos <- stringr::str_detect(granosU$UsosGrano, "panuchos")
  granosU$Pinole <- stringr::str_detect(granosU$UsosGrano, "pinole")
  granosU$Ponteduro <- stringr::str_detect(granosU$UsosGrano, "ponteduro")
  granosU$Pozol <- stringr::str_detect(granosU$UsosGrano, "pozol")
  granosU$Pozole <- stringr::str_detect(granosU$UsosGrano, "pozole")
  granosU$Salbutes <- stringr::str_detect(granosU$UsosGrano, "salbutes")
  granosU$Semilla <- stringr::str_detect(granosU$UsosGrano, "semilla")
  granosU$Tamal <- stringr::str_detect(granosU$UsosGrano, "tamal")
  granosU$Tesgüino <- stringr::str_detect(granosU$UsosGrano, "tesgüino")
  granosU$Tlacoyos <- stringr::str_detect(granosU$UsosGrano, "tlacoyos")
  granosU$Tomoxtle <- stringr::str_detect(granosU$UsosGrano, "tomoxtle")
  granosU$Tortilla <- stringr::str_detect(granosU$UsosGrano, "tortilla")
  granosU$Tostada <- stringr::str_detect(granosU$UsosGrano, "tostada")
  granosU$Totopo <- stringr::str_detect(granosU$UsosGrano, "totopo")
  granosU$u_Abono <- stringr::str_detect(granosU$Usos, "abono")
  granosU$u_Alimento <- stringr::str_detect(granosU$Usos, "alimento")
  granosU$u_Combustible <- stringr::str_detect(granosU$Usos, "combustible")
  granosU$u_Elote <- stringr::str_detect(granosU$Usos, "elote")
  granosU$u_Forraje <- stringr::str_detect(granosU$Usos, "forraje")
  granosU$u_Grano <- stringr::str_detect(granosU$Usos, "grano")
  granosU$u_Hoja <- stringr::str_detect(granosU$Usos, "hoja")
  granosU$u_Nixtamal <- stringr::str_detect(granosU$Usos, "nixtamal")
  granosU$u_Otro <- stringr::str_detect(granosU$Usos, "otro")
  granosU$u_Pozole <- stringr::str_detect(granosU$Usos, "pozole")
  granosU$u_Semilla <- stringr::str_detect(granosU$Usos, "semilla")
  granosU$u_Tortilla <- stringr::str_detect(granosU$Usos, "tortilla")
  granosU$u_Totomoxtle <- stringr::str_detect(granosU$Usos, "totomoxtle")

  granosU <- granosU[,-c(3,4)]

  granosUsos <- granosU %>%
    group_by(RazaPrimaria) %>%
    summarise(across(.cols = c(Atole:Totopo), .fns = sum)) %>%
    mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
    tidyr::pivot_longer(cols = c(Atole:Totopo)) %>%
    transmute(RazaPrimaria, name,
              Porcentaje = round(value / Total, 2)) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "Porcentaje", values_fill = 0) %>%
    dplyr::arrange(Atole)

  usosUsos <- granosU %>%
    group_by(RazaPrimaria) %>%
    summarise(across(.cols = c(u_Abono:u_Totomoxtle), .fns = sum)) %>%
    mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
    tidyr::pivot_longer(cols = c(u_Abono:u_Totomoxtle)) %>%
    transmute(RazaPrimaria, name,
              Porcentaje = round(value / Total, 2)) %>%
    tidyr::pivot_wider(names_from = "name", values_from = 'Porcentaje', values_fill = 0) %>%
    dplyr::arrange(u_Abono)
  colnames(usosUsos) <- c('RazaPrimaria', 'Abono', 'Alimento', 'Combustible',
                          'Elote', 'Forraje', 'Grano', 'Hoja', 'Nixtamal', 'Otro',
                          'Pozole', 'Semilla', 'Tortilla', 'Totomoxtle')

  p1 <- if(!is.na(granosUsos[1,2])){
    granosUsos |>
      echarts4r::e_charts(RazaPrimaria) |>
      echarts4r::e_color(background = "#fffce2") |>
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "percent"), max = 1) |>
      echarts4r::e_bar(Atole, stack = "grupo") |>
      echarts4r::e_bar(Corundas, stack = "grupo") |>
      echarts4r::e_bar(Elote, stack = "grupo") |>
      echarts4r::e_bar(Esquites, stack = "grupo") |>
      echarts4r::e_bar(Forraje, stack = "grupo") |>
      echarts4r::e_bar(Gorditas, stack = "grupo") |>
      echarts4r::e_bar(Harina, stack = "grupo") |>
      echarts4r::e_bar(Hoja, stack = "grupo") |>
      echarts4r::e_bar(Huacholes, stack = "grupo") |>
      echarts4r::e_bar(Nixtamal, stack = "grupo") |>
      echarts4r::e_bar(Panuchos, stack = "grupo") |>
      echarts4r::e_bar(Pinole, stack = "grupo") |>
      echarts4r::e_bar(Ponteduro, stack = "grupo") |>
      echarts4r::e_bar(Pozol, stack = "grupo") |>
      echarts4r::e_bar(Pozole, stack = "grupo") |>
      echarts4r::e_bar(Salbutes, stack = "grupo") |>
      echarts4r::e_bar(Semilla, stack = "grupo") |>
      echarts4r::e_bar(Tamal, stack = "grupo") |>
      echarts4r::e_bar(Tesgüino, stack = "grupo") |>
      echarts4r::e_bar(Tlacoyos, stack = "grupo") |>
      echarts4r::e_bar(Tomoxtle, stack = "grupo") |>
      echarts4r::e_bar(Tortilla, stack = "grupo") |>
      echarts4r::e_bar(Tostada, stack = "grupo") |>
      echarts4r::e_bar(Totopo, stack = "grupo") |>
      echarts4r::e_title("Usos del grano") |>
      echarts4r::e_tooltip(trigger = "item") |>
      echarts4r::e_grid(right = '15%') |>
      echarts4r::e_legend(type = 'scroll', orient = 'vertical',
               right = '5', top = '10%') |>
      echarts4r::e_toolbox_feature('saveAsImage') #|>
      # echarts4r::e_theme_custom('{"color":["#FBEC5D","#FFA542","#00BFFF","#800080","#8B0000","#000000",
      #                           "#FFF380","#FF4500","#000080","#FFA5FF","#A52A2A","#D1D1D1",
      #                           "#FFE66D","#FFA500","#B2B2FF","#A52AA5","#690000","#404040",
      #                           "#FFFF00","#FF6900","#00FF00","#FF00FF","#FF8040","#FFFAFA",
      #                           "#FAFAD2","#FF003D","#B2B280","#D10069","#FFA07A","#666666",
      #                           "#FFFFD1","#A50042","#FFE6A5","#A02020","#800026","#FFF5D1"]}')
  } else {
    granosUsos |>
      echarts4r::e_chart(RazaPrimaria) |>
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
      echarts4r::e_title("Usos del grano")
    }

  p2 <- if(!is.na(usosUsos[1,2])){
    usosUsos |>
      echarts4r::e_charts(RazaPrimaria) |>
      echarts4r::e_color(background = "#fffce2") |>
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "percent"), max = 1) |>
      echarts4r::e_bar(Abono, stack = "grupo") |>
      echarts4r::e_bar(Alimento, stack = "grupo") |>
      echarts4r::e_bar(Combustible, stack = "grupo") |>
      echarts4r::e_bar(Elote, stack = "grupo") |>
      echarts4r::e_bar(Forraje, stack = "grupo") |>
      echarts4r::e_bar(Grano, stack = "grupo") |>
      echarts4r::e_bar(Hoja, stack = "grupo") |>
      echarts4r::e_bar(Nixtamal, stack = "grupo") |>
      echarts4r::e_bar(Otro, stack = "grupo") |>
      echarts4r::e_bar(Pozole, stack = "grupo") |>
      echarts4r::e_bar(Semilla, stack = "grupo") |>
      echarts4r::e_bar(Tortilla, stack = "grupo") |>
      echarts4r::e_bar(Totomoxtle, stack = "grupo") |>
      echarts4r::e_title("Usos en general") |>
      echarts4r::e_tooltip(trigger = "item") |>
      echarts4r::e_grid(right = '15%') |>
      echarts4r::e_legend(type = 'scroll', orient = 'vertical',
               right = '5', top = '10%') |>
      echarts4r::e_toolbox_feature('saveAsImage') #|>
      # echarts4r::e_theme_custom('{"color":["#FBEC5D","#FFA542","#00BFFF","#800080","#8B0000",
      #                           "#FFF380","#FF4500","#000080","#FFA5FF","#A52A2A",
      #                           "#FFE66D","#FFA500","#B2B2FF"]}')
      # echarts4r::e_theme_custom("usos2")
    } else {
      granosUsos |>
        echarts4r::e_chart(RazaPrimaria) |>
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
        echarts4r::e_title("Usos del grano")
    }

  p1 <- echarts4r::e_flip_coords(p1)
  p2 <- echarts4r::e_flip_coords(p2)

  pf <- list(p1, p2)
  return(pf)
}
