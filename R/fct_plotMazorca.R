#' plotMazorca
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr
#' @import echarts4r
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#'
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
                                       "Hileras por\nmazorca",
                                       "Número de\nmazorcas"),
                            ordered = T)) %>%
    filter(Metrica != "Altura de la mazorca")
  mazorcas <- if(dim(mazorcas)[1] > 0){
    mazorcas
  } else {
    data.frame(Id = 0,
               RazaPrimaria = unique(maizSelecto$RazaPrimaria)[1],
               Metrica = c("Grosor", "Longitud",
                           "Anchura", "Hilera"),
               Valor = 0)
  }

  p1 <- if(mazorcas[1,1] != 0){
    mazorcas |>
      group_by(RazaPrimaria) |>
      e_chart(Metrica, timeline = TRUE) |>
      e_color(background = "#fffff7") |>
      e_scatter(Valor, colorBy = 'data',
                symbol_size = 25,
                legend = FALSE) |>
      e_toolbox_feature('saveAsImage') |>
      e_title("Características de la mazorca")
  } else {
    mazorcas |>
      group_by(RazaPrimaria) |>
      e_chart(Metrica, timeline = TRUE) |>
      e_color(background = "#fffff7") |>
      e_graphic_g(type = 'text', rotation = 0,
                  left = 'center', top = 'center',
                  bounding = 'raw', right = 110,
                  bottom = 110, z = 100,
                  style = list(
                    fill = '#000',
                    text = 'No hay datos',
                    font = 'bold 36px sans-serif'
                  )
      ) |>
      e_title("Características del grano")
}

  p1 <- e_flip_coords(p1)

  return(p1)
}
