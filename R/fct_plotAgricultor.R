#' plotAgricultor
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr
#' @import echarts4r
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
# data de agricultores ----
plotAgricultor <- function(maizSelecto){
  plot2 <- function(agric){
    gInd <- agric %>%
      filter(GpoIndigenaAgricultor != "" & GpoIndigenaAgricultor != "ND") %>%
      filter(GpoIndigenaAgricultor != "Ninguno") %>%
      filter(GpoIndigenaAgricultor != "No pertenece") %>%
      filter(GpoIndigenaAgricultor != "Español" ) %>%
      group_by(RazaPrimaria, GpoIndigenaAgricultor) %>%
      summarise(Cuenta = n())
    gInd <- left_join(GpoIndigena, gInd, by = "GpoIndigenaAgricultor") |>
      tidyr::pivot_wider(names_from = GpoIndigenaAgricultor, values_from = Cuenta,
                         values_fill = 0) |>
      filter(!is.na(RazaPrimaria))
    if(dim(gInd)[1] > 0){
      gInd |>
        echarts4r::e_charts(RazaPrimaria) |>
        echarts4r::e_color(background = "#fffce2") |>
        echarts4r::e_bar(Chinanteco, stack = "Indigena") |>
        echarts4r::e_bar(Chol, stack = "Indigena") |>
        echarts4r::e_bar(Cuicateco, stack = "Indigena") |>
        echarts4r::e_bar(Huasteco, stack = "Indigena") |>
        echarts4r::e_bar(Huichol, stack = "Indigena") |>
        echarts4r::e_bar(Mam, stack = "Indigena") |>
        echarts4r::e_bar(Maya, stack = "Indigena") |>
        echarts4r::e_bar(Mazateco, stack = "Indigena") |>
        echarts4r::e_bar(Mestizo, stack = "Indigena") |>
        echarts4r::e_bar(Mixteco, stack = "Indigena") |>
        echarts4r::e_bar(Náhuatl, stack = "Indigena") |>
        echarts4r::e_bar(Otomí, stack = "Indigena") |>
        echarts4r::e_bar(`Otomí tepehua`, stack = "Indigena") |>
        echarts4r::e_bar(Pame, stack = "Indigena") |>
        echarts4r::e_bar(Popoluca, stack = "Indigena") |>
        echarts4r::e_bar(Tarahumara, stack = "Indigena") |>
        echarts4r::e_bar(Tlapaneco, stack = "Indigena") |>
        echarts4r::e_bar(Tojolabal, stack = "Indigena") |>
        echarts4r::e_bar(Totonaco, stack = "Indigena") |>
        echarts4r::e_bar(Tzeltal, stack = "Indigena") |>
        echarts4r::e_bar(Tzotzil, stack = "Indigena") |>
        echarts4r::e_bar(Zapoteco, stack = "Indigena") |>
        echarts4r::e_title("Número de agricultores que pertenecen a grupos indígenas") |>
        echarts4r::e_tooltip(trigger = "item") |>
        echarts4r::e_grid(right = '15%') |>
        echarts4r::e_legend(type = 'scroll', orient = 'vertical',
                 right = '5', top = '10%') |>
        echarts4r::e_toolbox_feature('saveAsImage')
    } else {
      gInd |>
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
        echarts4r::e_title("Número de agricultores que pertenecen a grupos indígenas")

    }
  }

  # main function ----
  agric <- maizSelecto %>%
    select(Id, RazaPrimaria, GpoIndigenaAgricultor:EdadAgricultor)

  #p1 <- plot1(agric)

  p2 <- plot2(agric)
  p2 <- echarts4r::e_flip_coords(p2)

  #pf <- ggpubr::ggarrange(p1, p2, nrow = 1)
  return(p2)
}
