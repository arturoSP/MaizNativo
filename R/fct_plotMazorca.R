#' plotMazorca
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
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
                                       "Hileras por mazorca",
                                       "Número de mazorcas"),
                            ordered = T))
  p1 <- mazorcas %>%
    ggplot(aes(x = Metrica, y = Valor, fill = RazaPrimaria))+
    geom_violin(color = "gray80")+
    facet_wrap(facets = ~Metrica, scales = "free", nrow = 1)+
    ylab("[cm]")+
    theme_classic()+
    temabottom +
    scale_fill_manual(values = coloresTransparencia)+
    ggtitle("Características de la mazorca")

  return(p1)
}
