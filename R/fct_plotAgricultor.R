#' plotAgricultor
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
# data de agricultores ----
plotAgricultor <- function(maizSelecto){
  agric <- maizSelecto %>%
    select(Id, RazaPrimaria, GpoIndigenaAgricultor:PorMejorar)

  p1 <- agric %>%
    filter(!is.na(EdadAgricultor)) %>%
    ggplot(aes(x = RazaPrimaria, y = EdadAgricultor, fill = RazaPrimaria))+
    geom_violin(color = "gray80")+
    facet_wrap(facets = ~RazaPrimaria, scales = "free_x", nrow = 1)+
    ylab("Años")+
    theme_classic()+
    temabottom +
    scale_fill_manual(values = coloresTransparencia)+
    ggtitle("Edad de los agricultores")

  p2 <- agric %>%
    filter(GpoIndigenaAgricultor != "" & GpoIndigenaAgricultor != "ND") %>%
    filter(GpoIndigenaAgricultor != "Ninguno") %>%
    group_by(RazaPrimaria, GpoIndigenaAgricultor) %>%
    summarise(Cuenta = n()) %>%
    ggplot(aes(x = RazaPrimaria, y = Cuenta, fill = GpoIndigenaAgricultor))+
    geom_col(position = "stack")+
    ylab("Productores")+
    facet_wrap(facets = ~RazaPrimaria, scales = "free_x", nrow = 1)+
    theme_classic()+
    temabottom +
    scale_fill_manual(values = coloresTransparencia)+
    ggtitle("Pertenencia a grupos indígenas")

  pf <- ggpubr::ggarrange(p1, p2, nrow = 1)
  return(pf)
}
