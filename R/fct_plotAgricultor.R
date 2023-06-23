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
  # helper functions ----
  plot1 <- function(agric){
    eAgr <- agric %>%
      filter(!is.na(EdadAgricultor)) %>%
      select(RazaPrimaria, EdadAgricultor)
    if(dim(eAgr)[1] > 0) {
      eAgr %>%
        ggplot(aes(x = RazaPrimaria, y = EdadAgricultor, fill = RazaPrimaria))+
        geom_violin(color = "gray80")+
        facet_wrap(facets = ~RazaPrimaria, scales = "free_x", nrow = 1)+
        ylab("Años")+
        theme_classic()+
        temabottom +
        scale_fill_manual(values = coloresTransparencia)+
        ggtitle("Edad de los agricultores")
    } else {
      eAgr %>%
        rbind(data.frame(RazaPrimaria = unique(agric$RazaPrimaria),
                         EdadAgricultor = "No hay datos")) %>%
        mutate(Cuenta = 0) %>%
        ggplot(aes(x = RazaPrimaria, y = Cuenta, fill = RazaPrimaria))+
        geom_text(aes(label = EdadAgricultor))+
        facet_wrap(~RazaPrimaria, scales = "free_x", nrow = 1)+
        theme_classic()+
        temabottom +
        theme(axis.title = element_blank(),
              axis.text = element_blank())+
        scale_fill_manual(values = coloresTransparencia)+
        ggtitle("Edad de los agricultores")
    }
  }

  plot2 <- function(agric){
    gInd <- agric %>%
      filter(GpoIndigenaAgricultor != "" & GpoIndigenaAgricultor != "ND") %>%
      filter(GpoIndigenaAgricultor != "Ninguno") %>%
      filter(GpoIndigenaAgricultor != "No pertenece") %>%
      group_by(RazaPrimaria, GpoIndigenaAgricultor) %>%
      summarise(Cuenta = n())
    if(dim(gInd)[1] > 0){
      gInd %>%
        ggplot(aes(x = RazaPrimaria, y = Cuenta, fill = GpoIndigenaAgricultor))+
        geom_col(position = "stack")+
        ylab("Productores")+
        facet_wrap(facets = ~RazaPrimaria, scales = "free_x", nrow = 1)+
        theme_classic()+
        temabottom +
        scale_fill_manual(values = coloresTransparencia)+
        ggtitle("Pertenencia a grupos indígenas")
    } else {
      gInd %>%
        rbind(data.frame(RazaPrimaria = unique(agric$RazaPrimaria),
                         GpoIndigenaAgricultor = "No hay datos",
                         Cuenta = 0)) %>%
        ggplot(aes(x = RazaPrimaria, y = Cuenta, fill = GpoIndigenaAgricultor))+
        geom_text(aes(label = GpoIndigenaAgricultor))+
        facet_wrap(facets = ~RazaPrimaria, scales = "free_x", nrow = 1)+
        theme_classic()+
        theme(axis.text = element_blank(),
              axis.title = element_blank())+
        temabottom +
        ggtitle("Pertenencia a grupos indígenas")
    }
  }

  # main function ----
  agric <- maizSelecto %>%
    select(Id, RazaPrimaria, GpoIndigenaAgricultor:EdadAgricultor)

  p1 <- plot1(agric)

  p2 <- plot2(agric)

  pf <- ggpubr::ggarrange(p1, p2, nrow = 1)
  return(pf)
}
