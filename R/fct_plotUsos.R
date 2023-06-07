#' plotUsos
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr
#' @import ggplot2
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_lower
#' @importFrom tidyr pivot_longer
#' @importFrom ggpubr ggarrange
#' @importFrom magrittr %>%
#'
#'
## data de usos ----
plotUsos <- function(maizSelecto){
  granosU <- maizSelecto %>%
    select(Id, RazaPrimaria, UsosGrano, Usos) %>%
    filter(UsosGrano != "ND") %>%
    filter(Usos != "ND") %>%
    mutate(UsosGrano = str_to_lower(UsosGrano),
           Usos = str_to_lower(Usos))

  granosU$Atole <- str_detect(granosU$UsosGrano, "atole")
  granosU$Corundas <- str_detect(granosU$UsosGrano, "corundas")
  granosU$Elote <- str_detect(granosU$UsosGrano, "elote")
  granosU$Esquites <- str_detect(granosU$UsosGrano, "esquites")
  granosU$Forraje <- str_detect(granosU$UsosGrano, "forraje")
  granosU$Gorditas <- str_detect(granosU$UsosGrano, "gorditas")
  granosU$Harina <- str_detect(granosU$UsosGrano, "harina")
  granosU$Hoja <- str_detect(granosU$UsosGrano, "hoja")
  granosU$Huacholes <- str_detect(granosU$UsosGrano, "huacholes")
  granosU$Nixtamal <- str_detect(granosU$UsosGrano, "nixtamal")
  granosU$Panuchos <- str_detect(granosU$UsosGrano, "panuchos")
  granosU$Pinole <- str_detect(granosU$UsosGrano, "pinole")
  granosU$Ponteduro <- str_detect(granosU$UsosGrano, "ponteduro")
  granosU$Pozol <- str_detect(granosU$UsosGrano, "pozol")
  granosU$Pozole <- str_detect(granosU$UsosGrano, "pozole")
  granosU$Salbutes <- str_detect(granosU$UsosGrano, "salbutes")
  granosU$Semilla <- str_detect(granosU$UsosGrano, "semilla")
  granosU$Tamal <- str_detect(granosU$UsosGrano, "tamal")
  granosU$Tesgüino <- str_detect(granosU$UsosGrano, "tesgüino")
  granosU$Tlacoyos <- str_detect(granosU$UsosGrano, "tlacoyos")
  granosU$Tomoxtle <- str_detect(granosU$UsosGrano, "tomoxtle")
  granosU$Tortilla <- str_detect(granosU$UsosGrano, "tortilla")
  granosU$Tostada <- str_detect(granosU$UsosGrano, "tostada")
  granosU$Totopo <- str_detect(granosU$UsosGrano, "totopo")
  granosU$u_Abono <- str_detect(granosU$Usos, "abono")
  granosU$u_Alimento <- str_detect(granosU$Usos, "alimento")
  granosU$u_Combustible <- str_detect(granosU$Usos, "combustible")
  granosU$u_Elote <- str_detect(granosU$Usos, "elote")
  granosU$u_Forraje <- str_detect(granosU$Usos, "forraje")
  granosU$u_Grano <- str_detect(granosU$Usos, "grano")
  granosU$u_Hoja <- str_detect(granosU$Usos, "hoja")
  granosU$u_Nixtamal <- str_detect(granosU$Usos, "nixtamal")
  granosU$u_Otro <- str_detect(granosU$Usos, "otro")
  granosU$u_Pozole <- str_detect(granosU$Usos, "pozole")
  granosU$u_Semilla <- str_detect(granosU$Usos, "semilla")
  granosU$u_Tortilla <- str_detect(granosU$Usos, "tortilla")
  granosU$u_Totomoxtle <- str_detect(granosU$Usos, "totomoxtle")

  granosU <- granosU[,-3]

  granosUsos <- granosU %>%
    group_by(RazaPrimaria) %>%
    summarise(across(.cols = c(Atole:Totopo), .fns = sum)) %>%
    tidyr::pivot_longer(cols = c(Atole:Totopo), names_to = "Uso", values_to = "Valor") %>%
    group_by(RazaPrimaria) %>%
    reframe(Total = sum(Valor))
  usosUsos <- granosU %>%
    group_by(RazaPrimaria) %>%
    summarise(across(.cols = c(u_Abono:u_Totomoxtle), .fns = sum)) %>%
    tidyr::pivot_longer(cols = c(u_Abono:u_Totomoxtle),
                        names_to = "Uso", values_to = "Valor",
                        names_prefix = "u_") %>%
    group_by(RazaPrimaria) %>%
    reframe(Total = sum(Valor))

  p1 <- granosU %>%
    group_by(RazaPrimaria) %>%
    summarise(across(.cols = c(u_Abono:u_Totomoxtle), .fns = sum)) %>%
    tidyr::pivot_longer(cols = c(u_Abono:u_Totomoxtle),
                        names_to = "Uso",
                        values_to = "Valor",
                        names_prefix = "u_") %>%
    left_join(usosUsos) %>%
    mutate(Porcentaje = Valor / Total) %>%
    ggplot(aes(x = RazaPrimaria, y = Porcentaje, fill = Uso))+
    geom_col(color = "gray80", alpha = 0.7)+
    facet_wrap(~RazaPrimaria, nrow = 1, scales = "free_x")+
    scale_fill_manual(values = coloresTransparencia)+
    theme_classic()+
    temabottom+
    ggtitle("Usos en general")+
    ylab("Proporción de uso")

  p2 <- granosU %>%
    group_by(RazaPrimaria) %>%
    summarise(across(.cols = c(Atole:Totopo), .fns = sum)) %>%
    tidyr::pivot_longer(cols = c(Atole:Totopo), names_to = "Uso", values_to = "Valor") %>%
    left_join(granosUsos) %>%
    mutate(Porcentaje = Valor / Total) %>%
    ggplot(aes(x = RazaPrimaria, y = Porcentaje, fill = Uso))+
    geom_col(color = "gray80", alpha = 0.7)+
    facet_wrap(~RazaPrimaria, nrow = 1, scales = "free_x")+
    scale_fill_manual(values = coloresTransparencia)+
    theme_classic()+
    temabottom+
    ggtitle("Usos del grano")+
    ylab("Proporción de uso")

  pf <- ggpubr::ggarrange(p1, p2)
  return(pf)
}
