#' plotProCon
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr
#' @import ggplot2
#' @import tm
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @importFrom ggpubr ggarrange
#' @importFrom ggwordcloud geom_text_wordcloud
#' @importFrom magrittr %>%
#'
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
  MatrAFavor$palabra <- rownames(MatrAFavor)
  MatrAFavor <-  MatrAFavor %>% arrange(desc(cuenta))
  MatrAFavor <- if(nrow(MatrAFavor) >= 25) {
    MatrAFavor[c(1:25),]
  } else {
    MatrAFavor
  }
  p1 <- ggplot(MatrAFavor,
               aes(label = palabra, size = as.numeric(cuenta), color = as.numeric(cuenta)))+
    ggwordcloud::geom_text_wordcloud()+
    scale_size_area(max_size = 15)+
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
  MatrEnContra$palabra <- rownames(MatrEnContra)
  MatrEnContra <-  MatrEnContra %>% arrange(desc(cuenta))
  MatrEnContra <- if(nrow(MatrEnContra) >= 25) {
    MatrEnContra[c(1:25),]
  } else {
    MatrEnContra
  }
  p2 <- ggplot(MatrEnContra,
               aes(label = palabra, size = as.numeric(cuenta), color = as.numeric(cuenta)))+
    ggwordcloud::geom_text_wordcloud()+
    scale_size_area(max_size = 15)+
    scale_color_gradient2(low = "darkred", mid = "#FFDB48", high = "purple")+
    theme_classic()+
    ggtitle("A los productores no les gusta:")

  pf <- ggpubr::ggarrange(p1, p2, nrow = 1)
  return(pf)
}