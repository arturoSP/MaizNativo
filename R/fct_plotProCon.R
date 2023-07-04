#' plotProCon
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr
#' @import echarts4r
#' @import tm
#' @importFrom stringr str_sub
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#'
# data pros y contras ----
plotProCon <- function(maizSelecto){
  agric <- maizSelecto %>%
    select(Id, RazaPrimaria, AFavor, EnContra)

  AFavor <- agric %>%
    filter(AFavor != "") %>%
    select(AFavor) %>%
    as.vector()

  CorpAFavor <- Corpus(VectorSource(AFavor))
  CorpAFavor <- tm_map(CorpAFavor, removePunctuation)
  CorpAFavor[["1"]][["content"]] <- if(dim(AFavor)[1] > 1){
    stringr::str_sub(CorpAFavor[["1"]][["content"]], start = 2)
  } else {
    CorpAFavor[["1"]][["content"]]
  }
  CorpAFavor[["1"]][["content"]] <- stringr::str_replace_all(CorpAFavor[["1"]][["content"]], "\n", "")
  CorpAFavor <- tm_map(CorpAFavor, tolower)
  CorpAFavor <- tm_map(CorpAFavor, removeWords, c("y", "que", "del", "el", "La",
                                                  "la", "en", "de", "por", "muy",
                                                  "para", "casi", "tiene", "esta",
                                                  "plagas", "gorgojo", "los",
                                                  "todo", "poco", "mucho", "las",
                                                  "no", "porque", "son", "nd"))
  MatrAFavor <- termFreq(CorpAFavor[["1"]][["content"]]) %>% as.data.frame()
  colnames(MatrAFavor) <- c("cuenta")
  MatrAFavor$cuenta <-  floor(MatrAFavor$cuenta / max(MatrAFavor$cuenta) * 100)
  MatrAFavor$palabra <- rownames(MatrAFavor)
  MatrAFavor <-  MatrAFavor %>% arrange(desc(cuenta))
  MatrAFavor <- if(nrow(MatrAFavor) >= 25) {
    MatrAFavor[c(1:25),]
  } else {
    MatrAFavor
  }
  MatrAFavor[1,2] <- ifelse(MatrAFavor[1,2] == "character0",
                            "No hubo\nregistros",
                            MatrAFavor[1,2])

  p1 <- MatrAFavor |>
    e_chart() |>
    e_color(background = "#fffce2") |>
    e_cloud(word = palabra, freq = cuenta, shape = "diamond", sizeRange = c(20,60),
            textStyle = list(color = "#3d5fec"),
            emphasis = list(focus = "self",
                            textStyle = list(textShadowBlur = 3, textShadowColor = "#a000c7"))) |>
    e_title("A los productores les gusta:") |>
    e_toolbox_feature('saveAsImage')

  EnContra <- agric %>%
    filter(EnContra != "") %>%
    select(EnContra) %>%
    as.vector()

  CorpEnContra <- Corpus(VectorSource(EnContra))
  CorpEnContra <- tm_map(CorpEnContra, removePunctuation)
  CorpEnContra[["1"]][["content"]] <- if(dim(AFavor)[1] > 1){
    stringr::str_sub(CorpEnContra[["1"]][["content"]], start = 2)
  } else {
    CorpEnContra[["1"]][["content"]]
  }
  CorpEnContra[["1"]][["content"]] <- stringr::str_replace_all(CorpEnContra[["1"]][["content"]], "\n", "")
  CorpEnContra <- tm_map(CorpEnContra, tolower)
  CorpEnContra <- tm_map(CorpEnContra, removeWords, c("y", "que", "del", "el",
                                                      "la", "en", "de", "sin", "lo",
                                                      "nada", "bien", "ninguna",
                                                      "comentarios", "nada", "no", "negativo",
                                                      "todo", "hay", "con", "por",
                                                      "para", "casi", "tiene", "los",
                                                      "poco", "mucho",
                                                      "las", "no", "para", "porque", "son",
                                                      "está", "muy", "más",
                                                      "algo", "veces", "a", "nd",
                                                      "le", "se", "gusta", "\\n",
                                                      "fácil", "cuando", "muchas", "muchos"
  ))
  CorpEnContra[["1"]][["content"]]
  MatrEnContra <- termFreq(CorpEnContra[["1"]][["content"]]) %>% as.data.frame()
  colnames(MatrEnContra) <- c("cuenta")
  MatrEnContra$cuenta <- floor(MatrEnContra$cuenta / max(MatrEnContra$cuenta) * 100)
  MatrEnContra$palabra <- rownames(MatrEnContra)
  MatrEnContra <-  MatrEnContra %>% arrange(desc(cuenta))
  MatrEnContra <- if(nrow(MatrEnContra) >= 25) {
    MatrEnContra[c(1:25),]
  } else {
    MatrEnContra
  }
  MatrEnContra[1,2] <- ifelse(MatrEnContra[1,2] == "character0",
                              "No hubo\nregistros",
                              MatrEnContra[1,2])

  p2 <- MatrEnContra |>
    e_chart() |>
    e_color(background = "#fffce2") |>
    e_cloud(word = palabra, freq = cuenta, shape = "diamond", sizeRange = c(20,60),
            textStyle = list(color = "#a000c7"),
            emphasis = list(focus = "self",
                            textStyle = list(textShadowBlur = 3, textShadowColor = "#3d5fec"))) |>
    e_title("A los productores no les gusta:") |>
    e_toolbox_feature('saveAsImage')

  pf <- list(p1, p2)
  return(pf)
}
