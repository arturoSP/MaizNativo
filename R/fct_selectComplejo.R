#' selectComplejo
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import shiny
#'
# selecciona el archivo a presentar
selectComplejo <- function(complejo){
  info <- if(complejo == "Cónico"){
    #ComplText[2]
    texto <- paste(ComplText[2], ComplText[3], sep = "\n")
    capture.output(cat(texto, sep = "\n"))
  } else if(complejo == "Dentados tropicales"){
    #ComplText[22]
    capture.output(cat(ComplText[22], ComplText[23], sep = "  "))
  } else if(complejo == "Tropicales tardíos"){
    #ComplText[26]
    capture.output(cat(ComplText[26], ComplText[27], sep = "  "))
  } else if(complejo == "Ocho hileras"){
    #ComplText[10]
    capture.output(cat(ComplText[10], ComplText[11], sep = "  "))
  } else if(complejo == "Sierra de Chihuahua"){
    #ComplText[6]
    capture.output(cat(ComplText[6], ComplText[7], sep = "  "))
  } else if(complejo == "Chapalote"){
    #ComplText[14]
    capture.output(cat(ComplText[14], ComplText[15], sep = "  "))
  } else if(complejo == "Tropicales precoces"){
    #ComplText[18]
    capture.output(cat(ComplText[18], ComplText[19], sep = "  "))
  } else {
    #ComplText[29]
    capture.output(cat(ComplText[29]))
  }

  pict <- if(complejo == "Cónico"){
    list(src = "./inst/app/www/maizConico.jpg",
         alt = "Muestra de maíz cónico",
         width = "400px")
  } else if(complejo == "Dentados tropicales"){
    list(src = "./inst/app/www/maizTuxpeno.jpg",
         alt = "Muestra de maíz tuxpeño",
         width = "400px")
  } else if(complejo == "Tropicales tardíos"){
    list(src = "./inst/app/www/maizOlotillo.jpg",
         alt = "Muestra de maíz olotillo",
         width = "400px")
  } else if(complejo == "Ocho hileras"){
    list(src = "./inst/app/www/maizBlando.jpg",
         alt = "Muestra de maíz blando",
         width = "400px")
  } else if(complejo == "Sierra de Chihuahua"){
    list(src = "./inst/app/www/maizCristalinoChih.jpg",
         alt = "Muestra de maíz cristalino de Chihuahua",
         width = "400px")
  } else if(complejo == "Chapalote"){
    list(src = "./inst/app/www/maizChapalote.jpg",
         alt = "Muestra de maíz chapalote",
         width = "400px")
  } else if(complejo == "Tropicales precoces"){
    list(src = "./inst/app/www/maizZapaloteCh.jpg",
         alt = "Muestra de maíz zapalote chico",
         width = "400px")
  } else {
    list(src = "./inst/app/www/maizOlotillo.jpg",
         alt = "Muestra de maíz olotillo",
         width = "400px")
  }

  infoCompleta <- list(info = info, pict = pict)

  return(infoCompleta)
}

