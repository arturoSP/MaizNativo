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
  info1 <- if(complejo == "Cónico"){
    capture.output(cat(ComplText[2]))
  } else if(complejo == "Dentados tropicales"){
    capture.output(cat(ComplText[22]))
  } else if(complejo == "Tropicales tardíos"){
    capture.output(cat(ComplText[26]))
  } else if(complejo == "Ocho hileras"){
    capture.output(cat(ComplText[10]))
  } else if(complejo == "Sierra de Chihuahua"){
    capture.output(cat(ComplText[6]))
  } else if(complejo == "Chapalote"){
    capture.output(cat(ComplText[14]))
  } else if(complejo == "Tropicales precoces"){
    capture.output(cat(ComplText[18]))
  } else {
    capture.output(cat(ComplText[29]))
  }

  info2 <- if(complejo == "Cónico"){
    capture.output(cat(ComplText[3]))
  } else if(complejo == "Dentados tropicales"){
    capture.output(cat(ComplText[23]))
  } else if(complejo == "Tropicales tardíos"){
    capture.output(cat(ComplText[27]))
  } else if(complejo == "Ocho hileras"){
    capture.output(cat(ComplText[11]))
  } else if(complejo == "Sierra de Chihuahua"){
    capture.output(cat(ComplText[7]))
  } else if(complejo == "Chapalote"){
    capture.output(cat(ComplText[15]))
  } else if(complejo == "Tropicales precoces"){
    capture.output(cat(ComplText[19]))
  } else {
    capture.output(cat(""))
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
    list(src = "./inst/app/www/maizSinComplejo.png",
         alt = "Muestra de maíz olotillo",
         width = "400px")
  }

  infoCompleta <- list(info1 = info1, info2 = info2, pict = pict)

  return(infoCompleta)
}

