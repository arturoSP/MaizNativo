\name{bdMaiz}
\alias{bdMaiz}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("bdMaiz")}
\format{
  A data frame with 22931 observations on the following 85 variables.
  \describe{
    \item{\code{Id}}{Identificador único y consecutivo de la base de datos general, generado a partir de la integración de los diferentes registros de los diferentes proyectos.}
    \item{\code{IdEjemplar}}{Identificador del ejemplar en la base de datos específica de cada proyecto}
    \item{\code{SiglasColeccion}}{Siglas o abreviatura de la colección donde se ha resguardado el material colectado.}
    \item{\code{NombreColeccion}}{Nombre de la colección donde se resguardó el material colectado.}
    \item{\code{NumeroDeCatalogo}}{Identificador de la muestra con el que se registra un ejemplar o accesión dentro de una colección.}
    \item{\code{Fuente}}{Clave con que se nombró al proyecto específico del cual provienen los datos, registros o accesiones para la base de datos general.}
    \item{\code{FechaColecta}}{Fecha en que se obtuvo en campo la colecta u observación de los datos, registros o accesiones de la base de datos.}
    \item{\code{PeriodoColecta}}{Periodo en que se obtuvo en campo la colecta u observación de los datos, registros o accesiones de la base de datos con base en los siguientes criterios: Periodo I (1940-1965), periodo II (1966-1990) y periodo III (1991-2010).}
    \item{\code{NumeroColecta}}{Número de colecta para un registro, colecta o accesión de la base de datos general asignado por el colecto de la muestra.}
    \item{\code{DescripcionGpoColecta}}{Iniciales o abreviados del o los colectores del grupo de colecta que obtuvo el registro, colecta u accesión.}
    \item{\code{RazaPrimaria}}{Indica la raza principal al que fue asociado o identificado el registro, colecta o accesión.}
    \item{\code{ComplejoRacial}}{Indica el complejo racial al que está asociado o identificado el registro, colecta o accesión.}
    \item{\code{InfluenciaOtrasRazas}}{Indica que otras razas se encuentran asociadas al registro, colecta o accesión.}
    \item{\code{DiversidadMaiz}}{Se refiere a los tipos de maíces que el agricultor maneja asociados al que hace referencia el registro, colecta o accesión.}
    \item{\code{AFavor}}{Se reporta la información de características que son destacables o relevantes para el agricultor al que hace referencia el registro, colecta o accesión.}
    \item{\code{EnContra}}{Se reporta la información de características que no son deseables o son desfavorables para el agricultor al que hace referencia el registro, colecta o accesión.}
    \item{\code{PorMejorar}}{Se reporta la información de características que al agricultor le gustaría mejorar a la variedad al que hace referencia el registro, colecta o accesión.}
    \item{\code{Edafologia}}{Se refiere al tipo, textura o características del suelo del sitio al que hace referencia el registro, colecta o accesión.}
    \item{\code{Topografia}}{Se refiere al elemento topográfico donde se cultiva el material al que hace referencia el registro, colecta o accesión.}
    \item{\code{NombreComun}}{Hace referencia al nombre o nombres comunes o locales con los que se le conoce al tipo de maíz o la raza en una región determinada (municipio).}
    \item{\code{Estado}}{Estado de la República Mexicana donde se obtuvo el registro, colecta o accesión.}
    \item{\code{Municipio}}{Municipio dentro del estado donde se obtuvo el registro, colecta o accesión.}
    \item{\code{Localidad}}{Localidad donde se obtuvo el registro, colecta o accesión.}
    \item{\code{Longitud}}{Expresan las coordenadas de longitud en grados decimales para un registro, colecta o accesión.}
    \item{\code{Latitud}}{Expresan las coordenadas de latitud en grados decimales para un registro, colecta o accesión.}
    \item{\code{Altitud}}{Expresa la altitud que del sitio o lugar donde se obtuvo el registro, colecta o accesión.}
    \item{\code{ValidacionGeografica}}{Resultado de la validación geográfica a nivel municipal para cada registro, colecta o accesión.  Se denotan 3 tres tipos de datos: consistentes, consistente con tolerancia de 250 m (es un registro que se presenta en un municipio contiguo al señalado en la base de datos, pero válido por estar dentro de una zona de influencia de 250 m en una escala cartográfica de 1:250,000) e inconsistentes, estos últimos no presentan datos de coordenadas.}
    \item{\code{EdadAgricultor}}{Edad del agricultor que donó y/o proporcionó información sobre el tipo de maíz al que hace referencia el registro, colecta o accesión. Los campos NombreAgricultor y DireccionAgricultor no fueron incluidos en este corte de la base de datos por que se consideran datos confidenciales.}
    \item{\code{GpoIndigenaAgricultor}}{Grupo indígena al que pertenece el agricultor que donó y/o proporcionó información sobre el tipo de maíz al que hace referencia el registro, colecta o accesión.}
    \item{\code{FuenteColecta}}{Sitio del cual se obtuvo la información sobre el tipo de maíz al que hace referencia el registro, colecta o accesión.}
    \item{\code{SuperficieSembrada}}{Se indica, en los casos que se obtuvo o se reportó esta información, la superficie sembrada del registro, colecta o accesión.}
    \item{\code{TamañoColectaGrano}}{Cantidad de muestra obtenida por el colector de grano y semilla sobre el tipo de maíz al que hace referencia el registro, colecta o accesión.}
    \item{\code{TamañoColectaMazorca}}{Cantidad de muestra obtenida por el colector de mazorcas sobre el tipo de maíz al que hace referencia el registro, colecta o accesión.}
    \item{\code{MezclaAñosCultivo}}{Años de cultivar la mezcla varietal al que hace referencia el registro, colecta o accesión.}
    \item{\code{MezclaProcedencia}}{Procedencia de la mezcla varietal al que hace referencia el registro, colecta o accesión.}
    \item{\code{VariedadAgricultorAñosCultivo}}{Años de cultivar la variedad del agricultor al que hace referencia el registro, colecta o accesión.}
    \item{\code{VariedadAgricultorProcedencia}}{Procedencia de la variedad del agricultor al que hace referencia el registro, colecta o accesión.}
    \item{\code{VariedadIntroducidaAñosCultivo}}{Años de cultivar la variedad introducida al que hace referencia el registro, colecta o accesión.}
    \item{\code{VariedadIntroducidaProcedencia}}{Procedencia de la variedad introducida al que hace referencia el registro, colecta o accesión.}
    \item{\code{VariedadMejoradaAñosCultivo}}{Años de cultivar la variedad mejorada al que hace referencia el registro, colecta o accesión.}
    \item{\code{VariedadMejoradaProcedencia}}{Procedencia de la variedad mejorada al que hace referencia el registro, colecta o accesión.}
    \item{\code{Usos}}{Usos que se le da a la muestra al que hace referencia el registro, colecta o accesión.}
    \item{\code{UsosGrano}}{Usos que se le da al grano o semilla al que hace referencia el registro, colecta o accesión.}
    \item{\code{Destino}}{Destino de la producción al que hace referencia el registro, colecta o accesión.}
    \item{\code{EpocaSiembra1}}{Fecha o temporada en la que se llevó a cabo la siembra del material al que hace referencia el registro, colecta o accesión.}
    \item{\code{EpocaSiembra2}}{Fecha o temporada en la que se llevó a cabo la siembra del material al que hace referencia el registro, colecta o accesión.}
    \item{\code{EpocaFloracion}}{Periodo en el que las flores permanecen abiertas en el que se lleva a cabo el proceso de polinización y fertilización del material al que hace referencia el registro, colecta o accesión (esta información no fue uniformizada, los datos del campo se encuentran tal como fueron capturados).}
    \item{\code{EpocaMadurez}}{Periodo en el que el informante o agricultor considera que el material al que hace referencia el registro, colecta o accesión esta lista para cosecharse (esta información no fue uniformizada, los datos del campo se encuentran tal como fueron capturados).}
    \item{\code{EpocaCosecha1}}{Fecha o temporada en la que se llevó a cabo la cosecha del material al que hace referencia el registro, colecta o accesión.}
    \item{\code{EpocaCosecha2}}{Fecha o temporada en la que se llevó a cabo la cosecha del material al que hace referencia el registro, colecta o accesión.}
    \item{\code{MetodoSiembra}}{Se refiere a las diferentes modalidades manuales o tecnificadas que utiliza el agricultor o informante para la siembra del material asociado al registro, colecta o accesión.}
    \item{\code{SistemaCultivo}}{Se refiere al sistema de siembra (monocultivo ó policultivo) al que hace referencia el registro, colecta o accesión.}
    \item{\code{CultivosAsociados}}{Se indican los cultivos que acompañan al material asociado al registro, colecta o accesión.}
    \item{\code{Rendimiento1}}{Se indica el rendimiento estimado, en kilogramos por hectárea, mencionado por el agricultor para el registro, colecta u accesión.}
    \item{\code{Rendimiento2}}{Se indica el rendimiento estimado, en kilogramos por hectárea, mencionado por el agricultor para el registro, colecta u accesión.}
    \item{\code{Insectos}}{Se refiere a que si el agricultor observo algún insecto dañino en la planta o en alguna estructura de ella, para el registro, colecta o accesión.}
    \item{\code{Malezas}}{Se refiere a que si el agricultor observo alguna maleza asociada al cultivo al que hace referencia el registro, colecta o accesión.}
    \item{\code{ProblemasAlmacenamiento}}{Se reportan los problemas que se presentan durante el almacenamiento asociado a plagas o enfermedades al que hace referencia el registro, colecta o accesión.}
    \item{\code{ControlMecanico}}{Indica el control mecánico que se aplica para efectuar control de malezas, plagas o enfermedades asociadas a la población del registro, colecta o accesión.}
    \item{\code{ControlFungicidaBactericida}}{Indica si se aplican fungicidas o bactericidas para control de enfermedades asociadas a la población del registro, colecta o accesión.}
    \item{\code{ControlInsecticida}}{Indica si se aplican insecticidas para control de plagas asociadas a la población del registro, colecta o accesión.}
    \item{\code{ControlHerbicida}}{Indica si se aplican herbicidas para control de malezas asociadas a la población del registro, colecta o accesión.}
    \item{\code{Fertilizante}}{Se señala en este campo si se aplica algún tipo de fertilizante asociado a la población del registro, colecta o accesión.}
    \item{\code{TipoRiego}}{Indica el tipo de riego empleado en el cultivo al que hace referencia el registro, colecta o accesión.}
    \item{\code{AlturaPlanta}}{Medida en centímetros de la planta desde el suelo hasta la base de la espiga.}
    \item{\code{AlturaMazorca}}{Medida en centímetros desde el suelo hasta la base de la mazorca.}
    \item{\code{NumeroMazorcas}}{Número de mazorcas que presenta la planta.}
    \item{\code{LongitudMazorca}}{Medida en centímetros desde la base de la mazorca hasta la punta.}
    \item{\code{DiametroMazorca}}{Es la medida en centímetros que mide la mazorca en la parte central.}
    \item{\code{HilerasMazorca}}{Es el número de hileras de granos que tiene una mazorca.}
    \item{\code{RatioDiamLongMazorca}}{Es el valor de la relación entre el diámetro y la longitud de la mazorca}
    \item{\code{GranosHilera}}{Es el número de granos que tiene cada hilera.}
    \item{\code{GranoAnchura}}{Es el promedio de la medida de 10 granos consecutivos en su punto medio. Esta medida está dada en milímetros.}
    \item{\code{GranoGrosor}}{Es el promedio de la medida de 10 granos consecutivos en su espesor. Esta medida está dada en milímetros.}
    \item{\code{GranoLongitud}}{Es el promedio de la medida de 10 granos consecutivos en su altura. Esta medida está dada en milímetros}
    \item{\code{ContenidoHumedad}}{Es el porcentaje de humedad contenido en una muestra que se seca para su conservación o almacenamiento. Esta medida está dada en porcentaje.}
    \item{\code{Volumen100Granos}}{Es el volumen de 100 granos expresado en centímetros cúbicos.}
    \item{\code{Peso100Granos}}{Es el peso de 100 granos expresado en gramos.}
    \item{\code{RatioAnchLongGrano}}{Es el valor de la relación entre la anchura y la longitud del grano}
    \item{\code{RatioGrosAnchGrano}}{Es el valor de la relación entre el grosor y la anchura del grano}
    \item{\code{ColorGrano}}{Tonalidades y coloración del grano de maíz, característica importante ya que hace referencia a la calidad y uso de la variedad. Existen blancos, amarillos, azules, rojos etc.}
    \item{\code{ColorOlote}}{Coloración del olote de la mazorca, es información útil para la caracterización de las variedades. Las tonalidades van del amarillo, azul, blanco, rojo etc.}
    \item{\code{DisposicionHileras}}{Posición que guardan las hileras en la mazorca, es información útil para la caracterización de las variedades. Se clasifican en espiral, irregular, recta, semirecta y regular.}
    \item{\code{FormaMazorca}}{Forma que tiene la mazorca, es información útil para la caracterización de las variedades. Se clasifican en cilíndrica, cónica y esférica.}
    \item{\code{TipoGrano}}{Tipos y o texturas de grano de maíz, es información útil para la caracterización de las variedades. Se clasifican principalmente en ceroso, cristalino, dentado, dulce, harinoso y reventador.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\href{https://www.biodiversidad.gob.mx/diversidad/proyectoMaices}{Proyecto global de maíces nativos} (CONABIO, 2017).
}
\examples{
data(bdMaiz)
selecta(bdMaiz, "Cónicos", "Todos")
}
\keyword{datasets}
