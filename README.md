
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MaizNativo

<!-- badges: start -->

![stable](https://img.shields.io/badge/lifecycle-stable-green.svg)
[![License](https://img.shields.io/badge/License-GPL3-blue.svg)](https://github.com/arturoSP/ecocbo/blob/master/LICENSE.md)
<!-- badges: end -->

## Introducción

El **Proyecto Global de Maíces (PGM)** fue un proyecto de investigación
realizado por la Comisión Nacional para el Conocimiento y Uso de la
Biodiversidad (CONABIO) en México. El objetivo del PGM fue actualizar la
información sobre el maíz y sus parientes silvestres en México, con el
fin de determinar los centros de origen y diversidad genética del maíz.
Entre los resultados del PGM se obtuvieron bases de datos con más de
24,000 registros provenientes de la recolecta de muestras de la mayor
parte de las zonas agrícolas donde se cultiva maíz nativo en el país,
revelando así que México tiene 64 razas de maíz, de las cuales 59 son
razas nativas.

Con esta aplicación web pretendemos dar a conocer con más detalle
algunos de los resultados del PGM. La aplicación permite a los usuarios
aprender acerca de los distintos complejos raciales, visualizar mapas de
la distribución de las razas registradas en la base de datos de 2010, y
acceder a información sobre la diversidad genética del maíz nativo en
México. Además, la aplicación proporciona a los usuarios recursos sobre
cómo conservar y promover el uso de las variedades nativas de maíz.

## Instalación

Se puede instalar la versión de desarrollo del paquete MaizNativo desde
[GitHub](https://github.com/) con:

``` r
# install.packages("devtools")
devtools::install_github("arturoSP/MaizNativo")
```

## Webapp en shinyapp

En caso de no contar con acceso a R o RStudio, se puede visualizar la
información desde una ventana de navegador visitando la plataforma
[shinyapps](https://arturosp.shinyapps.io/MaizNativo/).

## Paquetes requeridos

dplyr, echarts4r, fullPage, ggplot2, golem (\>= 0.4.0), leaflet,
magrittr, shiny (\>= 1.7.4), shinycssloaders, stringr, tidyr, tm

## Proyecto desarrollado con apoyo de:

<img src="inst/app/www/logo_conacyt.png" height="120" />
<img src="inst/app/www/logo_enes.png" height="120" />
<img src="inst/app/www/logo_uaeh.png" height="120" />
