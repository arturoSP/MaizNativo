#' @import dplyr
#' @importFrom stringr str_to_title
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom rio import
#' @importFrom usethis use_data
#'

# importando base de datos ----
baseMaiz <- rio::import("BaseMaicesNativos csv")
#baseMaiz <- rio::import("./R/Maices/BaseMaicesNativos.csv")

# ajustando algunos valores y cambiando nombres a las columnas, para hacerlo fácil de recordar ----
bdMaiz <- baseMaiz %>%
  mutate(Estado = str_to_title(Estado)) %>%
  mutate(Municipio = str_to_title(Municipio)) %>%
  mutate(Localidad = str_to_title(Localidad)) %>%
  mutate(Estado = ifelse(Estado == "Distrito Federal", "Ciudad de México", Estado)) %>%
  mutate(Estado = ifelse(Estado == "Nd", "ND", Estado)) %>%
  mutate(`Diametro de mazorca` = ifelse(`Diametro de mazorca` > 20, `Diametro de mazorca`/10, `Diametro de mazorca`)) %>%
  mutate(`Granos por hilera` = ifelse(`Granos por hilera` > 100, `Granos por hilera` / 10, `Granos por hilera`)) %>%
  mutate(`Variedad mejorada/Años de cultivarla` = ifelse(`Variedad mejorada/Años de cultivarla` == "3 a 4 años", 4, `Variedad mejorada/Años de cultivarla`),
         `Variedad mejorada/Años de cultivarla` = ifelse(`Variedad mejorada/Años de cultivarla` == "ND", NA, `Variedad mejorada/Años de cultivarla`),
         `Variedad introducida/Años de cultivarla` = str_remove_all(.$`Variedad introducida/Años de cultivarla`, " años"),
         `Variedad introducida/Años de cultivarla` = ifelse(`Variedad introducida/Años de cultivarla` == "10 a 12", 11, `Variedad introducida/Años de cultivarla`),
         `Variedad introducida/Años de cultivarla` = ifelse(`Variedad introducida/Años de cultivarla` == "1 año", 1, `Variedad introducida/Años de cultivarla`),
         `Variedad introducida/Años de cultivarla` = ifelse(`Variedad introducida/Años de cultivarla` == "15 a 20", 18, `Variedad introducida/Años de cultivarla`),
         `Variedad introducida/Años de cultivarla` = ifelse(`Variedad introducida/Años de cultivarla` == "25-30", 28, `Variedad introducida/Años de cultivarla`),
         `Variedad introducida/Años de cultivarla` = ifelse(`Variedad introducida/Años de cultivarla` == "4 aproximadamente", 4, `Variedad introducida/Años de cultivarla`),
         `Variedad introducida/Años de cultivarla` = ifelse(`Variedad introducida/Años de cultivarla` == "4-5", 5, `Variedad introducida/Años de cultivarla`),
         `Variedad introducida/Años de cultivarla` = ifelse(`Variedad introducida/Años de cultivarla` == "Siempre", NA, `Variedad introducida/Años de cultivarla`),
         `Variedad introducida/Años de cultivarla` = ifelse(`Variedad introducida/Años de cultivarla` == "ND", NA, `Variedad introducida/Años de cultivarla`),
         `Tamaño de Colecta/Cantidad de mazorcas` = str_remove(.$`Tamaño de Colecta/Cantidad de mazorcas`, " mz")) %>%
  mutate(`Tamaño de Colecta/Cantidad de mazorcas` = str_remove(.$`Tamaño de Colecta/Cantidad de mazorcas`, " mazorcas")) %>%
  mutate(`Tamaño de Colecta/Cantidad de mazorcas` = str_remove(.$`Tamaño de Colecta/Cantidad de mazorcas`, "\\ "),
         `Tamaño de Colecta/Cantidad de mazorcas` = ifelse(`Tamaño de Colecta/Cantidad de mazorcas` == "1 en muy mal estado por almacenamiento de 4 años", 1, `Tamaño de Colecta/Cantidad de mazorcas`),
         `Tamaño de Colecta/Cantidad de mazorcas` = ifelse(`Tamaño de Colecta/Cantidad de mazorcas` == "Desgranadas", NA, `Tamaño de Colecta/Cantidad de mazorcas`),
         `Tamaño de Colecta/Cantidad de mazorcas` = ifelse(`Tamaño de Colecta/Cantidad de mazorcas` == "Desgranado", NA, `Tamaño de Colecta/Cantidad de mazorcas`),
         `Tamaño de Colecta/Cantidad de mazorcas` = ifelse(`Tamaño de Colecta/Cantidad de mazorcas` == "ND", NA, `Tamaño de Colecta/Cantidad de mazorcas`),
         `Tamaño de Colecta/Cantidad de mazorcas` = ifelse(`Tamaño de Colecta/Cantidad de mazorcas` == "Pocas", NA, `Tamaño de Colecta/Cantidad de mazorcas`),
         `Tamaño de Colecta/Cantidad de mazorcas` = ifelse(`Tamaño de Colecta/Cantidad de mazorcas` == "(20-50)", 35, `Tamaño de Colecta/Cantidad de mazorcas`),
         `Tamaño de Colecta/Cantidad de mazorcas` = ifelse(`Tamaño de Colecta/Cantidad de mazorcas` == "(20-50) 50", 35, `Tamaño de Colecta/Cantidad de mazorcas`),
         `Tamaño de Colecta/Cantidad de mazorcas` = ifelse(`Tamaño de Colecta/Cantidad de mazorcas` == "20-50", 35, `Tamaño de Colecta/Cantidad de mazorcas`),
         `Tamaño de Colecta/Cantidad de mazorcas` = ifelse(`Tamaño de Colecta/Cantidad de mazorcas` == "6 (3 macuernas)", 6, `Tamaño de Colecta/Cantidad de mazorcas`)) %>%
  mutate(`Tamaño de Colecta/Cantidad de grano o semilla` = str_remove(.$`Tamaño de Colecta/Cantidad de grano o semilla`, ",")) %>%
  mutate(EdadAgricultor_uniformizada = ifelse(EdadAgricultor_uniformizada == "ND", NA, EdadAgricultor_uniformizada)) %>%
  transmute(Id,
            IdEjemplar,
            SiglasColeccion = as.factor(SiglasColeccion),
            NombreColeccion = as.factor(NombreColeccion),
            NumeroDeCatalogo,
            Fuente,
            FechaColecta = as.POSIXct(paste(DiaColecta, MesColecta, AnioColecta,
                                            sep = "-"), format = "%d-%m-%Y", tz = "UTC-6"),
            PeriodoColecta,
            NumeroColecta = `NumeroDeColecta`,
            `DescripcionGpoColecta`,
            RazaPrimaria = `Raza primaria`,
            ComplejoRacial = `Complejo racial`,
            InfluenciaOtrasRazas = `Influencia de otras razas`,
            DiversidadMaiz = as.factor(`Diversidad del maíz`),
            AFavor = as.factor(`Características que le gustan de la variedad`),
            EnContra = as.factor(`Características que no le gustan de la variedad`),
            PorMejorar = as.factor(`Características que le interesaría mejorarle a la variedad`),
            Edafologia = as.factor(`Aspectos edáficos`),
            Topografia = as.factor(`Aspectos topográficos`),
            NombreComun = NomComun,
            Estado = as.factor(Estado),
            Municipio,
            Localidad,
            Longitud,
            Latitud,
            Altitud,
            ValidacionGeografica = as.factor(`ValidacionGeografica`),
            EdadAgricultor = as.numeric(`EdadAgricultor_uniformizada`),
            GpoIndigenaAgricultor = as.factor(`GrupoIndigenaAgricultor`),
            FuenteColecta = as.factor(`Fuente de Colecta`),
            SuperficieSembrada = as.numeric(`Superficie sembrada_uniformizada`),
            TamañoColectaGrano = `Tamaño de Colecta/Cantidad de grano o semilla`,
            TamañoColectaMazorca = as.numeric(`Tamaño de Colecta/Cantidad de mazorcas`),
            MezclaAñosCultivo = as.numeric(`Mezcla varietal/Años de cultivarla_uniformizada`),
            MezclaProcedencia = `Mezcla varietal/Procedencia`,
            VariedadAgricultorAñosCultivo = as.numeric(`Variedad del agricultor/Años de cultivarla_uniformizada`),
            VariedadAgricultorProcedencia = `Variedad del agricultor/Procedencia`,
            VariedadIntroducidaAñosCultivo = as.numeric(`Variedad introducida/Años de cultivarla`),
            VariedadIntroducidaProcedencia = `Variedad introducida/Procedencia`,
            VariedadMejoradaAñosCultivo = as.numeric(`Variedad mejorada/Años de cultivarla`),
            VariedadMejoradaProcedencia = `Variedad mejorada/Procedencia`,
            Usos = as.factor(`Usos`),
            UsosGrano = as.factor(`Usos del grano`),
            Destino = as.factor(`Destino`),
            EpocaSiembra1 = as.factor(`Época de siembra_uniformizada1`),
            EpocaSiembra2 = as.factor(`Época de siembra_uniformizada2`),
            EpocaFloracion = as.factor(`Época de floración`),
            EpocaMadurez = as.factor(`Época de madurez`),
            EpocaCosecha1 = as.factor(`Época de cosecha_uniformizada1`),
            EpocaCosecha2 = as.factor(`Época de cosecha_uniformizada2`),
            MetodoSiembra = as.factor(`Método de siembra_uniformizada`),
            SistemaCultivo = as.factor(`Sistema de Cultivo`),
            CultivosAsociados = as.factor(`Indicar cultivos asociados en el policultivo`),
            Rendimiento1 = as.numeric(`Rendimiento_uniformizada1`),
            Rendimiento2 = as.numeric(`Rendimiento_uniformizada2`),
            Insectos = as.factor(`Insectos dañinos observados`),
            Malezas = as.factor(`Malezas observadas`),
            ProblemasAlmacenamiento = as.factor(`Problemas durante el almacenamiento_uniformizada`),
            ControlMecanico = as.factor(`Control de plagas, malezas o enfermedades/Control mecánico`),
            ControlFungicidaBactericida = as.factor(`Control de enfermedades/Fungicidas o bactericidas`),
            ControlInsecticida = as.factor(`Control de plagas/Insecticidas`),
            ControlHerbicida = as.factor(`Control de malezas/Herbicidas`),
            Fertilizante = as.factor(`Fertilizante usado_uniformizada`),
            TipoRiego = as.factor(`Tipo de riego`),
            AlturaPlanta = as.numeric(`Altura de planta`),
            AlturaMazorca = as.numeric(`Altura de mazorca`),
            NumeroMazorcas = as.numeric(`Mazorcas por planta`),
            LongitudMazorca = as.numeric(`Longitud de mazorca`),
            DiametroMazorca = as.numeric(`Diametro de mazorca`),
            HilerasMazorca = as.numeric(`Hileras por mazorca`),
            RatioDiamLongMazorca = as.numeric(`Diámetro/longitud de la mazorca_recalculado`),
            GranosHilera = as.numeric(`Granos por hilera`),
            GranoAnchura = as.numeric(`Anchura de grano`),
            GranoGrosor = as.numeric(`Grosor de grano`),
            GranoLongitud = as.numeric(`Longitud de grano`),
            ContenidoHumedad = as.numeric(`Contenido de humedad`),
            Volumen100Granos = as.numeric(`Volumen de 100 granos`),
            Peso100Granos = as.numeric(`Peso seco 100 granos`),
            RatioAnchLongGrano = as.numeric(`Anchura/longitud del grano`),
            RatioGrosAnchGrano = as.numeric(`Grosor/anchura del grano_recalculado`)
  )

# columna ColoresGrano ----
nombreColoresGrano <- baseMaiz %>%
  select(Id, `Color de grano crema`:`Color de grano rosa`) %>%
  colnames() %>%
  str_remove_all("Color de grano ")

ColorGrano <- baseMaiz %>%
  select(Id, `Color de grano crema`:`Color de grano rosa`) %>%
  `colnames<-`(nombreColoresGrano) %>%
  mutate(across(.cols = c(crema:rosa), as.logical)) %>%
  mutate(crema = ifelse(crema, "crema", "ND"),
         `blanco puro (H)`= ifelse(` blanco puro (H)`, "blanco puro (H)", "ND"),
         `amarillo (B)`= ifelse(`amarillo (B)`, "amarillo (B)", "ND"),
         `morado (C)`= ifelse(`morado (C)`, "morado (C)", "ND"),
         `jaspeado (D)`= ifelse(`jaspeado (D)`, "jaspeado (D)", "ND"),
         `amarillo claro`= ifelse(`amarillo claro`, "amarillo claro", "ND"),
         `amarillo medio`= ifelse(`amarillo medio`, "amarillo medio", "ND"),
         `amarillo naranja (F)`= ifelse(`amarillo naranja (F)`, "amarillo naranja (F)", "ND"),
         `azul (K)`= ifelse(`azul (K)`, "azul (K)", "ND"),
         `azul oscuro (L)`= ifelse(`azul oscuro (L)`, "azul oscuro (L)", "ND"),
         `blanco (A)`= ifelse(`blanco (A)`, "blanco (A)", "ND"),
         `blanco cremoso`= ifelse(`blanco cremoso`, "blanco cremoso", "ND"),
         `café (E)`= ifelse(`café (E)`, "café (E)", "ND"),
         naranja= ifelse(naranja, "naranja", "ND"),
         `negro`= ifelse(`negro`, "negro", "ND"),
         `rojo (I)`= ifelse(`rojo (I)`, "rojo (I)", "ND"),
         `rojo naranja (J)`= ifelse(`rojo naranja (J)`, "rojo naranja (J)", "ND"),
         `rojo oscuro` = ifelse(`rojo oscuro`, "rojo oscuro", "ND"),
         rosa = ifelse(rosa, "rosa", "ND")) %>%
  transmute(Id,
            ColorGrano = paste(`crema`,`blanco puro (H)`,`amarillo (B)`,`morado (C)`,
                               `jaspeado (D)`,`amarillo claro`,`amarillo medio`,
                               `amarillo naranja (F)`,`azul (K)`,`azul oscuro (L)`,
                               `blanco (A)`,`blanco cremoso`,`café (E)`,`naranja`,
                               `negro`,`rojo (I)`,`rojo naranja (J)`,`rojo oscuro`,
                               `rosa`, sep = ","))

ColorGrano$ColorGrano <- str_remove_all(ColorGrano$ColorGrano, "ND,")
ColorGrano$ColorGrano <- str_remove_all(ColorGrano$ColorGrano, ",ND")

# columna ColorOlote ----
nombreColoresOlote <- baseMaiz %>%
  select(Id, `Color de olote amarillo claro`:`Color de olote rojo oscuro`) %>%
  colnames() %>%
  str_remove_all("Color de olote ")

ColorOlote <- baseMaiz %>%
  select(Id, `Color de olote amarillo claro`:`Color de olote rojo oscuro`) %>%
  `colnames<-`(nombreColoresOlote) %>%
  mutate(across(.cols = c(`amarillo claro`:`rojo oscuro`), as.logical)) %>%
  mutate(`amarillo claro` = ifelse(`amarillo claro`, "amarillo claro", "ND"),
         `amarillo medio` = ifelse(`amarillo medio`, "amarillo medio", "ND"),
         `amarillo naranja` = ifelse(`amarillo naranja`, "amarillo naranja", "ND"),
         `azul` = ifelse(`azul`, "azul", "ND"),
         `azul oscuro` = ifelse(`azul oscuro`, "azul oscuro", "ND"),
         `blanco` = ifelse(`blanco`, "blanco", "ND"),
         `blanco cremoso` = ifelse(`blanco cremoso`, "blanco cremoso", "ND"),
         `café` = ifelse(`café`, "café", "ND"),
         `naranja` = ifelse(`naranja`, "naranja", "ND"),
         `negro` = ifelse(`negro`, "negro", "ND"),
         `rojo` = ifelse(`rojo`, "rojo", "ND"),
         `rojo naranja` = ifelse(`rojo naranja`, "rojo naranja", "ND"),
         `rojo oscuro` = ifelse(`rojo oscuro`, "rojo oscuro", "ND")) %>%
  transmute(Id,
            ColorOlote = paste(`amarillo claro`,`amarillo medio`,`amarillo naranja`,
                               `azul`,`azul oscuro`,`blanco`,`blanco cremoso`,`café`,
                               `naranja`,`negro`,`rojo`,`rojo naranja`,`rojo oscuro`,
                               sep = ","))

ColorOlote$ColorOlote <- str_remove_all(ColorOlote$ColorOlote, "ND,")
ColorOlote$ColorOlote <- str_remove_all(ColorOlote$ColorOlote, ",ND")

# columna DisposicionHileras ----
nombreDisposicionHileras <- baseMaiz %>%
  select(Id, `Disposición de hileras en espiral`:`Disposición de hileras regular`) %>%
  colnames() %>%
  str_remove_all("Disposición de hileras ")

DisposicionHileras <- baseMaiz %>%
  select(Id, `Disposición de hileras en espiral`:`Disposición de hileras regular`) %>%
  `colnames<-`(nombreDisposicionHileras) %>%
  mutate(across(.cols = c(`en espiral`:regular), as.logical)) %>%
  mutate(`en espiral` = ifelse(`en espiral`, "en espiral", "ND"),
         irregular = ifelse(irregular, "irregular", "ND"),
         recta = ifelse(recta, "recta", "ND"),
         semirecta = ifelse(semirecta, "semirecta", "ND"),
         regular = ifelse(regular, "regular", "ND")) %>%
  transmute(Id,
            DisposicionHileras = paste(`en espiral`, irregular, recta, semirecta, regular,
                                       sep = ","))

DisposicionHileras$DisposicionHileras <- str_remove_all(DisposicionHileras$DisposicionHileras, "ND,")
DisposicionHileras$DisposicionHileras <- str_remove_all(DisposicionHileras$DisposicionHileras, ",ND")

# columna FormaMazorca ----
nombreFormaMazorca <- baseMaiz %>%
  select(Id, `Forma de mazorca cilíndrica`:`Forma de mazorca esférica`) %>%
  colnames() %>%
  str_remove_all("Forma de mazorca ")

FormaMazorca <- baseMaiz %>%
  select(Id, `Forma de mazorca cilíndrica`:`Forma de mazorca esférica`) %>%
  `colnames<-`(nombreFormaMazorca) %>%
  mutate(across(.cols = c(cilíndrica:esférica), as.logical)) %>%
  mutate(cilíndrica = ifelse(cilíndrica, "cilíndrica", "ND"),
         cónica = ifelse(cónica, "cónica", "ND"),
         `cónica-cilíndrica` = ifelse(`cónica-cilíndrica`, "cónica-cilíndrica", "ND"),
         esférica = ifelse(esférica, "esférica", "ND")) %>%
  transmute(Id,
            FormaMazorca = paste(cilíndrica, cónica, `cónica-cilíndrica`, esférica,
                                 sep = ","))

FormaMazorca$FormaMazorca <- str_remove_all(FormaMazorca$FormaMazorca, "ND,")
FormaMazorca$FormaMazorca <- str_remove_all(FormaMazorca$FormaMazorca, ",ND")

# columna TipoGrano ----
nombreTipoGrano <- baseMaiz %>%
  select(Id, `Tipo de grano ceroso`:`Tipo de grano semi-harinoso`) %>%
  colnames() %>%
  str_remove_all("Tipo de grano ")

TipoGrano <- baseMaiz %>%
  select(Id, `Tipo de grano ceroso`:`Tipo de grano semi-harinoso`) %>%
  `colnames<-`(nombreTipoGrano) %>%
  mutate(across(.cols = c(ceroso:`semi-harinoso`), as.logical)) %>%
  mutate(`ceroso` = ifelse(`ceroso`, "ceroso", "ND"),
         `cristalino (F)` = ifelse(`cristalino (F)`, "cristalino (F)", "ND"),
         `dentado (C)` = ifelse(`dentado  (C)`, "dentado (C)", "ND"),
         `dulce (H)` = ifelse(`dulce (H)`, "dulce (H)", "ND"),
         `harinoso (A)` = ifelse(`harinoso (A)`, "harinoso (A)", "ND"),
         `reventador (G)` = ifelse(`reventador (G)`, "reventador (G)", "ND"),
         `semi-cristalino (E)` = ifelse(`semi-cristalino (E)`, "semi-cristalino (E)", "ND"),
         `semi-dentado (D)` = ifelse(`semi-dentado (D)`, "semi-dentado (D)", "ND"),
         `semi-harinoso` = ifelse(`semi-harinoso`, "semi-harinoso", "ND")) %>%
  transmute(Id,
            TipoGrano = paste(`ceroso`,`cristalino (F)`,`dentado (C)`,`dulce (H)`,
                              `harinoso (A)`,`reventador (G)`,`semi-cristalino (E)`,
                              `semi-dentado (D)`,`semi-harinoso`,
                              sep = ","))

TipoGrano$TipoGrano <- str_remove_all(TipoGrano$TipoGrano, "ND,")
TipoGrano$TipoGrano <- str_remove_all(TipoGrano$TipoGrano, ",ND")

# integrando la información ----
bdMaiz <- bdMaiz %>%
  left_join(ColorGrano, by = "Id") %>%
  left_join(ColorOlote, by = "Id") %>%
  left_join(DisposicionHileras, by = "Id") %>%
  left_join(FormaMazorca, by = "Id") %>%
  left_join(TipoGrano, by = "Id")

# load the info
#usethis::use_data(bdMaiz, overwrite = TRUE)
#bdMaiz %>% write.csv("./R/Maices/bdMaiz.csv")
