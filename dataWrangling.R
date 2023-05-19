# EDA ---

library(tidyverse)

baseMaiz <- rio::import("./data/BaseMaicesNativos.xlsx", format = "xlsx", sheet = 1, readxl = F)

str(baseMaiz)
class(baseMaiz)
summary(baseMaiz)

head(baseMaiz)
head(baseMaiz %>%
  transmute(Id,
            IdEjemplar,
            SiglasColeccion = as.factor(SiglasColeccion),
            NombreColeccion = as.factor(NombreColeccion),
            NumeroDeCatalogo,
            Fuente,
            FechaColecta = as.POSIXct(paste(DiaColecta, MesColecta, AnioColecta,
                                         sep = "-"), format = "%d-%m-%Y", tz = "UTC-6"),
            PeriodoColecta,
            NumeroColecta = NumeroDeColecta,
            DescripcionGpoColecta,
            RazaPrimaria = Raza.primaria,
            ComplejoRacial = Complejo.racial,
            InfluenciaOtrasRazas = Influencia.de.otras.razas,
            NombreComun = NomComun,
            Estado = as.factor(Estado),
            Municipio,
            Localidad,
            ValidacionGeografica = as.factor(ValidacionGeografica),
            EdadAgricultor = as.numeric(EdadAgricultor_uniformizada),
            GpoIndigenaAgricultor = as.factor(GrupoIndigenaAgricultor),
            FuenteColecta = as.factor(Fuente.de.Colecta),
            SuperficieSembrada = as.numeric(Superficie.sembrada_uniformizada),
            TamañoColectaGrano = as.numeric(`Tamaño.de.Colecta/Cantidad.de.grano.o.semilla`),
            TamañoColectaMazorca = as.numeric(`Tamaño.de.Colecta/Cantidad.de.mazorcas`),
            MezclaAñosCultivo = as.numeric(`Mezcla.varietal/Años.de.cultivarla_uniformizada`),
            MezclaProcedencia = `Mezcla.varietal/Procedencia`,
            VariedadAgricultorAñosCultivo = as.numeric(`Variedad.del.agricultor/Años.de.cultivarla_uniformizada`),
            VariedadAgricultorProcedencia = `Variedad.del.agricultor/Procedencia`,
            VariedadIntroducidaAñosCultivo = as.numeric(`Variedad.introducida/Años.de.cultivarla`),
            VariedadIntroducidaProcedencia = `Variedad.introducida/Procedencia`,
            VariedadMejoradaAñosCultivo = as.numeric(`Variedad.mejorada/Años.de.cultivarla`),
            VariedadMejoradaProcedencia = `Variedad.mejorada/Procedencia`,
            Usos = as.factor(Usos),
            UsosGrano = as.factor(Usos.del.grano),
            Destino = as.factor(Destino),
            EpocaSiembra1 = as.factor(Época.de.siembra_uniformizada1),
            EpocaSiembra2 = as.factor(Época.de.siembra_uniformizada2),
            EpocaFloracion = as.factor(Época.de.floración),
            EpocaMadurez = as.factor(Época.de.madurez),
            EpocaCosecha1 = as.factor(Época.de.cosecha_uniformizada1)

            )
)
str(baseMaiz)
summary(baseMaiz$Época.de.siembra_uniformizada2)
