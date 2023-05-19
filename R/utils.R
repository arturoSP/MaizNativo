# read database ----
baseMaiz <- rio::import("./data/BaseMaicesNativos.xlsx", format = "xlsx", sheet = 1, readxl = F)

str(baseMaiz)
class(baseMaiz)
summary(baseMaiz)


# F_map ----
# prepare the data that will be mapped
F_map <- function(Samp, colSite, colYear, tLevel){
  map1 <- Samp |>
    group_by(Samp[[colSite]], Samp[[colYear]]) |>
    summarise(LAT = mean(LAT_DEGREES, na.rm = T),
              LNG = mean(LON_DEGREES, na.rm = T)) |>
    ungroup() |>
    mutate(`Samp[[colYear]]` = as.character(`Samp[[colYear]]`)) |>
    left_join(tLevel, by = c(`Samp[[colSite]]` = "Site", `Samp[[colYear]]` = "Year")) |>
    transmute(Site = `Samp[[colSite]]`,
              Year = `Samp[[colYear]]`,
              LAT, LNG, Level,
              Label = paste0("Site: ", Site, ", ", Year, "\nLevel: ", Level)) |>
    arrange(Site, desc(Year))

  return(map1)
}

# get colors for the map markers ----
getColor <- function(tLevel){
  tLevel <- tLevel |>
    mutate(Level = str_sub(Level, 1,1),
           Level = ifelse(Level == "N", 6, Level),
           Level = ifelse(is.na(Level), 6, Level),
           Level = as.numeric(Level))
  sapply(tLevel$Level, function(Level) {
    if(Level == 1) {
      "blue"
    } else if(Level == 2) {
      "lightblue"
    } else if(Level == 3) {
      "lightgreen"
    } else if(Level == 4) {
      "beige"
    } else if(Level == 5) {
      "orange"
    } else {
      "red"
    }
  })
}

# design the map ----
F_MapParam <- function(coord) {
  icons <- awesomeIcons(
    icon = "ios-information",
    iconColor = "black",
    library = "ion",
    markerColor = getColor(coord)
  )

  baseMaiz |>
    dplyr::filter(!is.na(Latitud) | !is.na(Longitud)) |>
    dplyr::filter(Raza.primaria != "ND") |>
    leaflet() |>
    addProviderTiles(providers$Stamen.TerrainBackground) |>
    addCircleMarkers(lat = baseMaiz$Latitud, lng = baseMaiz$Longitud, radius = 5,
                     popup = baseMaiz$Raza.primaria,
                     color = "green")#,
               clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = T))

  map1 <- coord |>
    leaflet() |>
    addProviderTiles(providers$Esri.OceanBasemap) |>
    addAwesomeMarkers(lat = coord$LAT, lng = coord$LNG,
                      icon = icons,
                      popup = coord$Label,
                      clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = T),
                      clusterId = "transectCluster")
  return(map1)
}
