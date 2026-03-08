# ..............................................................................

library(tidyverse)
library(readxl)
library(leaflet)
library(leaflet.minicharts)

# Karten Vater / Mutter Linien

# Daten laden
dataVater <- read_excel("./data/Karten Wanderung.xlsx",
                       sheet = "Vater")
dataMutter <- read_excel("./data/Karten Wanderung.xlsx",
                       sheet = "Mutter")

# parse numbers
dataVater <- dataVater |> 
  mutate(
    LAT_GEBURT = LAT_GEBURT |> parse_number(),
    LON_GEBURT = LON_GEBURT |> parse_number(),
    LAT_GEBURT_KIND = LAT_GEBURT_KIND |> parse_number(),
    LON_GEBURT_KIND = LON_GEBURT_KIND |> parse_number()
    )
dataMutter <- dataMutter |> 
  mutate(
    LAT_GEBURT = LAT_GEBURT |> parse_number(),
    LON_GEBURT = LON_GEBURT |> parse_number(),
    LAT_GEBURT_KIND = LAT_GEBURT_KIND |> parse_number(),
    LON_GEBURT_KIND = LON_GEBURT_KIND |> parse_number()
    )


# ..............................................................................
# Vater
#data <- dataVater
#col <- "blue"
# create popup strings and convert to a list (one element per row)

drawMapV1 <- function(data, col) {
  leaflet(data) |>
    addTiles() |>
    addFlows(
      lng0 = data$LON_GEBURT, lat0 = data$LAT_GEBURT, # Startpunkte
      lng1 = data$LON_GEBURT_KIND, lat1 = data$LAT_GEBURT_KIND, # Endpunkte
      color = col,
      flow = 1,              # Bestimmt die Richtung (1 = Start zu Ende)
      minThickness = 1, 
      maxThickness = 1,       # Konstante Dicke,
      popup = popupArgs(labels = 
        as.list(paste("Eltern: ", data$VORNAME, data$NACHNAME)))
    )
  # V1: mit Pfeilspitzen
  leaflet(data) |>
    addTiles() |>
    addFlows(
      lng0 = data$LON_GEBURT, lat0 = data$LAT_GEBURT, # Startpunkte
      lng1 = data$LON_GEBURT_KIND, lat1 = data$LAT_GEBURT_KIND, # Endpunkte
      color = col,
      flow = 1,              # Bestimmt die Richtung (1 = Start zu Ende)
      minThickness = 1, 
      maxThickness = 1,       # Konstante Dicke,
      popup = popupArgs(labels = 
        as.list(paste("Eltern: ", data$VORNAME, data$NACHNAME)))
    )
}
drawMapV2 <- function(data, col) {
  # V2: mit Bubbles am Ende
  map <- leaflet(data) |>
    addTiles() |> 
        addFlows(
      lng0 = data$LON_GEBURT, lat0 = data$LAT_GEBURT, # Startpunkte
      lng1 = data$LON_GEBURT_KIND, lat1 = data$LAT_GEBURT_KIND, # Endpunkte
      color = col,
      flow = 1,              # Bestimmt die Richtung (1 = Start zu Ende)
      minThickness = 1, 
      maxThickness = 1,       # Konstante Dicke,
      popup = popupArgs(labels = 
        as.list(paste("Eltern: ", data$VORNAME, data$NACHNAME)))
    )

  for (i in seq_len(nrow(data))) {
    map <- 
    addCircleMarkers(
      map,
      lng = data$LON_GEBURT_KIND[i],
      lat = data$LAT_GEBURT_KIND[i],
      radius = 4,
      fillColor = ~ col,
      color = col,
      fillOpacity = 0.5,
      weight = 1,
      popup = ~ str_c("Eltern: ", data$VORNAME[i], " ", data$NACHNAME[i]))
  }
  map
}
drawMapV1(dataVater, "blue")
drawMapV2(dataVater, "blue")

drawMapV1(dataMutter, "red")
drawMapV2(dataMutter, "red")






