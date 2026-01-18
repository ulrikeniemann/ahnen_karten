# librarys laden
packages <- c("tidyverse", "readxl", "leaflet", "leaflet.minicharts")
packages <- rev(packages)
for (package in packages) {
    if (!require(package, character.only = TRUE)) {
        install.packages(package, character.only = TRUE)
    }
    library(package, character.only = TRUE)
}
rm(packages, package)

# Daten laden
data <- read_excel("./data/AHNEN_2026.xlsx", sheet = "Karten")
data <- data |>
  rename(
    lat1 = `LAT GebOrt`,
    lon1 = `LON GebTag`,
    lat2 = `LAT Sterbe`,
    lon2 = `LON Sterbe`
  )
# leichtes Rauschen
data <- data |>
  mutate(
    lat1x = jitter(lat1, factor = 1),
    lon1x = jitter(lon1, factor = 1),
    lat2x = jitter(lat2, factor = 1),
    lon2x = jitter(lon2, factor = 1)
  )

################################################################################

library(ggplot2)
library(sf)
library(dplyr)

# Deutschland-Karte als Hintergrund (optional, z.B. mit rnaturalearth)
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("sf")
library(rnaturalearth)
germany <- ne_countries(scale = "medium", country = "Germany", returnclass = "sf")

dat <- data |>
  filter(!is.na(lon1) & !is.na(lon2) & `Entf. GebOrt - SterbeOrt` > 20)

ggplot() +
  geom_sf(data = germany, fill = "grey95") + 
  labs(title = "Geburts- und Sterbeorte") +
  geom_curve(
    data = dat,
    aes(
      x = lon1x, y = lat1x,
      xend = lon2x, yend = lat2x,
      color = Linie
    ),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    #color = "blue",
    curvature = 0.2,
    size = 0.7
  ) +
  # Legende für die Farben der Pfeile
  scale_color_manual(
    name = "Linie",
    values = c("Vaterlinie" = "blue", "Mutterlinie" = "red")
  ) +
  theme_minimal() +
  # alle gitternetzlinien und Achsen entfernen
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


  ##############################################################################

# Test: Größe der Bubble nach Anzahl Menschen
datGeb <- data |>
  filter(!is.na(lon1)) |>
  group_by(LINIE, lon1x, lat1x) |>
  summarise(count = n(), .groups = "drop") |> 
  mutate(size = (count / 4) + 2.75)

leaflet(datGeb) |>
  addTiles() |>
  addProviderTiles("OpenStreetMap.DE") |>
  addCircleMarkers(
    ~lon1x,
    ~lat1x,
    radius = ~ size,
    fillColor = ~ LINIE,
    color = c("red", "#469bf2"),
    fillOpacity = 0.8,
    weight = 1,
    popup = ~ count
  )



# ------------------------------------------------------------------------------

add_custom_layers <- function(map) {
  map |>
  addTiles() |>
  addProviderTiles("OpenStreetMap.DE") |>
  addTiles(
    urlTemplate = "https://sgx.geodatenzentrum.de/wmts_topplus_open/tile/1.0.0/web/default/WEBMERCATOR/{z}/{y}/{x}.png",
    attribution = '© dl-de/by-2-0',
    group = "TopPlusOpen.Color"
  ) |> 
    addProviderTiles("Esri.WorldImagery", group = "Satellit") |> 
  addProviderTiles("OpenTopoMap", group = "OpenTopoMap") |> 
  addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") |> 
    addTiles(
    urlTemplate = "http://tile.mtbmap.cz/mtbmap_tiles/{z}/{x}/{y}.png",
    attribution = '© dl-de/by-2-0',
    group = "MtbMap"
  ) |> 
    addProviderTiles("CartoDB.PositronNoLabels", group = "CartoDB.PositronNoLabels") |> 
    addProviderTiles("Stadia.StamenToner", group = "Stadia.StamenToner") |> 
   #
   addLayersControl(
    baseGroups = c("TopPlusOpen.Color", "OpenStreetMap", "Satellit", "OpenTopoMap", "Esri.WorldImagery", "MtbMap", "CartoDB.PositronNoLabels",
    "Stadia.StamenToner"),
    position = "topleft"
  )
}

leaflet() |> add_custom_layers()

# ------------------------------------------------------------------------------

addLegendCustom <- function(map, colors, labels, sizes = 10, borders = "black", opacities = 0.8) {
  color_circle <- mapply(function(color, size, border, opacity) {
    paste0(
      "<div style='display: inline-block; width:", size, "px; height:", size, "px; 
      border-radius: 50%; background:", color, "; border:1px solid ", border, 
      "; opacity:", opacity, ";'></div>"
    )
  }, colors, sizes, borders, opacities, SIMPLIFY = FALSE)
  
  map %>% addLegend(
    position = "bottomright",
    colors = rep("transparent", length(colors)),
    labels = paste0(unlist(color_circle), " ", labels),
    opacity = 1
  )
}

# Beispiel-Anwendung:
leaflet() %>%
  addTiles() %>%
  addLegendCustom(
    colors = c("blue", "red"),
    labels = c("Vaterlinie", "Mutterlinie"),
    sizes = c(16, 16),
    borders = c("black", "black")
  )

# ------------------------------------------------------------------------------

legend_html <- "
<div style='background: white; padding: 8px; border-radius: 6px; box-shadow: 0 1px 5px rgba(0,0,0,0.65);'>
  <span style='display: inline-block; width:16px; height:16px; border-radius:50%; background:#469bf2; border:1px solid black; opacity:0.8; vertical-align:middle;'></span>
  Vaterlinie<br>
  <span style='display: inline-block; width:16px; height:16px; border-radius:50%; background:red; border:1px solid black; opacity:0.8; vertical-align:middle;'></span>
  Mutterlinie
</div>
"

leaflet() %>%
  addTiles() %>%
  addControl(html = legend_html, position = "bottomright")



# ------------------------------------------------------------------------------

# Ensure GENERATION is numeric and drop NAs
datGeb <- datGeb |> mutate(GENERATION = as.numeric(GENERATION))
generationen <- sort(unique(na.omit(datGeb$GENERATION)))

blues_15 <- rev(colorRampPalette(brewer.pal(9, "Blues"))(15))

blues_pal <- colorBin(
  palette = blues_15,
  domain = generationen,
  bins = 15,
  pretty = FALSE
)

leaflet(datGeb) |>
  add_custom_layers() |> 
  addCircleMarkers(
    ~lon1,
    ~lat1,
    radius = 4,
    fillColor = ~ blues_pal(GENERATION),
    color = "black",
    fillOpacity = 0.8,
    weight = 1,
    popup = ~ str_c(VORNAME, " ", NAME)
  ) |>
  addLegend(
    position = "bottomright",
    pal = blues_pal,
    values = generationen,
    title = "Generation",
    opacity = 1,
    labFormat = function(type, cuts, p) as.character(01:14)
  )


# ------------------------------------------------------------------------------


library(leaflet)
library(leaflet.extras)

df <- data.frame(
  lon_from = c(13.4050, 11.5820),
  lat_from = c(52.5200, 48.1351),
  lon_to   = c(9.9937,  8.6821),
  lat_to   = c(53.5511, 50.1109)
)

leaflet() |>
  addTiles() |>
  addPolylines(
    lng = as.matrix(df[, c("lon_from", "lon_to")]),
    lat = as.matrix(df[, c("lat_from", "lat_to")]),
    color = "blue",
    weight = 3
  ) |>
  addArrowhead(
    lng = df$lon_to,
    lat = df$lat_to,
    color = "blue",
    fillOpacity = 1
  )

# ------------------------------------------------------------------------------

# Beispiel-Daten
df <- data.frame(
  lon_from = runif(20, 6, 14),
  lat_from = runif(20, 47, 54),
  lon_to   = runif(20, 6, 14),
  lat_to   = runif(20, 47, 54)
)

leaflet() |>
  addTiles() |>

  # Linien
  addPolylines(
    lng = as.matrix(df[, c("lon_from", "lon_to")]),
    lat = as.matrix(df[, c("lat_from", "lat_to")]),
    color = "#1f78b4",
    weight = 1,
    opacity = 0.8
  ) |>

  # Pfeilspitzen exakt am Ziel
  addArrowhead(
    lng = df$lon_to,
    lat = df$lat_to,
    color = "#1f78b4",
    fillColor = "#1f78b4",
    fillOpacity = 1,
    weight = 2
  )

# ------------------------------------------------------------------------------

arrow_triangle <- function(x0, y0, x1, y1, size = 0.15) {
  dx <- x1 - x0
  dy <- y1 - y0
  len <- sqrt(dx^2 + dy^2)

  if (len == 0) return(NULL)

  ux <- dx / len
  uy <- dy / len

  # senkrechte Richtung
  px <- -uy
  py <- ux

  data.frame(
    lng = c(
      x1,
      x1 - size * (ux + px),
      x1 - size * (ux - px),
      x1                # Polygon schließen
    ),
    lat = c(
      y1,
      y1 - size * (uy + py),
      y1 - size * (uy - py),
      y1
    )
  )
}

df <- data.frame(
  lon_from = runif(20, 6, 14),
  lat_from = runif(20, 47, 54),
  lon_to   = runif(20, 6, 14),
  lat_to   = runif(20, 47, 54)
)

m <- leaflet() |>
  addTiles() |>
  addPolylines(
    lng = as.matrix(df[, c("lon_from", "lon_to")]),
    lat = as.matrix(df[, c("lat_from", "lat_to")]),
    color = "#1f78b4",
    weight = 1,
    opacity = 0.8
  )

# Pfeilspitzen als Polygone
for (i in seq_len(nrow(df))) {
  tri <- arrow_triangle(
    df$lon_from[i], df$lat_from[i],
    df$lon_to[i],   df$lat_to[i],
    size = 0.2
  )

  m <- m |>
    addPolygons(
      lng = tri$lng,
      lat = tri$lat,
      fillColor = "#1f78b4",
      fillOpacity = 1,
      color = "#1f78b4",
      weight = 1
    )
}

m

# ------------------------------------------------------------------------------

library(htmlwidgets)

df <- data.frame(
  lon_from = c(13.4050, 11.5820),
  lat_from = c(52.5200, 48.1351),
  lon_to   = c(9.9937,  8.6821),
  lat_to   = c(53.5511, 50.1109)
)

m <- leaflet(df) |>
  addTiles() |>
  addPolylines(
    lng = ~c(lon_from, lon_to),
    lat = ~c(lat_from, lat_to),
    color = "#1f78b4",
    weight = 1
  )

js <- "
function(el, x) {
  var map = this;

  x.data.forEach(function(d) {
    var latlngs = [
      [d.lat_from, d.lon_from],
      [d.lat_to, d.lon_to]
    ];

    var polyline = L.polyline(latlngs, {
      color: '#1f78b4',
      weight: 4
    }).addTo(map);

    L.polylineDecorator(polyline, {
      patterns: [
        {
          offset: '100%',
          repeat: 0,
          symbol: L.Symbol.arrowHead({
            pixelSize: 50,
            polygon: true,
            pathOptions: {
              fillOpacity: 1,
              weight: 0,
              color: '#1f78b4',
              fillColor: '#1f78b4'
            }
          })
        }
      ]
    }).addTo(map);
  });
}
"

m |> onRender(js)

# ------------------------------------------------------------------------------

#install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata"))
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

df <- data.frame(
  lon_from = runif(20, 6, 14),
  lat_from = runif(20, 47, 54),
  lon_to   = runif(20, 6, 14),
  lat_to   = runif(20, 47, 54)
)


ggplot(df) +
  geom_segment(
    aes(
      x = lon_from, y = lat_from,
      xend = lon_to, yend = lat_to
    ),
    arrow = arrow(type = "closed", length = unit(4, "mm")),
    linewidth = 1,
    color = "blue"
  ) +
  coord_fixed()

