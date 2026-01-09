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

