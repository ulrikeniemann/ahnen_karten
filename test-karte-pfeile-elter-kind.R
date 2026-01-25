# Test: Karte Pfeile Geburtsort Vater-Kind und Mutter-Kind

# Daten V1 laden (V2 fehlt die Kind-Spalte)
datV1 <- read_excel("./data/AHNEN_2026.xlsx", sheet = "Karten")

datV1 <- datV1 |>
  filter(!is.na(`LAT GebOrt`)) |> # nur Personen mit Geburtsort
  mutate(Kekule = as.integer(Kekule)) |> # Kekule als Integer
  select(Kekule, Vorname, Name, `LAT GebOrt`, `LON GebTag`, KIND)

# Geburtsorte Kinder anspielen
tempKinder <- datV1 |>
  filter(! (Kekule == 1 & Vorname == "Juliane")) |> # jede Kekule nur einmal
  select(KIND = Kekule, Vorname_Kind = Vorname, Name_Kind = Name, 
    lat_kind = `LAT GebOrt`, lon_kind = `LON GebTag`)

# Kinder joinen
datV1_kinder <- datV1 |>
  left_join(tempKinder, by = "KIND")

# berechne die Distanz zwischen Geburtsort Eltern - Kind in km
datV1_kinder <- datV1_kinder |>
  mutate(
    `Entfernung Geburt Eltern-Kind` = geosphere::distHaversine(
      # matrix(c(`LAT GebOrt`, lat_kind), ncol = 2),
      # matrix(c(`LON GebTag`, lon_kind), ncol = 2)
      cbind(`LON GebTag`, `LAT GebOrt`),
      cbind(lon_kind, lat_kind)
    ) / 1000
  )

# nicht die kurzen Entfernungen darstellen
datV1_kinder <- datV1_kinder |>
  filter(`Entfernung Geburt Eltern-Kind` > 5) |> 
  rename(
    lat1 = `LAT GebOrt`, lon1 = `LON GebTag`,
    lat2 = lat_kind, lon2 = lon_kind
  )

# Karte
leaflet(datV1_kinder) |>
  addTiles() |>
  addFlows(
    lng0 = datV1_kinder$lon1, lat0 = datV1_kinder$lat1, # Startpunkte
    lng1 = datV1_kinder$lon2, lat1 = datV1_kinder$lat2, # Endpunkte
    color = col,
    flow = 1,              # Bestimmt die Richtung (1 = Start zu Ende)
    minThickness = 2, 
    maxThickness = 2       # Konstante Dicke
  )
