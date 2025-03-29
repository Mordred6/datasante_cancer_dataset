# Installer les packages nécessaires (si non installés)
install.packages("ggthemes")
install.packages("plotly")
install.packages(c("ggplot2", "leaflet", "dplyr", "readr", "rnaturalearth", "rnaturalearthdata"))

# Charger les bibliothèques
library(ggthemes)
library(plotly)
library(ggplot2)
library(leaflet)
library(dplyr)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)

# Charger les données
Dataset_world_death <- read.csv("/01 annual-number-of-deaths-by-cause.csv")

# Vérifier les dimensions et les valeurs manquantes
dim(Dataset_world_death)
missing_values <- colSums(is.na(Dataset_world_death))
print(missing_values)

# ---  GRAPHIQUE GGPlot : Évolution des décès par cancer dans le monde ---

# Filtrer les données pour le monde entier
world_data <- Dataset_world_death %>%
  filter(Entity == "World")

# Créer un graphique de l'évolution des décès par cancer
ggplot(data = world_data, mapping = aes(x = Year, y = Deaths...Neoplasms...Sex..Both...Age..All.Ages..Number.)) +
  geom_line(color = "blue", size = 1) + 
  labs(title = "Évolution des décès du cancer dans le monde",
       x = "Année",
       y = "Nombre de décès du cancer") +
  theme_minimal()

# ---  CARTE INTERACTIVE LEAFLET ---

# Filtrer les données pour l'année 2019
data_2019 <- Dataset_world_death %>%
  filter(Year == 2019) %>%
  select(Code, Deaths...Neoplasms...Sex..Both...Age..All.Ages..Number.) %>%
  rename(iso_a3_eh = Code, deaths = Deaths...Neoplasms...Sex..Both...Age..All.Ages..Number.)

# Charger les données géographiques
df_world <- ne_countries(scale = "medium", returnclass = "sf")

# Fusionner les données avec les données géographiques
map_data <- left_join(df_world, data_2019, by = "iso_a3_eh")

# Définir les intervalles de la légende
breaks <- c(0, 10000, 50000, 100000, 200000, 500000, Inf)

# Créer une palette de couleurs pour les catégories définies
palette_deaths <- colorBin("YlOrRd", domain = map_data$deaths, bins = breaks, na.color = "transparent")

# Créer la carte interactive avec la légende en nombres absolus
leaflet(map_data) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palette_deaths(deaths),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    popup = ~paste0("Pays: ", name, "<br>Décès: ", formatC(deaths, big.mark = " ", format = "f", digits = 0))
  ) %>%
  addLegend(
    "bottomright", 
    pal = palette_deaths, 
    values = ~deaths,
    title = "Nombre de décès",
    opacity = 0.7,
    labFormat = function(type, cuts, p) {
      paste0(prettyNum(cuts[-length(cuts)], big.mark = " "), " - ", prettyNum(cuts[-1], big.mark = " "))
    }
  )
