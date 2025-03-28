# Installer et charger les librairies nécessaires
install.packages(c("ggthemes", "plotly", "leaflet", "dplyr", "readr", "rnaturalearth", "rnaturalearthdata", "sf", "cluster", "factoextra", "tidyr"))

library(ggthemes)
library(plotly)
library(leaflet)
library(dplyr)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(cluster)
library(factoextra)
library(tidyr)

# Charger les données principales
dataset_world_death <- read_csv("C:/!Etudes/A2/DataSanté/DatasetCancer/01 annual-number-of-deaths-by-cause.csv")

# Charger une liste des pays reconnus
countries <- read_csv("https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv") %>%
  select(`ISO3166-1-Alpha-3`) %>%
  rename(Code = `ISO3166-1-Alpha-3`)

# Filtrer uniquement les pays reconnus
dataset_world_death_filtered <- dataset_world_death %>%
  filter(Code %in% countries$Code) %>%
  select(Entity, Code, Year, `Deaths - Neoplasms - Sex: Both - Age: All Ages (Number)`)

# Charger les données de population
population <- read_csv("https://raw.githubusercontent.com/datasets/population/master/data/population.csv") %>%
  rename(Year = Year, Code = `Country Code`, Population = Value)

# Joindre les données de population pour calculer le taux de mortalité
dataset_world_cancer_death_rate <- dataset_world_death_filtered %>%
  left_join(population, by = c("Code", "Year")) %>%
  mutate(Cancer_Mortality_Rate = `Deaths - Neoplasms - Sex: Both - Age: All Ages (Number)` / Population * 100) %>%
  select(Entity, Code, Year, Cancer_Mortality_Rate)

# Sauvegarder le nouveau dataset
write_csv(dataset_world_cancer_death_rate, "dataset_world_cancer_death_rate.csv")

# Charger le nouveau dataset
Dataset_cancer_mortality <- read.csv("dataset_world_cancer_death_rate.csv")

# Vérification des dimensions et valeurs manquantes
dim(Dataset_cancer_mortality)
missing_values <- colSums(is.na(Dataset_cancer_mortality))
print(missing_values)

# Filtrer les statistiques mondiales
world_data <- Dataset_cancer_mortality %>%
  filter(Entity == "World")

# Filtrer les données pour l'année 2019
cancer_2019 <- Dataset_cancer_mortality %>%
  filter(Year == 2019 & !Entity %in% c("World", "G20", "European Union", "High-income countries")) %>%
  select(Entity, Code, Cancer_Mortality_Rate)

# Charger les coordonnées des pays
world <- ne_countries(scale = "medium", returnclass = "sf")

# Joindre les données avec la carte des pays
world_data_map <- world %>%
  left_join(cancer_2019, by = c("iso_a3_eh" = "Code"))

# Palette de couleurs pour la carte
pal <- colorBin(
  palette = "YlOrRd",  # Choix de la palette de couleurs
  domain = world_data_map$Cancer_Mortality_Rate,  # Données à afficher
  bins = c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, Inf),  # Définition des plages de taux
  na.color = "gray"  # Couleur pour les valeurs manquantes
)

# Générer la carte interactive
leaflet(world_data_map) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(Cancer_Mortality_Rate),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    label = ~paste(name, ":", round(Cancer_Mortality_Rate, 2), "% de mortalité"),
    highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~Cancer_Mortality_Rate,
    title = "Taux de mortalité du cancer (2019) (%)",
    opacity = 1
  )

