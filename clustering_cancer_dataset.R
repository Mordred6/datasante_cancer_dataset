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

# Clustering des pays selon le taux de mortalité
data_cancer_mortality_rate_peryear <- Dataset_cancer_mortality %>%
  select(Entity, Year, Cancer_Mortality_Rate) %>%
  spread(key = Year, value = Cancer_Mortality_Rate, fill = 0)

rownames(data_cancer_mortality_rate_peryear) <- data_cancer_mortality_rate_peryear$Entity
data_cancer_mortality_rate_peryear <- data_cancer_mortality_rate_peryear %>%
  select(-Entity)

data_cancer_mortality_rate_peryear_scaled <- scale(data_cancer_mortality_rate_peryear)

# Déterminer le nombre optimal de clusters
fviz_nbclust(data_cancer_mortality_rate_peryear_scaled, kmeans, method = "wss") + 
  labs(title = "Méthode du coude pour déterminer le meilleur k")

# Appliquer K-Means
set.seed(123)
k <- 3  # Modifier selon le résultat
kmeans_model <- kmeans(data_cancer_mortality_rate_peryear_scaled, centers = k, nstart = 25)
data_cancer_mortality_rate_peryear$Cluster <- as.factor(kmeans_model$cluster)

# Visualisation PCA
pca_model <- prcomp(data_cancer_mortality_rate_peryear_scaled)
fviz_pca_ind(pca_model,
             geom.ind = "point",
             col.ind = data_cancer_mortality_rate_peryear$Cluster,
             palette = "jco",
             addEllipses = TRUE,
             legend.title = "Cluster",
             title = "PCA - K-Means Clustering")

# Afficher la répartition des clusters
print(table(data_cancer_mortality_rate_peryear$Cluster))

# Afficher les pays dans chaque cluster
for (i in unique(data_cancer_mortality_rate_peryear$Cluster)) {
  cat("\n Pays dans le Cluster", i, ":\n")
  print(rownames(data_cancer_mortality_rate_peryear[data_cancer_mortality_rate_peryear$Cluster == i, ]))
}

