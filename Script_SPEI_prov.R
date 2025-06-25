### Fichier SPEI recup sur SPEI Global ### 
# Je veux merge les 23 fichiers SPEI des provenances # 

#### Packages ####
library(dplyr)
library(readr)
library (lubridate)
library(ggplot2)
library(plotly)
library(httr)
library(ncdf4)
library(readxl)

# Dossier des fichiers SPEI 
folder_path <- "/Users/david/Desktop/SPEI_prov"

# Liste des fichiers .csv
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Lire tous les fichiers et ajouter leur nom comme colonne "provenance"
compiled_df <- lapply(file_list, function(file) {
  provenance <- tools::file_path_sans_ext(basename(file))  # extraire nom de fichier sans extension
  df <- read_delim(file, delim = ";", col_names = TRUE, col_types = cols())  # lecture générique
  df$provenance <- provenance  # ajouter la variable provenance
  return(df)
}) %>% bind_rows()

# Afficher les premières lignes pour vérifier
head(compiled_df)

# Sauvegarder le fichier final dans mon working directory
write_csv(compiled_df, file.path("SPEI_merged.csv"))

# import de SPEI_merged 
SPEI_merged <- read.csv("~/Desktop/R & Git/GFF_SPEI/raw_data/SPEI_merged.csv", sep=",")
#View(SPEI_merged)
str(SPEI_merged)

#### Mise en forme JDD ####
SPEI_merged$dates <- as.Date(SPEI_merged$dates, format="%Y-%m-%d")  # Convertir la colonne 'dates' en format Date (avec jour, mois et année)
SPEI_merged$dates <- format(SPEI_merged$dates, "%Y-%m") # Extraire uniquement le mois et l'année (et en garder le format Date)
SPEI_merged$prov <- as.factor(SPEI_merged$prov) # Convertir 'prov' en facteur
SPEI_merged$provenance <- NULL
str(SPEI_merged) # Vérifier les modifications

# Je filtre les données pour les années de 1960 à 1990
SPEI_filtered <- SPEI_merged %>%
  filter(year(as.Date(paste0(SPEI_merged$dates, "-01"), format="%Y-%m-%d")) >= 1960 & 
           year(as.Date(paste0(SPEI_merged$dates, "-01"), format="%Y-%m-%d")) <= 1990)

write_csv(SPEI_filtered, file.path("SPEI_filtered"))


# Garder uniquement les lignes correspondant au mois de juillet
SPEI_filtered_july <- SPEI_filtered %>%
  filter(substr(dates, 6, 7) == "07")  # <- on ignore complètement l'année ici

# Calculer la moyenne globale de spei01 pour juillet, par province
moyennes_juillet <- SPEI_filtered_july %>%
  group_by(prov) %>%
  summarise(moyenne_spei_juillet = mean(spei01, na.rm = TRUE))

# Afficher les résultats
print(moyennes_juillet)


  write_csv(SPEI_juillet, file.path("SPEI_juillet"))
  
  # Filtrer les lignes pour ne garder que la période de végétation (avril à septembre)
  SPEI_veg <- SPEI_filtered %>%
    filter(format(dates, "%m") %in% c("04", "05", "06", "07", "08", "09"))
  
  write_csv(SPEI_veg, file.path("SPEI_veg"))
  

#### Visualisation SPEI ####

levels(SPEI_filtered$prov) # je vérifie la cohérence de mon jdd et que j'ai bien mes 23 prov
table(SPEI_filtered$prov) # # je vérifie le nombre d'occurrences de chaque provenance 371 -> 31 années (1960 à 1990) x 12 

# Calculer la moyenne du SPEI sur la période par provenance et trier par moyenne croissante
moyennes_spei <- SPEI_filtered %>%
  group_by(prov) %>%
  summarise(moyenne_spei = mean(spei01, na.rm = TRUE)) %>%
  arrange(moyenne_spei)

write_csv(moyennes_spei, file.path("mean_spei_1960_1990"))

#### Plot évolution SPEI ####
# Convertir la colonne 'dates' en format Date
SPEI_filtered$dates <- as.Date(paste0(SPEI_filtered$dates, "-01"), format="%Y-%m-%d")

# Extraire l'année de la colonne 'dates' et l'ajouter à une nouvelle colonne 'year'
SPEI_filtered$year <- format(SPEI_filtered$dates, "%Y")

# Calculer la moyenne du SPEI par année et par provenance
moyennes_annuelles <- SPEI_filtered %>%
  group_by(prov, year) %>%
  summarise(moyenne_spei = mean(spei01, na.rm = TRUE)) %>%
  arrange(prov, year)

# Tracer la moyenne du SPEI par année pour chaque provenance
ggplot(moyennes_annuelles, aes(x = as.numeric(year), y = moyenne_spei, color = prov)) +
  geom_line() +       # Ajouter la ligne pour l'évolution de la moyenne
  geom_point() +      # Ajouter les points pour la moyenne par année
  labs(title = "Évolution annuelle moyenne du SPEI pour chaque provenance (1960-1990)",
       x = "Année", y = "Moyenne SPEI") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(moyennes_annuelles$year), 
                                  max(moyennes_annuelles$year), by = 5)) +  # Ajuster les années
  theme(legend.position = "bottom")  # Positionner la légende en bas

### Données World clim

library(readxl)
library(raster)
library(geodata)
library(sp)
library(sf)
library(tidyverse)
library(terra)
library(dplyr)
library(ncdf4)



# Import jdd avec données pour GPS des prov 

Data_clim_prov <- read_excel("~/Desktop/Régis - data stage Charlotte/Data_clim_prov.xlsx", 
                             col_types = c("text", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "text", "text", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric"))

str(Data_clim_prov)

# je charge les points GPS de mes provenances
points <- vect(
  x = Data_clim_prov,
  geom = c("Lon_WGS84", "Lat_WGS84"),
  crs = "EPSG:4326"
)


# Téléchargement et extraction de toutes les variables

# Précipitations
prec <- worldclim_global(var = "prec", res = 10, path = "data/")
prec_vals <- extract(prec, points)[, -1]  # on enlève la colonne ID
colnames(prec_vals) <- paste0("prec_", month.abb)

# Température moyenne
tavg <- worldclim_global(var = "tavg", res = 10, path = "data/")
tavg_vals <- extract(tavg, points)[, -1]
colnames(tavg_vals) <- paste0("tavg_", month.abb)

# Température minimale
tmin <- worldclim_global(var = "tmin", res = 10, path = "data/")
tmin_vals <- extract(tmin, points)[, -1]
colnames(tmin_vals) <- paste0("tmin_", month.abb)

# Température maximale
tmax <- worldclim_global(var = "tmax", res = 10, path = "data/")
tmax_vals <- extract(tmax, points)[, -1]
colnames(tmax_vals) <- paste0("tmax_", month.abb)

# Radiation solaire
srad <- worldclim_global(var = "srad", res = 10, path = "data/")
srad_vals <- extract(srad, points)[, -1]
colnames(srad_vals) <- paste0("srad_", month.abb)

# Vitesse du vent
wind <- worldclim_global(var = "wind", res = 10, path = "data/")
wind_vals <- extract(wind, points)[, -1]
colnames(wind_vals) <- paste0("wind_", month.abb)

# Pression de vapeur d'eau
vapr <- worldclim_global(var = "vapr", res = 10, path = "data/")
vapr_vals <- extract(vapr, points)[, -1]
colnames(vapr_vals) <- paste0("vapr_", month.abb)


# je fusionne toutes les données dans un seul tableau
climate_all <- cbind(
  Data_clim_prov["id_prov"],
  prec_vals,
  tavg_vals,
  tmin_vals,
  tmax_vals,
  srad_vals,
  wind_vals,
  vapr_vals
)

str(climate_all)

#### Test avec Terraclimate ########@

# Spécifie les coordonnées de longitude et latitude (exemple : Paris)
x <- c(2.3490, 48.8647)  # Longitude, Latitude de Paris
var <- "aet"

# URL de base pour télécharger les données agrégées
baseurlagg <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_", var, "_1958_CurrentYear_GLOBE.nc")

# Ouvrir le fichier NetCDF
nc <- nc_open(baseurlagg)

# Extraire les variables de latitude et longitude
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

# Trouver l'index de la latitude et de la longitude les plus proches
flat <- match(abs(lat - x[2]) < 1/48, 1)  # La latitude cible est x[2]
latindex <- which(flat %in% 1)

flon <- match(abs(lon - x[1]) < 1/48, 1)  # La longitude cible est x[1]
lonindex <- which(flon %in% 1)

# Définir les indices pour récupérer les données à partir des coordonnées cibles
start <- c(lonindex, latindex, 1)
count <- c(1, 1, -1)

# Lire les données pour la période complète
data <- as.numeric(ncvar_get(nc, varid = var, start = start, count = count))

# Afficher les données extraites
print(data)

# Créer les vecteurs de mois et d’années
mois <- rep(1:12, times = length(data) / 12)
annees <- rep(1958:(1958 + (length(data)/12) - 1), each = 12)

# Créer dynamiquement le data frame avec le nom de la variable
df <- data.frame(
  Annee = annees,
  Mois = mois
)
df[[var]] <- data  # Ajout de la variable avec son vrai nom

# Afficher les premières lignes
head(df)


#### test pour recup toutes les VA sur terra climate ###############
# Installer les packages nécessaires si besoin

Data_clim_prov <- read_excel("~/Desktop/Régis - data stage Charlotte/Data_clim_prov.xlsx", 
                             col_types = c("text", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "text", "text", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric"))

str(Data_clim_prov)
# Je crée un fichier avec localisation GPS de toutes mes provenances
coords <- data.frame(
  Provenance = Data_clim_prov$id_prov,
  Lon = Data_clim_prov$Lon_WGS84,
  Lat = Data_clim_prov$Lat_WGS84
)

vars <- c("aet", "def", "pet", "ppt", "q", "soil", "srad", "swe",
          "tmax", "tmin", "tmean", "vap", "vpd", "ws")

nb_years <- 2024 - 1958 + 1
mois <- rep(1:12, times = nb_years)
annees <- rep(1958:2024, each = 12)

# Itération sur chaque provenance
for (i in 1:nrow(coords)) {
  x <- coords[i, c("Lon", "Lat")]
  prov_name <- coords$Provenance[i]
  
  # DataFrame spécifique pour chaque provenance
  df <- data.frame(Provenance = prov_name, Annee = annees, Mois = mois)
  
  # Pour chaque variable
  for (var in vars) {
    message(paste("Téléchargement de :", var, "pour", prov_name))
    
    url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",
                  var, "_1958_CurrentYear_GLOBE.nc")
    
    tryCatch({
      nc <- nc_open(url)
      
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
      
      latindex <- which.min(abs(lat - x["Lat"]))
      lonindex <- which.min(abs(lon - x["Lon"]))
      
      start <- c(lonindex, latindex, 1)
      count <- c(1, 1, -1)
      
      data <- as.numeric(ncvar_get(nc, varid = var, start = start, count = count))
      
      nc_close(nc)
      
      df[[var]] <- data
    }, error = function(e) {
      message(paste("⚠️ Erreur pour", var, ":", e$message))
      df[[var]] <- NA
    })
  }
  
  # Sauvegarde des données pour chaque provenance dans un fichier CSV
  file_name <- paste0("data_", prov_name, "_climatiques.csv")
  write.csv(df, file_name, row.names = FALSE)
  
  message(paste("Fichier créé pour la provenance :", prov_name))
}

#### Test Débug -----

# Charger les packages nécessaires
library(ncdf4)
library(dplyr)

# Coordonnées de BIVG (à adapter si besoin)
prov_name <- "BIVG"
lat_BIVG <- 46.53   # latitude de BIVG
lon_BIVG <- 3.05    # longitude de BIVG

# Variables à extraire (selon TerraClimate)
vars <- c("aet", "def", "pet", "ppt", "q", "soil", "srad", "swe",
          "tmax", "tmin", "tmean", "vap", "vpd", "ws")

# Créer vecteurs année/mois
nb_years <- 2024 - 1958 + 1
mois <- rep(1:12, times = nb_years)
annees <- rep(1958:2024, each = 12)

# Initialiser le DataFrame
df <- data.frame(Provenance = prov_name, Annee = annees, Mois = mois)

# Boucle sur chaque variable
library(ncdf4)
library(dplyr)

prov_data <- data.frame(
  id_prov = c("BIVG", "BOLE", "BOR", "DALJ", "DEBO", "GUZN", "HAGN", "KUBR",
              "LIGO", "LIPO", "LISE", "MLML", "ORLE", "PORN", "RUS", "RYCH",
              "SDVG", "SLEN", "SLOB", "SMAR", "SPIT", "VGMO", "ZELA"),
  Lat_WGS84 = c(46.5333333333, 52.4, 49.4166666667, 51.0833333333, 52, 53.67,
                48.85, 51.4166666667, 50.58, 53.8333333333, 51.3333333333, 53.5666666667,
                47.9666666667, 47.33, 53.8333333333, 51.1333333333, 48.1833333333,
                55.75, 52.67, 51.1666666667, 48.6, 48.7166666667, 50.33),
  Lon_WGS84 = c(3.05, 16.05, 20, 20.75, 14.75, 21.58, 7.86666666667, 17.3333333333,
                17.8333333333, 18, 15.4166666667, 20, 2.43333333333, 19.67, 19.9166666667,
                17.9166666667, 6.51666666667, 26.6666666667, 23.6666666667, 17.8333333333,
                7.21666666667, 7.26666666667, 16.75)
)

vars <- c("aet", "def", "pet", "ppt", "q", "soil", "srad", "swe",
          "tmax", "tmin", "tmean", "vap", "vpd", "ws")

nb_years <- 2024 - 1958 + 1
mois <- rep(1:12, times = nb_years)
annees <- rep(1958:2024, each = 12)

for (i in seq_len(nrow(prov_data))) {
  prov_name <- prov_data$id_prov[i]
  lat <- prov_data$Lat_WGS84[i]
  lon <- prov_data$Lon_WGS84[i]
  
  message(paste0("📍 Traitement de la provenance : ", prov_name))
  
  df <- data.frame(Provenance = prov_name, Annee = annees, Mois = mois)
  
  for (var in vars) {
    message(paste0("   🔄 Variable : ", var))
    
    url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                  "agg_terraclimate_", var, "_1958_CurrentYear_GLOBE.nc")
    
    tryCatch({
      nc <- nc_open(url)
      
      lon_nc <- ncvar_get(nc, "lon")
      lat_nc <- ncvar_get(nc, "lat")
      
      lat_index <- which.min(abs(lat_nc - lat))
      lon_index <- which.min(abs(lon_nc - lon))
      
      start <- c(lon_index, lat_index, 1)
      count <- c(1, 1, -1)
      
      values <- as.numeric(ncvar_get(nc, varid = var, start = start, count = count))
      nc_close(nc)
      
      df[[var]] <- values
      message(paste0("   ✅ ", var, " ajouté"))
      
    }, error = function(e) {
      message(paste0("   ⚠️ Erreur pour ", var, ": ", e$message))
      df[[var]] <- NA
    })
  }
  
  output_file <- paste0("data_", prov_name, "_climatiques.csv")
  write.csv(df, output_file, row.names = FALSE)
  message(paste0("📁 Fichier exporté : ", output_file, "\n"))
}
