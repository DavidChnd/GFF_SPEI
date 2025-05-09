### Fichier SPEI recup sur SPEI Global ### 
# Je veux merge les 23 fichiers SPEI des provenances # 

#### Packages ####
library(dplyr)
library(readr)
library (lubridate)
library(ggplot2)
library(plotly)

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



