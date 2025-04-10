### Fichier SPEI recup sur SPEI Global ### 
# Je veux merge les 23 fichiers SPEI des provenances # 

# Charger les packages
library(dplyr)
library(readr)

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

# Sauvegarder le fichier final
write_csv(compiled_df, file.path(folder_path, "SPEI_merged.csv"))

file_path <- "/Users/david/Desktop/R & Git/GFF_SPEI/raw_data/SPEI_merged.csv"

# import de SPEI_merged 

SPEI_merged <- read.csv("~/Desktop/R & Git/GFF_SPEI/raw_data/SPEI_merged.csv", header=FALSE, sep=";")
View(SPEI_merged)
