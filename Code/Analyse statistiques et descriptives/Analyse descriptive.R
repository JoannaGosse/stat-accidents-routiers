library(readr)  
library(dplyr)     
library(ggplot2)  
library(corrplot)   
library(leaflet)   
library(sf)          

# Importation des données pour 2023
caracteristiques2023 <- read_delim("C:/Users/Formation/Desktop/Accidents_routiers/data/2023_caracteristiques.csv", delim = ";")
lieux2023 <- read_delim("C:/Users/Formation/Desktop/Accidents_routiers/data/2023_lieux.csv", delim = ";")
vehicules2023 <- read_delim("C:/Users/Formation/Desktop/Accidents_routiers/data/2023_vehicules.csv", delim = ";")
usagers2023 <- read_delim("C:/Users/Formation/Desktop/Accidents_routiers/data/2023_usagers.csv", delim = ";")

# Agrégation des données des usagers
usagers_summary2023 <- usagers2023 %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_morts = sum(grav == 2, na.rm = TRUE),
    nb_usagers = n_distinct(id_usager)
  )

# Agrégation des données des véhicules
vehicules_summary2023 <- vehicules2023 %>%
  group_by(Num_Acc) %>%
  summarize(nb_vehicules = n_distinct(id_vehicule))

# Agrégation des données des lieux
lieux_summary2023 <- lieux2023 %>%
  group_by(Num_Acc) %>%
  summarize(catr = first(catr))

# Fusion des tables pour obtenir une vue complète
data_accidents2023 <- caracteristiques2023 %>%
  select(Num_Acc, atm) %>%
  left_join(usagers_summary2023, by = "Num_Acc") %>%
  left_join(vehicules_summary2023, by = "Num_Acc") %>%
  left_join(lieux_summary2023, by = "Num_Acc") %>%
  mutate(
    nb_morts = coalesce(nb_morts, 0),
    nb_usagers = coalesce(nb_usagers, 0),
    nb_vehicules = coalesce(nb_vehicules, 0)
  )

# Définition d'un thème uniforme pour les graphiques
theme_gris <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray98"),
    panel.background = element_rect(fill = "gray95"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray92"),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(color = "gray20", size = 12),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "gray30"),
    legend.position = "none"
  )



ggplot(data_accidents2023, aes(x = as.factor(atm))) +
  geom_bar(fill = "gray40", color = "gray60") +
  labs(
    title = "Distribution des Accidents par Conditions Atmosphériques",
    x = "Conditions Atmosphériques",
    y = "Nombre d'Accidents"
  ) +
  theme_gris


ggplot(data_accidents2023, aes(x = as.factor(catr))) +
  geom_bar(fill = "gray40", color = "gray20") +
  labs(
    title = "Répartition par Type de Route",
    x = "Catégorie de Route",
    y = "Nombre d'Accidents"
  ) +
  theme_gris +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Agrégation par gravité
gravite_summary2023 <- usagers2023 %>%
  group_by(grav) %>%
  summarize(nb_usagers = n()) %>%
  mutate(
    grav = case_when(
      grav == 1 ~ "Indemne",
      grav == 2 ~ "Tué",
      grav == 3 ~ "Blessé hospitalisé",
      grav == 4 ~ "Blessé léger",
      TRUE ~ "Non renseigné"
    )
  )

# Visualisation
ggplot(gravite_summary2023, aes(x = grav, y = nb_usagers, fill = grav)) +
  geom_bar(stat = "identity", fill = "gray40", color = "gray20") +
  labs(
    title = "Répartition de la Gravité des Blessures (2023)",
    x = "Gravité des Blessures",
    y = "Nombre d'Usagers"
  ) +
  theme_gris +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Importation et harmonisation des données 2019-2023
caracteristiques <- list()
for (annee in 2019:2023) {
  df <- read_delim(paste0("C:/Users/Formation/Desktop/Accidents_routiers/data/", annee, "_caracteristiques.csv"), delim = ";")
  df <- df %>%
    mutate(across(c(atm, col, int), as.character), mois = as.numeric(mois)) %>%
    mutate(annee = annee)
  caracteristiques[[as.character(annee)]] <- df
}
caracteristiques_all <- bind_rows(caracteristiques)

# Évolution annuelle
accidents_annuels <- caracteristiques_all %>%
  group_by(annee) %>%
  summarize(nb_accidents = n())

ggplot(accidents_annuels, aes(x = annee, y = nb_accidents)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "darkblue") +
  labs(title = "Évolution Annuelle du Nombre d'Accidents (2019-2023)") +
  theme_minimal()


# Nettoyage et création d'objet spatial
accidents_geo <- caracteristiques2023 %>%
  mutate(lat = as.numeric(gsub(",", ".", lat)), long = as.numeric(gsub(",", ".", long))) %>%
  filter(lat >= 41 & lat <= 51.5, long >= -5 & long <= 10)

# Carte interactive
leaflet(accidents_geo) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~long, lat = ~lat, radius = 3, color = "red", stroke = FALSE, fillOpacity = 0.5)

# Carte de densité
ggplot() +
  stat_density_2d(data = accidents_geo, aes(x = long, y = lat, fill = ..level..), geom = "polygon") +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_minimal()








