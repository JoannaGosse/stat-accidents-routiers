library(readr)       
library(dplyr)       
library(ggplot2)     

caracteristiques2023 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2023_caracteristiques.csv", delim = ";")

accidents_geo <- caracteristiques2023 %>%
  mutate(
    lat = as.numeric(gsub(",", ".", lat)),
    long = as.numeric(gsub(",", ".", long))
  ) %>%
  filter(
    lat >= 41 & lat <= 51.5,
    long >= -5 & long <= 10
  )

# Annotations pour les grandes villes
cities <- data.frame(
  long = c(2.3522, 5.3698, 4.8357, 7.2619),
  lat = c(48.8566, 43.2965, 45.764, 43.7102),
  label = c("Paris", "Marseille", "Lyon", "Nice")
)

#  ?tape 2 : Carte de densit? am?lior?e avec bornes KDE 
ggplot() +
  # Couche KDE pour la densit?
  stat_density_2d(
    data = accidents_geo,
    aes(x = long, y = lat, fill = ..level..),
    geom = "polygon", alpha = 0.7
  ) +
  # Contours des niveaux de densit? (isolignes)
  geom_density_2d(
    data = accidents_geo,
    aes(x = long, y = lat, color = ..level..),
    size = 0.3
  ) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Densit?") +
  scale_color_gradient(low = "orange", high = "darkred", name = "Contours") +
  
  # Points individuels pour les accidents
  geom_point(
    data = accidents_geo,
    aes(x = long, y = lat),
    color = "black", size = 0.2, alpha = 0.4
  ) +
  
  # Ajout des annotations des grandes villes
  geom_text(
    data = cities,
    aes(x = long, y = lat, label = label),
    color = "blue", size = 3, fontface = "bold", hjust = 1
  ) +
  
  # Encadrer la zone ?tudi?e
  annotate(
    "rect", xmin = -5, xmax = 10, ymin = 41, ymax = 51.5,
    color = "blue", fill = NA, alpha = 0.5, linetype = "dashed"
  ) +
  
  # Limites g?ographiques
  coord_fixed(ratio = 1.2, xlim = c(-5, 10), ylim = c(41, 51.5)) +
  
  # Titre et axes
  labs(
    title = "Carte de Densit? des Accidents Routiers en France (2023)",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  # Th?me esth?tique
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right"
  )
