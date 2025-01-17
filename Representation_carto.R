library(readr)
library(dplyr)
library(sf)
library(mapsf)
library(RColorBrewer)

# 1.1) Charger les donnees: data/firmsnetwork/{cities.csv,links.csv}
caracteristiques <- read_csv2('data/2023/caracteristiques.csv')
lieux <- read_csv2('data/2023/lieux.csv')
usagers <- read_csv2('data/2023/usagers.csv')
vehicules <- read_csv2('data/2023/vehicules.csv')
densite_dep <- read_csv2('data_insee/densite_departement.csv')
  
  # Étape 1 : Calculer des valeurs intérésantes
nb_impliques <- usagers %>%
  group_by(Num_Acc) %>%
  summarise(nb_impliques = n())

nb_morts <- usagers %>%
  filter(grav == 2) %>% 
  group_by(Num_Acc) %>%
  summarise(nb_morts = n())

nb_blessé <- usagers %>%
  filter(grav == 3) %>% 
  group_by(Num_Acc) %>%
  summarise(nb_blessé = n())

nb_hospit <- usagers %>%
  filter(grav == 4) %>% 
  group_by(Num_Acc) %>%
  summarise(nb_hospit = n())

nb_indemnes <- usagers %>%
  filter(grav == 1) %>% 
  group_by(Num_Acc) %>%
  summarise(nb_indemnes = n())



# Étape 2 : Joindre ces valeurs à la table principale


caracteristiques <- caracteristiques %>%
  left_join(nb_impliques, by = "Num_Acc")
caracteristiques$nb_impliques[is.na(caracteristiques$nb_impliques)] <- 0

caracteristiques <- caracteristiques %>%
  left_join(nb_morts, by = "Num_Acc")
caracteristiques$nb_morts[is.na(caracteristiques$nb_morts)] <- 0

caracteristiques <- caracteristiques %>%
  left_join(nb_blessé, by = "Num_Acc")
caracteristiques$nb_blessé[is.na(caracteristiques$nb_blessé)] <- 0

caracteristiques <- caracteristiques %>%
  left_join(nb_hospit, by = "Num_Acc")
caracteristiques$nb_hospit[is.na(caracteristiques$nb_hospit)] <- 0

caracteristiques <- caracteristiques %>%
  left_join(nb_indemnes, by = "Num_Acc")
caracteristiques$nb_indemnes[is.na(caracteristiques$nb_indemnes)] <- 0


#Réduire l'étude à la france métropolitaine

caracteristiques <- caracteristiques %>%
  filter(!is.na(long), !is.na(lat), 41<lat, lat<51, -5<long,long<10) 


#Grouper par département
caracteristiques <- caracteristiques %>%
    group_by(dep) %>%
    summarise(
      morts = sum(nb_morts),
      mortalite = sum(morts)/n(),
      impliqués = sum(nb_impliques),
      nombre_accidents=n(),
    )

# 1.2) Cartographier la specialisation des aires urbaines


# - avec fond de carte de la france

france <- st_read('pays','departements-20180101')
france <- france %>% filter(code_insee %in% caracteristiques$dep)
france$centroid <- st_centroid(france$geometry)
mf_base(france, extent = st_bbox(st_position))


caracteristiques <- caracteristiques %>%
  inner_join(france %>% select(code_insee, centroid), by = c('dep'='code_insee'))

caracteristiques <- caracteristiques %>%
  mutate(long = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2])

st_caracteristiques = st_as_sf(caracteristiques, coords = c("long","lat"), crs = 2154)

mf_map(
  st_caracteristiques,
  type = 'prop_choro',
  var = c('nombre_accidents', 'mortalite'),
  leg_pos = c("topright"),
  leg_title = c("Nombre d'accidents", "Mortalité"),
  pal = brewer.pal(8, "Reds")
)
mf_title("Nombre d'accidents et taux de mortalité")
