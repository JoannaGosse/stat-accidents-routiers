library(corrplot)
library(readr)
library(dplyr)

caracteristiques <- read_delim("D:/Documents/BRUSQ/projet stats/data/2023_caracteristiques.csv", delim = ";")
lieux <- read_delim("D:/Documents/BRUSQ/projet stats/data/2023_lieux.csv", delim = ";")
vehicules <- read_delim("D:/Documents/BRUSQ/projet stats/data/2023_vehicules.csv", delim = ";")
usagers <- read_delim("D:/Documents/BRUSQ/projet stats/data/2023_usagers.csv", delim = ";")

colnames(caracteristiques)
colnames(lieux)
colnames(vehicules)
colnames(usagers)


#Agréger les informations des usagers, on verra ailleurs si on veut utiliser age, sexe, place, catégorie ou motif trajet
usagers_summary <- usagers %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_morts = sum(grav == 2, na.rm = TRUE),   # Nombre de morts dans un accident 
    nb_usagers = n_distinct(id_usager)         # Nombre total d'usagers impliqués dans un accident 
  )

#Agréger les informations des véhicules : on ne prend pas catv et motor car plusieurs véhicule impliqués dans certains accidents
vehicules_summary <- vehicules %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_vehicules = n_distinct(id_vehicule),    # Nombre de véhicules impliqués
  )

#Agréger les informations des lieux: Catégorie de route, Circulation, Profil de route, Plan de la route, Surface de la route, Infrastructure, Situation de la route, Vitesse maximale autorisée
lieux_summary <- lieux %>%
  group_by(Num_Acc) %>%
  summarize(
    catr = first(catr),       
    circ = first(circ),
    prof = first(prof),       
    plan = first(plan),       
    surf = first(surf),        
    infra = first(infra),      
    situ = first(situ),        
    vma = first(vma)           
  )

#Joindre les tables agrégées à la table des caractéristiques, chaque accident est détaillé 
data_accidents_complet <- caracteristiques %>%
  left_join(lieux_summary, by = "Num_Acc") %>%       
  left_join(usagers_summary, by = "Num_Acc") %>%     
  left_join(vehicules_summary, by = "Num_Acc") %>%   
  mutate(
    nb_morts = coalesce(nb_morts, 0),       #Remplacer NA par 0 pour les accidents sans morts
    nb_usagers = coalesce(nb_usagers, 0),   #Remplacer NA par 0 pour les accidents sans usagers
    nb_vehicules = coalesce(nb_vehicules, 0), #Remplacer NA par 0 pour les accidents sans véhicules
  )


# données pour corrélation
data_accidents_complet_corr <- data_accidents_complet %>%
  select(jour, mois, lum, agg, int, atm, col, catr, circ, prof, plan,
         surf, infra, situ, vma)

# Conv en var num
data_accidents_complet_corr_num <- data_accidents_complet_corr %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.factor, as.numeric)

# Matrice de corrélation
mat_corr <- cor(data_accidents_complet_corr_num, use = "complete.obs")
print(mat_corr)
corrplot(mat_corr, method = "color", type = "upper", tl.cex = 0.7)






