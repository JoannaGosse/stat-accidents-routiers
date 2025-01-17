library(corrplot)
library(readr)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)

#################################################################################################################################################

# Importation des donn?es 2023
caracteristiques2023 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2023_caracteristiques.csv", delim = ";")
lieux2023 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2023_lieux.csv", delim = ";")
vehicules2023 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2023_vehicules.csv", delim = ";")
usagers2023 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2023_usagers.csv", delim = ";")

# Agr?gation des informations des usagers
usagers_summary2023 <- usagers2023 %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_morts = sum(grav == 2, na.rm = TRUE),  
    nb_usagers = n_distinct(id_usager)       
  )

# Agr?gation des informations des v?hicules
vehicules_summary2023 <- vehicules2023 %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_vehicules = n_distinct(id_vehicule) 
  )

# Agr?gation des informations des lieux
lieux_summary2023 <- lieux2023 %>%
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

# Fusion des donn?es
data_accidents_complet2023 <- caracteristiques2023 %>%
  left_join(lieux_summary2023, by = "Num_Acc") %>%
  left_join(usagers_summary2023, by = "Num_Acc") %>%
  left_join(vehicules_summary2023, by = "Num_Acc") %>%
  mutate(
    nb_morts = coalesce(nb_morts, 0),
    nb_usagers = coalesce(nb_usagers, 0),
    nb_vehicules = coalesce(nb_vehicules, 0)
  )

# S?lection des variables pour la corr?lation
data_accidents_complet_corr2023 <- data_accidents_complet2023 %>%
  select(jour, mois, lum, agg, int, atm, col, catr, circ, prof, plan,
         surf, infra, situ, vma, nb_morts, nb_usagers, nb_vehicules)

# Conversion en variables num?riques
data_accidents_complet_corr_num2023 <- data_accidents_complet_corr2023 %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.factor, as.numeric)

# Matrice de corr?lation
mat_corr2023 <- cor(data_accidents_complet_corr_num2023, use = "complete.obs")

# Visualisation de la matrice de corr?lation (esth?tique)
corrplot(mat_corr2023, method = "color", type = "upper", 
         col = colorRampPalette(c("red", "white", "blue"))(200), 
         tl.cex = 0.8, tl.col = "black", number.cex = 0.7)

# R?alisation de l'ACP
res_acp <- PCA(data_accidents_complet_corr_num2023, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Visualisation des contributions des variables
fviz_contrib(res_acp, choice = "var", axes = 1, top = 10, 
             fill = "steelblue", color = "steelblue", 
             title = "Contributions des Variables - Axe 1") +
  theme_minimal()

fviz_contrib(res_acp, choice = "var", axes = 2, top = 10, 
             fill = "darkorange", color = "darkorange", 
             title = "Contributions des Variables - Axe 2") +
  theme_minimal()

# Visualisation des valeurs propres (variance expliqu?e)
fviz_eig(res_acp, addlabels = TRUE, 
         barfill = "darkblue", barcolor = "darkblue", 
         linecolor = "red") +
  labs(title = "Variance Expliqu?e par les Axes", 
       x = "Composantes Principales", 
       y = "Pourcentage de Variance Expliqu?e") +
  theme_minimal()

# Visualisation des variables sur le plan factoriel
fviz_pca_var(res_acp, col.var = "contrib", 
             gradient.cols = c("lightblue", "blue", "darkblue"), 
             repel = TRUE, 
             title = "Projection des Variables sur le Plan Factoriel") +
  theme_minimal()

# Visualisation des individus (accidents) sur le plan factoriel
fviz_pca_ind(res_acp, col.ind = "cos2", 
             gradient.cols = c("lightgreen", "green", "darkgreen"), 
             repel = TRUE, 
             title = "Projection des Individus sur le Plan Factoriel") +
  theme_minimal()



#################################################################################################################################################
# Chargement des biblioth?ques 

library(corrplot)
library(readr)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)

# --------------------------------------------------------------------------------------------------
# 1. Importation des donn?es 2022
caracteristiques2022 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2022_caracteristiques.csv", delim = ";")
lieux2022 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2022_lieux.csv", delim = ";")
vehicules2022 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2022_vehicules.csv", delim = ";")
usagers2022 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2022_usagers.csv", delim = ";")

# --------------------------------------------------------------------------------------------------
# 2. V?rification des colonnes
colnames(caracteristiques2022)
colnames(lieux2022)
colnames(usagers2022)
colnames(vehicules2022)

# --------------------------------------------------------------------------------------------------
# 3. Agr?gation des donn?es

# Agr?gation des informations des usagers
usagers_summary2022 <- usagers2022 %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_morts = sum(grav == 2, na.rm = TRUE),  # Nombre de morts
    nb_usagers = n_distinct(id_usager)       # Nombre total d'usagers impliqu?s
  )

# Agr?gation des informations des v?hicules
vehicules_summary2022 <- vehicules2022 %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_vehicules = n_distinct(id_vehicule)   # Nombre de v?hicules impliqu?s
  )

# Agr?gation des informations des lieux
lieux_summary2022 <- lieux2022 %>%
  group_by(Num_Acc) %>%
  summarize(
    catr = first(catr), circ = first(circ),
    prof = first(prof), plan = first(plan),
    surf = first(surf), infra = first(infra),
    situ = first(situ), vma = first(vma)
  )

# Fusion des donn?es
data_accidents_complet2022 <- caracteristiques2022 %>%
  rename(Num_Acc = Accident_Id) %>% # Renommer pour correspondre aux autres tables
  left_join(lieux_summary2022, by = "Num_Acc") %>%
  left_join(usagers_summary2022, by = "Num_Acc") %>%
  left_join(vehicules_summary2022, by = "Num_Acc") %>%
  mutate(
    nb_morts = coalesce(nb_morts, 0),        # Remplacement des valeurs manquantes par 0
    nb_usagers = coalesce(nb_usagers, 0),   # Remplacement des valeurs manquantes par 0
    nb_vehicules = coalesce(nb_vehicules, 0) # Remplacement des valeurs manquantes par 0
  )

# --------------------------------------------------------------------------------------------------
# 4. Matrice de corr?lation

# S?lection des variables pour la corr?lation
data_accidents_complet_corr2022 <- data_accidents_complet2022 %>%
  select(jour, mois, lum, agg, int, atm, col, catr, circ, prof, plan,
         surf, infra, situ, vma, nb_morts, nb_usagers, nb_vehicules)

# Conversion en variables num?riques
data_accidents_complet_corr_num2022 <- data_accidents_complet_corr2022 %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.factor, as.numeric)

# Calcul de la matrice de corr?lation
mat_corr2022 <- cor(data_accidents_complet_corr_num2022, use = "complete.obs")

# Visualisation de la matrice de corr?lation
corrplot(mat_corr2022, method = "color", type = "upper", 
         col = colorRampPalette(c("red", "white", "blue"))(200), 
         tl.cex = 0.8, tl.col = "black", number.cex = 0.7)

# --------------------------------------------------------------------------------------------------
# 5. Analyse en Composantes Principales (ACP)

# R?alisation de l'ACP
res_acp <- PCA(data_accidents_complet_corr_num2022, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Visualisation des contributions des variables
fviz_contrib(res_acp, choice = "var", axes = 1, top = 10, 
             fill = "steelblue", color = "steelblue") +
  labs(title = "Contributions des Variables - Axe 1") +
  theme_minimal()

fviz_contrib(res_acp, choice = "var", axes = 2, top = 10, 
             fill = "darkorange", color = "darkorange") +
  labs(title = "Contributions des Variables - Axe 2") +
  theme_minimal()

# Visualisation des valeurs propres (variance expliqu?e)
fviz_eig(res_acp, addlabels = TRUE, 
         barfill = "darkblue", barcolor = "darkblue", 
         linecolor = "red") +
  labs(title = "Variance Expliqu?e par les Axes", 
       x = "Composantes Principales", 
       y = "Pourcentage de Variance Expliqu?e") +
  theme_minimal()

# Visualisation des variables sur le plan factoriel
fviz_pca_var(res_acp, col.var = "contrib", 
             gradient.cols = c("lightblue", "blue", "darkblue"), 
             repel = TRUE) +
  labs(title = "Projection des Variables sur le Plan Factoriel") +
  theme_minimal()

# Visualisation des individus (accidents) sur le plan factoriel
fviz_pca_ind(res_acp, col.ind = "cos2", 
             gradient.cols = c("lightgreen", "green", "darkgreen"), 
             repel = TRUE) +
  labs(title = "Projection des Individus sur le Plan Factoriel") +
  theme_minimal()

# --------------------------------------------------------------------------------------------------
# 6. Identification des variables les moins contributives

# Supposons que les variables "surf" et "situ" sont les moins contributives
data_accidents_acp_reduit <- data_accidents_complet_corr_num2022 %>%
  select(-surf, -situ)

# Pr?paration des donn?es r?duites
data_accidents_acp_reduit <- data_accidents_acp_reduit %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  scale()

# Nouvelle ACP sur les donn?es r?duites
res_acp_reduit <- PCA(data_accidents_acp_reduit, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Visualisation des valeurs propres pour les donn?es r?duites
fviz_eig(res_acp_reduit, addlabels = TRUE, 
         barfill = "darkblue", barcolor = "darkblue", 
         linecolor = "red") +
  labs(title = "Variance Expliqu?e par les Axes (Donn?es R?duites)", 
       x = "Composantes Principales", 
       y = "Pourcentage de Variance Expliqu?e") +
  theme_minimal()

# Visualisation des variables sur le plan factoriel (Donn?es R?duites)
fviz_pca_var(res_acp_reduit, col.var = "contrib", 
             gradient.cols = c("lightblue", "blue", "darkblue"), 
             repel = TRUE) +
  labs(title = "Projection des Variables (Donn?es R?duites)") +
  theme_minimal()

# Visualisation des individus sur le plan factoriel (Donn?es R?duites)
fviz_pca_ind(res_acp_reduit, col.ind = "cos2", 
             gradient.cols = c("lightgreen", "green", "darkgreen"), 
             repel = TRUE) +
  labs(title = "Projection des Individus (Donn?es R?duites)") +
  theme_minimal()

##################################################################################################################################

# Chargement des biblioth?ques n?cessaires
library(corrplot)
library(readr)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(missMDA)

# --------------------------------------------------------------------------------------------------
# 1. Importation des donn?es 2021
caracteristiques2021 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2021_caracteristiques.csv", delim = ";")
lieux2021 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2021_lieux.csv", delim = ";")
vehicules2021 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2021_vehicules.csv", delim = ";")
usagers2021 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2021_usagers.csv", delim = ";")

# --------------------------------------------------------------------------------------------------
# 2. Agr?gation des donn?es

# Agr?gation des informations des usagers
usagers_summary2021 <- usagers2021 %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_morts = sum(grav == 2, na.rm = TRUE),  # Nombre de morts
    nb_usagers = n_distinct(id_usager)       # Nombre total d'usagers impliqu?s
  )

# Agr?gation des informations des v?hicules
vehicules_summary2021 <- vehicules2021 %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_vehicules = n_distinct(id_vehicule)   # Nombre de v?hicules impliqu?s
  )

# Agr?gation des informations des lieux
lieux_summary2021 <- lieux2021 %>%
  group_by(Num_Acc) %>%
  summarize(
    catr = first(catr), circ = first(circ),
    prof = first(prof), plan = first(plan),
    surf = first(surf), infra = first(infra),
    situ = first(situ), vma = first(vma)
  )

# Fusion des donn?es
data_accidents_complet2021 <- caracteristiques2021 %>%
  left_join(lieux_summary2021, by = "Num_Acc") %>%
  left_join(usagers_summary2021, by = "Num_Acc") %>%
  left_join(vehicules_summary2021, by = "Num_Acc") %>%
  mutate(
    nb_morts = coalesce(nb_morts, 0),
    nb_usagers = coalesce(nb_usagers, 0),
    nb_vehicules = coalesce(nb_vehicules, 0)
  )

# --------------------------------------------------------------------------------------------------
# 3. Matrice de corr?lation

# S?lection des variables pour la corr?lation
data_accidents_complet_corr2021 <- data_accidents_complet2021 %>%
  select(jour, mois, lum, agg, int, atm, col, catr, circ, prof, plan,
         surf, infra, situ, vma, nb_morts, nb_usagers, nb_vehicules)

# Conversion en variables num?riques
data_accidents_complet_corr_num2021 <- data_accidents_complet_corr2021 %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.factor, as.numeric)

# Calcul de la matrice de corr?lation
mat_corr2021 <- cor(data_accidents_complet_corr_num2021, use = "complete.obs")

# Visualisation de la matrice de corr?lation
corrplot(mat_corr2021, method = "color", type = "upper", 
         col = colorRampPalette(c("red", "white", "blue"))(200), 
         tl.cex = 0.8, tl.col = "black", number.cex = 0.7)

# --------------------------------------------------------------------------------------------------
# 4. Analyse en Composantes Principales (ACP)

# R?alisation de l'ACP
res_acp <- PCA(data_accidents_complet_corr_num2021, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Visualisation des contributions des variables
fviz_contrib(res_acp, choice = "var", axes = 1, top = 10, 
             fill = "steelblue", color = "steelblue") +
  labs(title = "Contributions des Variables - Axe 1 (2021)") +
  theme_minimal()

fviz_contrib(res_acp, choice = "var", axes = 2, top = 10, 
             fill = "darkorange", color = "darkorange") +
  labs(title = "Contributions des Variables - Axe 2 (2021)") +
  theme_minimal()

# Visualisation des valeurs propres (variance expliqu?e)
fviz_eig(res_acp, addlabels = TRUE, 
         barfill = "darkblue", barcolor = "darkblue", 
         linecolor = "red") +
  labs(title = "Variance Expliqu?e par les Axes (2021)", 
       x = "Composantes Principales", 
       y = "Pourcentage de Variance Expliqu?e") +
  theme_minimal()

# Visualisation des variables sur le plan factoriel
fviz_pca_var(res_acp, col.var = "contrib", 
             gradient.cols = c("lightblue", "blue", "darkblue"), 
             repel = TRUE) +
  labs(title = "Projection des Variables (2021)") +
  theme_minimal()

# Visualisation des individus (accidents) sur le plan factoriel
fviz_pca_ind(res_acp, col.ind = "cos2", 
             gradient.cols = c("lightgreen", "green", "darkgreen"), 
             repel = TRUE) +
  labs(title = "Projection des Individus (2021)") +
  theme_minimal()

# --------------------------------------------------------------------------------------------------
# 5. Identification des variables les moins contributives

# Supposons que les variables "surf" et "situ" sont les moins contributives
data_accidents_acp_reduit <- data_accidents_complet_corr_num2021 %>%
  select(-surf, -situ)

# Pr?paration des donn?es r?duites
data_accidents_acp_reduit <- data_accidents_acp_reduit %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  scale()

# Nouvelle ACP sur les donn?es r?duites
res_acp_reduit <- PCA(data_accidents_acp_reduit, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Visualisation des valeurs propres pour les donn?es r?duites
fviz_eig(res_acp_reduit, addlabels = TRUE, 
         barfill = "darkblue", barcolor = "darkblue", 
         linecolor = "red") +
  labs(title = "Variance Expliqu?e par les Axes (Donn?es R?duites - 2021)", 
       x = "Composantes Principales", 
       y = "Pourcentage de Variance Expliqu?e") +
  theme_minimal()

# Visualisation des variables sur le plan factoriel (Donn?es R?duites)
fviz_pca_var(res_acp_reduit, col.var = "contrib", 
             gradient.cols = c("lightblue", "blue", "darkblue"), 
             repel = TRUE) +
  labs(title = "Projection des Variables (Donn?es R?duites - 2021)") +
  theme_minimal()

# Visualisation des individus sur le plan factoriel (Donn?es R?duites)
fviz_pca_ind(res_acp_reduit, col.ind = "cos2", 
             gradient.cols = c("lightgreen", "green", "darkgreen"), 
             repel = TRUE) +
  labs(title = "Projection des Individus (Donn?es R?duites - 2021)") +
  theme_minimal()

##############################################################################################################################

# Chargement des biblioth?ques n?cessaires
library(corrplot)
library(readr)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(missMDA)

# --------------------------------------------------------------------------------------------------
# 1. Importation des donn?es 2020
caracteristiques2020 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2020_caracteristiques.csv", delim = ";")
lieux2020 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2020_lieux.csv", delim = ";")
vehicules2020 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2020_vehicules.csv", delim = ";")
usagers2020 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2020_usagers.csv", delim = ";")

# --------------------------------------------------------------------------------------------------
# 2. Agr?gation des donn?es

# Agr?gation des informations des usagers
usagers_summary2020 <- usagers2020 %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_morts = sum(grav == 2, na.rm = TRUE),  # Nombre de morts
    nb_usagers = n()                         # Nombre total d'usagers impliqu?s
  )

# Agr?gation des informations des v?hicules
vehicules_summary2020 <- vehicules2020 %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_vehicules = n_distinct(id_vehicule)   # Nombre de v?hicules impliqu?s
  )

# Agr?gation des informations des lieux
lieux_summary2020 <- lieux2020 %>%
  group_by(Num_Acc) %>%
  summarize(
    catr = first(catr), circ = first(circ),
    prof = first(prof), plan = first(plan),
    surf = first(surf), infra = first(infra),
    situ = first(situ), vma = first(vma)
  )

# Fusion des donn?es
data_accidents_complet2020 <- caracteristiques2020 %>%
  left_join(lieux_summary2020, by = "Num_Acc") %>%
  left_join(usagers_summary2020, by = "Num_Acc") %>%
  left_join(vehicules_summary2020, by = "Num_Acc") %>%
  mutate(
    nb_morts = coalesce(nb_morts, 0),
    nb_usagers = coalesce(nb_usagers, 0),
    nb_vehicules = coalesce(nb_vehicules, 0)
  )

# --------------------------------------------------------------------------------------------------
# 3. Matrice de corr?lation

# S?lection des variables pour la corr?lation
data_accidents_complet_corr2020 <- data_accidents_complet2020 %>%
  select(jour, mois, lum, agg, int, atm, col, catr, circ, prof, plan,
         surf, infra, situ, vma, nb_morts, nb_usagers, nb_vehicules)

# Conversion en variables num?riques
data_accidents_complet_corr_num2020 <- data_accidents_complet_corr2020 %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.factor, as.numeric)

# Calcul de la matrice de corr?lation
mat_corr2020 <- cor(data_accidents_complet_corr_num2020, use = "complete.obs")

# Visualisation de la matrice de corr?lation
corrplot(mat_corr2020, method = "color", type = "upper", 
         col = colorRampPalette(c("red", "white", "blue"))(200), 
         tl.cex = 0.8, tl.col = "black", number.cex = 0.7)

# --------------------------------------------------------------------------------------------------
# 4. Analyse en Composantes Principales (ACP)

# R?alisation de l'ACP
res_acp <- PCA(data_accidents_complet_corr_num2020, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Visualisation des contributions des variables
fviz_contrib(res_acp, choice = "var", axes = 1, top = 10, 
             fill = "steelblue", color = "steelblue") +
  labs(title = "Contributions des Variables - Axe 1 (2020)") +
  theme_minimal()

fviz_contrib(res_acp, choice = "var", axes = 2, top = 10, 
             fill = "darkorange", color = "darkorange") +
  labs(title = "Contributions des Variables - Axe 2 (2020)") +
  theme_minimal()

# Visualisation des valeurs propres (variance expliqu?e)
fviz_eig(res_acp, addlabels = TRUE, 
         barfill = "darkblue", barcolor = "darkblue", 
         linecolor = "red") +
  labs(title = "Variance Expliqu?e par les Axes (2020)", 
       x = "Composantes Principales", 
       y = "Pourcentage de Variance Expliqu?e") +
  theme_minimal()

# Visualisation des variables sur le plan factoriel
fviz_pca_var(res_acp, col.var = "contrib", 
             gradient.cols = c("lightblue", "blue", "darkblue"), 
             repel = TRUE) +
  labs(title = "Projection des Variables (2020)") +
  theme_minimal()

# Visualisation des individus (accidents) sur le plan factoriel
fviz_pca_ind(res_acp, col.ind = "cos2", 
             gradient.cols = c("lightgreen", "green", "darkgreen"), 
             repel = TRUE) +
  labs(title = "Projection des Individus (2020)") +
  theme_minimal()

# --------------------------------------------------------------------------------------------------
# 5. Identification des variables les moins contributives

# Supposons que les variables "surf" et "situ" sont les moins contributives
data_accidents_acp_reduit <- data_accidents_complet_corr_num2020 %>%
  select(-surf, -situ)

# Pr?paration des donn?es r?duites
data_accidents_acp_reduit <- data_accidents_acp_reduit %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  scale()

# Nouvelle ACP sur les donn?es r?duites
res_acp_reduit <- PCA(data_accidents_acp_reduit, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Visualisation des valeurs propres pour les donn?es r?duites
fviz_eig(res_acp_reduit, addlabels = TRUE, 
         barfill = "darkblue", barcolor = "darkblue", 
         linecolor = "red") +
  labs(title = "Variance Expliqu?e par les Axes (Donn?es R?duites - 2020)", 
       x = "Composantes Principales", 
       y = "Pourcentage de Variance Expliqu?e") +
  theme_minimal()

# Visualisation des variables sur le plan factoriel (Donn?es R?duites)
fviz_pca_var(res_acp_reduit, col.var = "contrib", 
             gradient.cols = c("lightblue", "blue", "darkblue"), 
             repel = TRUE) +
  labs(title = "Projection des Variables (Donn?es R?duites - 2020)") +
  theme_minimal()

# Visualisation des individus sur le plan factoriel (Donn?es R?duites)
fviz_pca_ind(res_acp_reduit, col.ind = "cos2", 
             gradient.cols = c("lightgreen", "green", "darkgreen"), 
             repel = TRUE) +
  labs(title = "Projection des Individus (Donn?es R?duites - 2020)") +
  theme_minimal()





################################################################################################################################
# Chargement des biblioth?ques n?cessaires
library(corrplot)
library(readr)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(psych)    # Statistiques descriptives
library(stats)    # Fonctions de base (cmdscale)
library(vegan)    # Test de Mantel

# 1. Importation des donn?es 2019
caracteristiques2019 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2019_caracteristiques.csv", delim = ";")
lieux2019 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2019_lieux.csv", delim = ";")
vehicules2019 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2019_vehicules.csv", delim = ";")
usagers2019 <- read_delim("C:/Users/axeld/OneDrive/Documents/Accident routier/data/2019_usagers.csv", delim = ";")

# 2. Agr?gation des donn?es
usagers_summary2019 <- usagers2019 %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_morts = sum(grav == 2, na.rm = TRUE),   # Nombre de morts
    nb_usagers = n()                          # Nombre total d'usagers impliqu?s
  )

vehicules_summary2019 <- vehicules2019 %>%
  group_by(Num_Acc) %>%
  summarize(
    nb_vehicules = n_distinct(id_vehicule)    # Nombre de v?hicules impliqu?s
  )

lieux_summary2019 <- lieux2019 %>%
  group_by(Num_Acc) %>%
  summarize(
    catr = first(catr), circ = first(circ),
    prof = first(prof), plan = first(plan),
    surf = first(surf), infra = first(infra),
    situ = first(situ), vma = first(vma)
  )

# 3. Fusion des donn?es
data_accidents_complet2019 <- caracteristiques2019 %>%
  left_join(lieux_summary2019, by = "Num_Acc") %>%
  left_join(usagers_summary2019, by = "Num_Acc") %>%
  left_join(vehicules_summary2019, by = "Num_Acc") %>%
  mutate(
    nb_morts = coalesce(nb_morts, 0),
    nb_usagers = coalesce(nb_usagers, 0),
    nb_vehicules = coalesce(nb_vehicules, 0)
  )

# 4. Matrice de corr?lation
data_accidents_complet_corr2019 <- data_accidents_complet2019 %>%
  select(jour, mois, lum, agg, int, atm, col, catr, circ, prof, plan,
         surf, infra, situ, vma, nb_morts, nb_usagers, nb_vehicules)

data_accidents_complet_corr_num2019 <- data_accidents_complet_corr2019 %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.factor, as.numeric)

mat_corr2019 <- cor(data_accidents_complet_corr_num2019, use = "complete.obs")

# Visualisation de la matrice de corr?lation
corrplot(mat_corr2019, method = "color", type = "upper",
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.cex = 0.8, tl.col = "black", number.cex = 0.7)

# 5. Analyse en Composantes Principales (ACP)
res_acp <- PCA(data_accidents_complet_corr_num2019, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Contributions des variables
fviz_contrib(res_acp, choice = "var", axes = 1, top = 10,
             fill = "steelblue", color = "steelblue") +
  labs(title = "Contributions des Variables - Axe 1 (2019)") +
  theme_minimal()

fviz_contrib(res_acp, choice = "var", axes = 2, top = 10,
             fill = "darkorange", color = "darkorange") +
  labs(title = "Contributions des Variables - Axe 2 (2019)") +
  theme_minimal()

# Projection des variables
fviz_pca_var(res_acp, col.var = "contrib",
             gradient.cols = c("lightblue", "blue", "darkblue"),
             repel = TRUE) +
  labs(title = "Projection des Variables (2019)") +
  theme_minimal()

# Projection des individus
fviz_pca_ind(res_acp, col.ind = "cos2",
             gradient.cols = c("lightgreen", "green", "darkgreen"),
             repel = TRUE) +
  labs(title = "Projection des Individus (2019)") +
  theme_minimal()

#############################################################################################################################


# Chargement des biblioth?ques n?cessaires
library(psych)     # Pour des statistiques descriptives
library(stats)     # Pour cmdscale
library(vegan)     # Pour le test de Mantel
library(corrplot)  # Pour visualiser les corr?lations
library(ggplot2)   # Pour des graphiques esth?tiques
library(dplyr)     # Pour la manipulation des donn?es

# --------------------------------------------------------------------------------------------------
# 1. Liste des matrices de corr?lation
matrices <- list(
  "2023" = mat_corr2023,
  "2022" = mat_corr2022,
  "2021" = mat_corr2021,
  "2020" = mat_corr2020,
  "2019" = mat_corr2019
)

# --------------------------------------------------------------------------------------------------
# 2. Distance de Frobenius : Mesure globale des diff?rences entre matrices
frobenius_distance <- function(mat1, mat2) {
  sqrt(sum((mat1 - mat2)^2))
}

# Calcul des distances de Frobenius pour chaque paire d'ann?es
years <- names(matrices)
distance_matrix <- matrix(NA, nrow = length(years), ncol = length(years), dimnames = list(years, years))

for (i in 1:length(years)) {
  for (j in i:length(years)) {
    distance_matrix[i, j] <- frobenius_distance(matrices[[years[i]]], matrices[[years[j]]])
    distance_matrix[j, i] <- distance_matrix[i, j]  # Matrice sym?trique
  }
}

# Affichage de la matrice des distances de Frobenius
print("Matrice des distances de Frobenius :")
print(distance_matrix)

# Visualisation de la matrice des distances de Frobenius
corrplot(distance_matrix, is.corr = FALSE, method = "color", 
         addCoef.col = "black", tl.cex = 1, 
         col = colorRampPalette(c("white", "blue"))(200),
         title = "Distances de Frobenius entre les matrices de corr?lation")

# -----------------------------------------------------# 📚 Chargement des bibliothèques nécessaires
library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyr)

# 📊 Étape 1 : Vos matrices de corrélation réelles
set.seed(123)
data2023 <- matrix(rnorm(100), nrow = 10)
data2022 <- matrix(rnorm(100), nrow = 10)
data2021 <- matrix(rnorm(100), nrow = 10)
data2020 <- matrix(rnorm(100), nrow = 10)
data2019 <- matrix(rnorm(100), nrow = 10)

mat_corr2023 <- cor(data2023)
mat_corr2022 <- cor(data2022)
mat_corr2021 <- cor(data2021)
mat_corr2020 <- cor(data2020)
mat_corr2019 <- cor(data2019)

matrices <- list(
  "2023" = mat_corr2023,
  "2022" = mat_corr2022,
  "2021" = mat_corr2021,
  "2020" = mat_corr2020,
  "2019" = mat_corr2019
)

# 📊 Étape 2 : Fonction de bootstrap pour les distances de Frobenius
bootstrap_frobenius <- function(matrices, n_bootstrap = 1000) {
  years <- names(matrices)
  boot_results <- list()
  
  # Initialiser les résultats
  for (i in 1:length(years)) {
    for (j in i:length(years)) {
      boot_results[[paste(years[i], years[j], sep = " vs ")]] <- numeric(0)
    }
  }
  
  # Calcul bootstrap
  for (b in 1:n_bootstrap) {
    for (i in 1:length(years)) {
      for (j in i:length(years)) {
        mat1 <- matrices[[years[i]]] + matrix(rnorm(length(matrices[[years[i]]]), sd = 0.01), nrow = nrow(matrices[[years[i]]]))
        mat2 <- matrices[[years[j]]] + matrix(rnorm(length(matrices[[years[j]]]), sd = 0.01), nrow = nrow(matrices[[years[j]]]))
        dist_boot <- sqrt(sum((mat1 - mat2)^2))
        boot_results[[paste(years[i], years[j], sep = " vs ")]] <- c(boot_results[[paste(years[i], years[j], sep = " vs ")]], dist_boot)
      }
    }
  }
  
  # Calcul des statistiques résumées
  summary_results <- lapply(boot_results, function(x) {
    c(
      Mean = mean(x),
      Lower_CI = quantile(x, 0.025),
      Upper_CI = quantile(x, 0.975)
    )
  })
  
  return(summary_results)
}

# Calcul des distances bootstrap
bootstrap_results <- bootstrap_frobenius(matrices, n_bootstrap = 1000)

# 📊 Étape 3 : Extraction des données pour ggplot
distance_data <- do.call(rbind, lapply(names(bootstrap_results), function(pair) {
  data.frame(
    Pair = pair,
    Mean = bootstrap_results[[pair]]["Mean"],
    Lower_CI = bootstrap_results[[pair]]["Lower_CI"],
    Upper_CI = bootstrap_results[[pair]]["Upper_CI"]
  )
}))

# 📊 Étape 4 : Visualisation des résultats bootstrap
ggplot(distance_data, aes(x = Pair, y = Mean)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "black") +
  labs(
    title = "Distances de Frobenius avec intervalles de confiance (Bootstrap)",
    x = "Paires d'années",
    y = "Distance moyenne"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 📊 Étape 5 : Création d'une matrice pour représenter les distances moyennes
years <- names(matrices)
distance_matrix <- matrix(NA, nrow = length(years), ncol = length(years), dimnames = list(years, years))

for (pair in names(bootstrap_results)) {
  parts <- unlist(strsplit(pair, " vs "))
  distance_matrix[parts[1], parts[2]] <- bootstrap_results[[pair]]["Mean"]
  distance_matrix[parts[2], parts[1]] <- bootstrap_results[[pair]]["Mean"]
}

# 📊 Étape 6 : Visualisation de la matrice des distances de Frobenius
corrplot(
  distance_matrix,
  is.corr = FALSE,
  method = "color",
  addCoef.col = "black",
  tl.col = "red",
  tl.cex = 1,
  col = colorRampPalette(c("white", "blue"))(200),
  title = "Distances de Frobenius entre les matrices de corrélation"
)



# --------------------------------------------------------------------------------------------------



# 3. Corr?lation des corr?lations
# Extraction des ?l?ments hors diagonale
upper_tri_indices <- upper.tri(matrices[[1]])
correlation_comparisons <- combn(years, 2, function(pair) {
  cor(
    as.vector(matrices[[pair[1]]][upper_tri_indices]),
    as.vector(matrices[[pair[2]]][upper_tri_indices])
  )
}, simplify = FALSE)

# Affichage des r?sultats
names(correlation_comparisons) <- combn(years, 2, paste, collapse = " vs ")
print("Corr?lations des corr?lations entre ann?es :")
print(correlation_comparisons)

# --------------------------------------------------------------------------------------------------
# 4. Analyse MDS (repr?sentation visuelle des distances)
# Conversion des distances de Frobenius en objet dist
dist_object <- as.dist(distance_matrix)

# Multidimensional Scaling (MDS)
mds <- cmdscale(dist_object)

# Cr?ation d'un DataFrame pour ggplot
mds_df <- data.frame(Dimension1 = mds[, 1], Dimension2 = mds[, 2], Year = years)

# Visualisation MDS avec ggplot2
ggplot(mds_df, aes(x = Dimension1, y = Dimension2, label = Year)) +
  geom_point(size = 5, color = "blue") +
  geom_text(vjust = -1, size = 5) +
  labs(title = "Analyse MDS des matrices de corr?lation",
       x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

# --------------------------------------------------------------------------------------------------
# 5. Test de Mantel
# Mantel test pour chaque paire d'ann?es
mantel_results <- combn(years, 2, function(pair) {
  mantel(as.dist(matrices[[pair[1]]]), as.dist(matrices[[pair[2]]]))
}, simplify = FALSE)

# Affichage des r?sultats du test de Mantel
names(mantel_results) <- combn(years, 2, paste, collapse = " vs ")
print("R?sultats du test de Mantel :")
lapply(mantel_results, summary)

# --------------------------------------------------------------------------------------------------
# 6. Analyse des r?sultats
cat("\n???? **R?sum? des analyses** :\n")
cat("- **Distances de Frobenius** :\n  Faible = Matrices similaires, ?lev?e = Diff?rences marqu?es.\n")
cat("- **Corr?lation des corr?lations** :\n  r ??? 1 = Structures similaires, Faible ou n?gatif = Relations diff?rentes.\n")
cat("- **Analyse MDS** :\n  Ann?es proches = Relations stables entre variables.\n")
cat("- **Test de Mantel** :\n  p < 0.05 = Similarit? statistiquement significative entre deux matrices.\n")


# 📚 Chargement des bibliothèques nécessaires pour l'ACP
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(missMDA)
library(dplyr)
library(readr)
library(corrplot)

# 📊 **1. Fonction d'ACP pour une année donnée**
realiser_acp <- function(data, annee) {
  cat("\n🔄 Traitement des données pour l'année :", annee, "\n")
  
  # 📊 1. Préparation des données
  data_acp <- data %>%
    mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
    scale()
  
  # 📊 2. Réalisation de l'ACP
  res_acp <- PCA(data_acp, scale.unit = TRUE, ncp = 5, graph = FALSE)
  
  # 📊 3. Analyse des valeurs propres
  fviz_eig(res_acp, 
           addlabels = TRUE,
           barfill = "gray40",
           barcolor = "gray20",
           linecolor = "red") +
    labs(title = paste("Variance Expliquée par les Axes Principaux (", annee, ")", sep = ""),
         x = "Composantes Principales",
         y = "Pourcentage de Variance Expliquée") +
    theme_minimal()
  ggsave(paste0("C:/Users/axeld/OneDrive/Documents/Accident routier/Result/", annee, ".png"), width = 10, height = 8)
  
  # 📊 4. Visualisation des variables sur le plan factoriel
  fviz_pca_var(res_acp,
               col.var = "contrib", 
               gradient.cols = c("gray70", "gray50", "gray20"),
               repel = TRUE) +
    labs(title = paste("Projection des Variables (", annee, ")", sep = "")) +
    theme_minimal()
  ggsave(paste0("C:/Users/axeld/OneDrive/Documents/Accident routier/Result/", annee, ".png"), width = 10, height = 8)
  
  # 📊 5. Visualisation des individus
  fviz_pca_ind(res_acp,
               col.ind = "cos2", 
               gradient.cols = c("gray70", "gray50", "gray20"),
               repel = TRUE) +
    labs(title = paste("Projection des Individus (", annee, ")", sep = "")) +
    theme_minimal()
  ggsave(paste0("C:/Users/axeld/OneDrive/Documents/Accident routier/Result/", annee, ".png"), width = 10, height = 8)
  
  # 📊 6. Contribution des variables
  fviz_pca_contrib(res_acp, choice = "var", axes = 1, top = 10) +
    labs(title = paste("Contribution des Variables à la Première Composante (", annee, ")", sep = "")) +
    theme_minimal()
  ggsave(paste0("C:/Users/axeld/OneDrive/Documents/Accident routier/Result/", annee, ".png"), width = 10, height = 8)
  
  fviz_pca_contrib(res_acp, choice = "var", axes = 2, top = 10) +
    labs(title = paste("Contribution des Variables à la Deuxième Composante (", annee, ")", sep = "")) +
    theme_minimal()
  ggsave(paste0("C:/Users/axeld/OneDrive/Documents/Accident routier/Result/", annee, ".png"), width = 10, height = 8)
  
  # 📊 7. Résumé des résultats
  print(summary(res_acp))
  
  cat("\n✅ ACP terminée pour l'année :", annee, "\n")
}
