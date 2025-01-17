######################################################################################################
# SECTION 1 : TEST DU CHI²
######################################################################################################

# Chargement des bibliothèques et des données
library(dplyr)
caracteristiques <- read.csv("D:/Accidents_routiers/data/2023_caracteristiques.csv", sep = ";", stringsAsFactors = TRUE)

# Vérifier les colonnes nécessaires
if (!"lum" %in% colnames(caracteristiques)) {
  stop("La colonne 'lum' est absente du fichier 'caracteristiques'.")
}

# Table de contingence pour le test du Chi²
table_chi2 <- table(caracteristiques$lum)

# Test du Chi²
chi2_test <- chisq.test(table_chi2)

# Résultats du test
cat("Résultats du test du Chi² (lum) :\n")
print(chi2_test)


######################################################################################################
# SECTION 2 : TEST DU FISHER
######################################################################################################

# Chargement des bibliothèques et des données
library(dplyr)
caracteristiques <- read.csv("D:/Accidents_routiers/data/2023_caracteristiques.csv", sep = ";", stringsAsFactors = TRUE)

# Vérifier les colonnes nécessaires
if (!"lum" %in% colnames(caracteristiques) || !"agg" %in% colnames(caracteristiques)) {
  stop("Les colonnes 'lum' et 'agg' sont nécessaires.")
}

# Regroupement des catégories pour réduire la taille de la table
caracteristiques <- caracteristiques %>%
  mutate(
    lum_grouped = ifelse(lum %in% c(1, 2), "Jour", "Nuit"), # Lum regroupé en "Jour" et "Nuit"
    agg_grouped = ifelse(agg == 1, "Hors agglomération", "Agglomération") # Agg regroupé
  )

# Table de contingence avec les variables regroupées
table_reduced <- table(caracteristiques$lum_grouped, caracteristiques$agg_grouped)

# Vérifier la taille de la table
cat("Dimensions de la table de contingence regroupée :\n")
print(dim(table_reduced))

# Vérifier si la table est petite (par exemple 2x2) ou nécessite une simulation
if (all(dim(table_reduced) <= c(2, 2))) {
  cat("Exécution du test exact de Fisher sur la table regroupée...\n")
  fisher_test <- fisher.test(table_reduced)
} else {
  cat("La table est trop grande, exécution du test exact de Fisher avec simulation Monte Carlo...\n")
  fisher_test <- fisher.test(table_reduced, simulate.p.value = TRUE, B = 1e6) # B contrôle le nombre de simulations
}

# Résultats du test
cat("Résultats du test exact de Fisher (lum_grouped vs agg_grouped) :\n")
print(fisher_test)


######################################################################################################
# SECTION 3 : TEST DE ANOVA
######################################################################################################

# Chargement des bibliothèques et des données 
library(dplyr)
library(ggplot2)

# Charger les données
caracteristiques <- read.csv("D:/Accidents_routiers/data/2023_caracteristiques.csv", sep = ";", stringsAsFactors = TRUE)
lieux <- read.csv("D:/Accidents_routiers/data/2023_lieux.csv", sep = ";", stringsAsFactors = TRUE)

# Vérification des colonnes nécessaires dans les deux fichiers
cat("Colonnes disponibles dans 'caracteristiques' :\n")
print(colnames(caracteristiques))

cat("Colonnes disponibles dans 'lieux' :\n")
print(colnames(lieux))

if (!"lum" %in% colnames(caracteristiques)) {
  stop("La colonne 'lum' est absente des données de 'caracteristiques'.")
}

if (!"vma" %in% colnames(lieux)) {
  stop("La colonne 'vma' est absente des données de 'lieux'.")
}

# Fusionner les deux jeux de données sur la clé commune 'Num_Acc'
cat("Fusion des données 'caracteristiques' et 'lieux'...\n")
data <- caracteristiques %>%
  inner_join(lieux, by = "Num_Acc")

# Vérifier les colonnes après fusion
cat("Colonnes disponibles après fusion :\n")
print(colnames(data))

# Vérifier les valeurs manquantes ou incohérentes dans 'lum' et 'vma'
cat("Résumé des colonnes 'lum' et 'vma' :\n")
summary(data[c("lum", "vma")])

# Nettoyer les données en supprimant les lignes avec des valeurs manquantes
data <- data %>%
  filter(!is.na(lum) & !is.na(vma))

# Vérification des données après nettoyage
cat("Résumé après nettoyage des colonnes 'lum' et 'vma' :\n")
summary(data[c("lum", "vma")])

# Convertir 'lum' en facteur et 'vma' en numérique si nécessaire
data <- data %>%
  mutate(
    lum = as.factor(lum),
    vma = as.numeric(vma)
  )

# Vérification des catégories de 'lum'
cat("Catégories de 'lum' :\n")
print(levels(data$lum))

# Effectuer l'ANOVA
cat("Exécution de l'ANOVA pour 'vma' en fonction de 'lum'...\n")
anova_result <- aov(vma ~ lum, data = data)

# Résultats de l'ANOVA
cat("Résumé des résultats de l'ANOVA :\n")
summary(anova_result)

# Moyennes de 'vma' par catégorie de 'lum'
cat("Moyennes de 'vma' par catégorie de 'lum' :\n")
means <- data %>%
  group_by(lum) %>%
  summarize(mean_vma = mean(vma, na.rm = TRUE))
print(means)

# Tracer un graphique des moyennes de 'vma' par 'lum'
cat("Création du graphique des moyennes...\n")
ggplot(data = means, aes(x = lum, y = mean_vma)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Moyennes de la vitesse maximale autorisée (vma) par catégorie de lumière (lum)",
    x = "Catégorie de lumière (lum)",
    y = "Vitesse maximale autorisée moyenne (vma)"
  ) +
  theme_minimal()



######################################################################################################
# SECTION 4 : REGRESSION LOGISTIQUE 
######################################################################################################

# Chargement des bibliothèques et des données
library(dplyr)
library(caret)

usagers <- read.csv("D:/Accidents_routiers/data/2023_usagers.csv", sep = ";", stringsAsFactors = TRUE)
caracteristiques <- read.csv("D:/Accidents_routiers/data/2023_caracteristiques.csv", sep = ";", stringsAsFactors = TRUE)
lieux <- read.csv("D:/Accidents_routiers/data/2023_lieux.csv", sep = ";", stringsAsFactors = TRUE)

# Vérification des colonnes nécessaires
cat("Vérification des colonnes nécessaires dans les tables...\n")
if (!"grav" %in% colnames(usagers)) {
  stop("La colonne 'grav' est manquante dans la table des usagers.")
}
if (!all(c("lum", "atm", "agg") %in% colnames(caracteristiques))) {
  stop("Les colonnes nécessaires (lum, atm, agg) ne sont pas toutes présentes dans la table des caractéristiques.")
}
if (!"vma" %in% colnames(lieux)) {
  stop("La colonne 'vma' est manquante dans la table des lieux.")
}

# Fusion des données sur la clé commune `Num_Acc`
cat("Fusion des tables usagers, caractéristiques et lieux sur 'Num_Acc'...\n")
data <- usagers %>%
  inner_join(caracteristiques, by = "Num_Acc") %>%
  inner_join(lieux, by = "Num_Acc")

# Préparation des données pour la régression logistique
cat("Transformation de la colonne `grav` en binaire `grave`...\n")
data_model <- data %>%
  mutate(
    grave = ifelse(as.numeric(grav) >= 3, 1, 0) # Transformer `grav` en binaire (1 = grave, 0 = non grave)
  ) %>%
  select(grave, lum, atm, agg, catr, vma) %>%
  filter(complete.cases(.)) %>% # Supprimer les lignes avec des valeurs manquantes
  mutate(across(-grave, ~ as.numeric(as.factor(.)))) # Convertir les colonnes explicatives en numériques

# Vérifiez la structure des données préparées
cat("Aperçu des données préparées pour le modèle :\n")
print(head(data_model))

# Vérifiez la distribution de la variable cible `grave`
cat("Distribution de la variable cible `grave` :\n")
print(table(data_model$grave))

# Partition des données en train et test
cat("Partition des données en train et test...\n")
set.seed(123) # Pour la reproductibilité
index <- sample(1:nrow(data_model), size = 0.7 * nrow(data_model))
train_data <- data_model[index, ]
test_data <- data_model[-index, ]

# Vérifiez les valeurs uniques de `grave` dans les données d'entraînement
print(unique(train_data$grave))

# Construction du modèle de régression logistique
cat("Construction du modèle de régression logistique...\n")
modele_logistique <- glm(grave ~ ., data = train_data, family = binomial)

# Résumé du modèle
cat("Résumé du modèle de régression logistique :\n")
summary(modele_logistique)

# Prédictions sur les données de test
cat("Réalisation des prédictions sur les données de test...\n")
predictions <- predict(modele_logistique, newdata = test_data, type = "response")
test_data$predicted <- ifelse(predictions > 0.5, 1, 0)

# Évaluation des performances
cat("Évaluation des performances avec la matrice de confusion...\n")
confusion_matrix <- confusionMatrix(as.factor(test_data$predicted), as.factor(test_data$grave))

# Affichage de la matrice de confusion
cat("Matrice de confusion :\n")
print(confusion_matrix)

