######################################################################################################
# SECTION 1 : TEST DU CHI�
######################################################################################################

# Chargement des biblioth�ques et des donn�es
library(dplyr)
caracteristiques <- read.csv("D:/Accidents_routiers/data/2023_caracteristiques.csv", sep = ";", stringsAsFactors = TRUE)

# V�rifier les colonnes n�cessaires
if (!"lum" %in% colnames(caracteristiques)) {
  stop("La colonne 'lum' est absente du fichier 'caracteristiques'.")
}

# Table de contingence pour le test du Chi�
table_chi2 <- table(caracteristiques$lum)

# Test du Chi�
chi2_test <- chisq.test(table_chi2)

# R�sultats du test
cat("R�sultats du test du Chi� (lum) :\n")
print(chi2_test)


######################################################################################################
# SECTION 2 : TEST DU FISHER
######################################################################################################

# Chargement des biblioth�ques et des donn�es
library(dplyr)
caracteristiques <- read.csv("D:/Accidents_routiers/data/2023_caracteristiques.csv", sep = ";", stringsAsFactors = TRUE)

# V�rifier les colonnes n�cessaires
if (!"lum" %in% colnames(caracteristiques) || !"agg" %in% colnames(caracteristiques)) {
  stop("Les colonnes 'lum' et 'agg' sont n�cessaires.")
}

# Regroupement des cat�gories pour r�duire la taille de la table
caracteristiques <- caracteristiques %>%
  mutate(
    lum_grouped = ifelse(lum %in% c(1, 2), "Jour", "Nuit"), # Lum regroup� en "Jour" et "Nuit"
    agg_grouped = ifelse(agg == 1, "Hors agglom�ration", "Agglom�ration") # Agg regroup�
  )

# Table de contingence avec les variables regroup�es
table_reduced <- table(caracteristiques$lum_grouped, caracteristiques$agg_grouped)

# V�rifier la taille de la table
cat("Dimensions de la table de contingence regroup�e :\n")
print(dim(table_reduced))

# V�rifier si la table est petite (par exemple 2x2) ou n�cessite une simulation
if (all(dim(table_reduced) <= c(2, 2))) {
  cat("Ex�cution du test exact de Fisher sur la table regroup�e...\n")
  fisher_test <- fisher.test(table_reduced)
} else {
  cat("La table est trop grande, ex�cution du test exact de Fisher avec simulation Monte Carlo...\n")
  fisher_test <- fisher.test(table_reduced, simulate.p.value = TRUE, B = 1e6) # B contr�le le nombre de simulations
}

# R�sultats du test
cat("R�sultats du test exact de Fisher (lum_grouped vs agg_grouped) :\n")
print(fisher_test)


######################################################################################################
# SECTION 3 : TEST DE ANOVA
######################################################################################################

# Chargement des biblioth�ques et des donn�es 
library(dplyr)
library(ggplot2)

# Charger les donn�es
caracteristiques <- read.csv("D:/Accidents_routiers/data/2023_caracteristiques.csv", sep = ";", stringsAsFactors = TRUE)
lieux <- read.csv("D:/Accidents_routiers/data/2023_lieux.csv", sep = ";", stringsAsFactors = TRUE)

# V�rification des colonnes n�cessaires dans les deux fichiers
cat("Colonnes disponibles dans 'caracteristiques' :\n")
print(colnames(caracteristiques))

cat("Colonnes disponibles dans 'lieux' :\n")
print(colnames(lieux))

if (!"lum" %in% colnames(caracteristiques)) {
  stop("La colonne 'lum' est absente des donn�es de 'caracteristiques'.")
}

if (!"vma" %in% colnames(lieux)) {
  stop("La colonne 'vma' est absente des donn�es de 'lieux'.")
}

# Fusionner les deux jeux de donn�es sur la cl� commune 'Num_Acc'
cat("Fusion des donn�es 'caracteristiques' et 'lieux'...\n")
data <- caracteristiques %>%
  inner_join(lieux, by = "Num_Acc")

# V�rifier les colonnes apr�s fusion
cat("Colonnes disponibles apr�s fusion :\n")
print(colnames(data))

# V�rifier les valeurs manquantes ou incoh�rentes dans 'lum' et 'vma'
cat("R�sum� des colonnes 'lum' et 'vma' :\n")
summary(data[c("lum", "vma")])

# Nettoyer les donn�es en supprimant les lignes avec des valeurs manquantes
data <- data %>%
  filter(!is.na(lum) & !is.na(vma))

# V�rification des donn�es apr�s nettoyage
cat("R�sum� apr�s nettoyage des colonnes 'lum' et 'vma' :\n")
summary(data[c("lum", "vma")])

# Convertir 'lum' en facteur et 'vma' en num�rique si n�cessaire
data <- data %>%
  mutate(
    lum = as.factor(lum),
    vma = as.numeric(vma)
  )

# V�rification des cat�gories de 'lum'
cat("Cat�gories de 'lum' :\n")
print(levels(data$lum))

# Effectuer l'ANOVA
cat("Ex�cution de l'ANOVA pour 'vma' en fonction de 'lum'...\n")
anova_result <- aov(vma ~ lum, data = data)

# R�sultats de l'ANOVA
cat("R�sum� des r�sultats de l'ANOVA :\n")
summary(anova_result)

# Moyennes de 'vma' par cat�gorie de 'lum'
cat("Moyennes de 'vma' par cat�gorie de 'lum' :\n")
means <- data %>%
  group_by(lum) %>%
  summarize(mean_vma = mean(vma, na.rm = TRUE))
print(means)

# Tracer un graphique des moyennes de 'vma' par 'lum'
cat("Cr�ation du graphique des moyennes...\n")
ggplot(data = means, aes(x = lum, y = mean_vma)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Moyennes de la vitesse maximale autoris�e (vma) par cat�gorie de lumi�re (lum)",
    x = "Cat�gorie de lumi�re (lum)",
    y = "Vitesse maximale autoris�e moyenne (vma)"
  ) +
  theme_minimal()



######################################################################################################
# SECTION 4 : REGRESSION LOGISTIQUE 
######################################################################################################

# Chargement des biblioth�ques et des donn�es
library(dplyr)
library(caret)

usagers <- read.csv("D:/Accidents_routiers/data/2023_usagers.csv", sep = ";", stringsAsFactors = TRUE)
caracteristiques <- read.csv("D:/Accidents_routiers/data/2023_caracteristiques.csv", sep = ";", stringsAsFactors = TRUE)
lieux <- read.csv("D:/Accidents_routiers/data/2023_lieux.csv", sep = ";", stringsAsFactors = TRUE)

# V�rification des colonnes n�cessaires
cat("V�rification des colonnes n�cessaires dans les tables...\n")
if (!"grav" %in% colnames(usagers)) {
  stop("La colonne 'grav' est manquante dans la table des usagers.")
}
if (!all(c("lum", "atm", "agg") %in% colnames(caracteristiques))) {
  stop("Les colonnes n�cessaires (lum, atm, agg) ne sont pas toutes pr�sentes dans la table des caract�ristiques.")
}
if (!"vma" %in% colnames(lieux)) {
  stop("La colonne 'vma' est manquante dans la table des lieux.")
}

# Fusion des donn�es sur la cl� commune `Num_Acc`
cat("Fusion des tables usagers, caract�ristiques et lieux sur 'Num_Acc'...\n")
data <- usagers %>%
  inner_join(caracteristiques, by = "Num_Acc") %>%
  inner_join(lieux, by = "Num_Acc")

# Pr�paration des donn�es pour la r�gression logistique
cat("Transformation de la colonne `grav` en binaire `grave`...\n")
data_model <- data %>%
  mutate(
    grave = ifelse(as.numeric(grav) >= 3, 1, 0) # Transformer `grav` en binaire (1 = grave, 0 = non grave)
  ) %>%
  select(grave, lum, atm, agg, catr, vma) %>%
  filter(complete.cases(.)) %>% # Supprimer les lignes avec des valeurs manquantes
  mutate(across(-grave, ~ as.numeric(as.factor(.)))) # Convertir les colonnes explicatives en num�riques

# V�rifiez la structure des donn�es pr�par�es
cat("Aper�u des donn�es pr�par�es pour le mod�le :\n")
print(head(data_model))

# V�rifiez la distribution de la variable cible `grave`
cat("Distribution de la variable cible `grave` :\n")
print(table(data_model$grave))

# Partition des donn�es en train et test
cat("Partition des donn�es en train et test...\n")
set.seed(123) # Pour la reproductibilit�
index <- sample(1:nrow(data_model), size = 0.7 * nrow(data_model))
train_data <- data_model[index, ]
test_data <- data_model[-index, ]

# V�rifiez les valeurs uniques de `grave` dans les donn�es d'entra�nement
print(unique(train_data$grave))

# Construction du mod�le de r�gression logistique
cat("Construction du mod�le de r�gression logistique...\n")
modele_logistique <- glm(grave ~ ., data = train_data, family = binomial)

# R�sum� du mod�le
cat("R�sum� du mod�le de r�gression logistique :\n")
summary(modele_logistique)

# Pr�dictions sur les donn�es de test
cat("R�alisation des pr�dictions sur les donn�es de test...\n")
predictions <- predict(modele_logistique, newdata = test_data, type = "response")
test_data$predicted <- ifelse(predictions > 0.5, 1, 0)

# �valuation des performances
cat("�valuation des performances avec la matrice de confusion...\n")
confusion_matrix <- confusionMatrix(as.factor(test_data$predicted), as.factor(test_data$grave))

# Affichage de la matrice de confusion
cat("Matrice de confusion :\n")
print(confusion_matrix)

