library(readr)
library(dplyr)
library(randomForest)
library(caret)

############################################# Chargement et préparation des données ############################################


caracteristiques2019 = read.csv("C:/Joanna/Projet Stat/data/2019_caracteristiques.csv", sep = ";")
usagers2019 = read.csv("C:/Joanna/Projet Stat/data/2019_usagers.csv", sep = ";")
lieux2019 = read.csv("C:/Joanna/Projet Stat/data/2019_lieux.csv", sep = ";")
vehicules2019 = read.csv("C:/Joanna/Projet Stat/data/2019_vehicules.csv", sep = ";")

caracteristiques2020 = read.csv("C:/Joanna/Projet Stat/data/2020_caracteristiques.csv", sep = ";")
usagers2020 = read.csv("C:/Joanna/Projet Stat/data/2020_usagers.csv", sep = ";")
usagers2020$id_usager <- NULL
lieux2020 = read.csv("C:/Joanna/Projet Stat/data/2020_lieux.csv", sep = ";")
vehicules2020 = read.csv("C:/Joanna/Projet Stat/data/2020_vehicules.csv", sep = ";")

caracteristiques2021 = read.csv("C:/Joanna/Projet Stat/data/2021_caracteristiques.csv", sep = ";")
usagers2021 = read.csv("C:/Joanna/Projet Stat/data/2021_usagers.csv", sep = ";")
usagers2021$id_usager <- NULL
lieux2021 = read.csv("C:/Joanna/Projet Stat/data/2021_lieux.csv", sep = ";")
vehicules2021 = read.csv("C:/Joanna/Projet Stat/data/2021_vehicules.csv", sep = ";")

caracteristiques2022 = read.csv("C:/Joanna/Projet Stat/data/2022_caracteristiques.csv", sep = ";")
names(caracteristiques2022)[names(caracteristiques2022) == "Accident_Id"] <- "Num_Acc"
usagers2022 = read.csv("C:/Joanna/Projet Stat/data/2022_usagers.csv", sep = ";")
usagers2022$id_usager <- NULL
lieux2022 = read.csv("C:/Joanna/Projet Stat/data/2022_lieux.csv", sep = ";")
vehicules2022 = read.csv("C:/Joanna/Projet Stat/data/2022_vehicules.csv", sep = ";")

caracteristiques2023 = read.csv("C:/Joanna/Projet Stat/data/2023_caracteristiques.csv", sep = ";")
usagers2023 = read.csv("C:/Joanna/Projet Stat/data/2023_usagers.csv", sep = ";")
usagers2023$id_usager <- NULL
lieux2023 = read.csv("C:/Joanna/Projet Stat/data/2023_lieux.csv", sep = ";")
vehicules2023 = read.csv("C:/Joanna/Projet Stat/data/2023_vehicules.csv", sep = ";")

usagers = rbind(usagers2019, usagers2020, usagers2021, usagers2022, usagers2023)
usagers = na.omit(usagers)

caracteristiques = rbind(caracteristiques2019, caracteristiques2020, caracteristiques2021, caracteristiques2022, caracteristiques2023)
caracteristiques = na.omit(caracteristiques)

lieux = rbind(lieux2019, lieux2020, lieux2021, lieux2022, lieux2023)
lieux = na.omit(lieux)

vehicules = rbind(vehicules2019, vehicules2020, vehicules2021, vehicules2022, vehicules2023)
vehicules = na.omit(vehicules)



year_summary <- function(usagers, vehicules, lieux, caracteristiques){

  #Agréger les informations des usagers, on verra ailleurs si on veut utiliser age, sexe, place, catégorie ou motif trajet
  usagers_summary <- usagers %>%
    group_by(Num_Acc) %>%
    summarize(
      mortel = ifelse(grav == 2, 1, 0),   # Vérification si l'accident est mortel ou non
    )
  
  #Agréger les informations des véhicules : on ne prend pas catv et motor car plusieurs véhicules impliqués dans certains accidents
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
      nb_vehicules = coalesce(nb_vehicules, 0), #Remplacer NA par 0 pour les accidents sans véhicules
    )
  data_accidents_complet = data_accidents_complet[!duplicated(data_accidents_complet$Num_Acc), ] #Pour faire en sorte que chaque accident est unique

  return(data_accidents_complet)
}


year_summary_2019 = year_summary(usagers2019, vehicules2019, lieux2019, caracteristiques2019)
year_summary_2020 = year_summary(usagers2020, vehicules2020, lieux2020, caracteristiques2020)
year_summary_2021 = year_summary(usagers2021, vehicules2021, lieux2021, caracteristiques2021)
year_summary_2022 = year_summary(usagers2022, vehicules2022, lieux2022, caracteristiques2022)
year_summary_2023 = year_summary(usagers2023, vehicules2023, lieux2023, caracteristiques2023)

all_year_summary = rbind(year_summary_2019, year_summary_2020, year_summary_2021, year_summary_2022, year_summary_2023)


all_year_summary[,"grav"] <- NULL


############################################# MODELE LINEAIRE LM? LOGIT ET GLM ############################################

model1_result = lm(formula = mortel ~ place + catu + sexe + an_nais + trajet + secu1 + secu2 + secu3 + locp + actp + etatp, data = all_year_summary)
summary(model1_result)


#GLM
logit_model = glm(formula = mortel ~ place + catu + sexe + an_nais + trajet + secu1 + secu2 + secu3 + locp + actp + etatp, data = all_year_summary, family = binomial(link = 'logit'))

logit2_result = glm(formula = mortel ~ 1, data = all_year_summary, family = binomial(link = 'logit'))


McFadden = 1 - logLik(logit_model)/logLik(logit2_result)


#CLASSIFICATION ET ACCURACY AVEC LOGIT MODEL

accuracy<- function(model){
  length(which(ifelse(fitted(model)>0.5,1,0)==all_year_summary$mortel))/length(fitted(model))
}


accuracy(logit_model)

f1_score <- function(model,threshold) {
  # Prévisions du modèle
  predictions_prob <- predict(model, type = "response")
  predictions_class <- ifelse(predictions_prob > threshold, 1, 0)
  
  # Labels réels
  actual_labels <- model$data$indicatrice_grav
  
  # Créer la matrice de confusion
  confusion_mat <- table(Predicted = predictions_class, Actual = actual_labels)

  # Calculer la précision et le rappel
  true_positives <- confusion_mat[1, 1]
  false_positives <- confusion_mat[1, 2]
  false_negatives <- confusion_mat[2, 1]
  
  precision <- true_positives / (true_positives + false_positives)
  recall <- true_positives / (true_positives + false_negatives)
  
  # Calculer le F1 score
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  return(f1)
}


fscore = f1_score(logit_model)

thetas = seq(0.2,0.4,0.01)
scores = sapply(thetas,function(theta){f1_score(logit_model,theta)})

plot(thetas,scores)

#analyse en composantes principales / matrice de correlation / gwr / balanced accuracy tp6 correction + f1




############################################# RANDOM FOREST ############################################


# Sélectionner aléatoirement les indices pour le train et le test
set.seed(123) # Pour répétabilité

mortel_1_indices <- sample(which(all_year_summary$mortel == 1), 9584)
mortel_0_indices <- sample(which(all_year_summary$mortel == 0), 9584)

# Combiner les indices
all_indices <- c(mortel_1_indices, mortel_0_indices)

# Sélectionner les lignes correspondantes
selected_data <- all_year_summary %>% slice(all_indices)

# Partitionner les données en 75% pour l'entraînement et 25% pour le test
train_indices <- sample(nrow(selected_data), 0.75 * nrow(selected_data))
train_set <- selected_data %>% slice(train_indices)
test_set <- selected_data %>% slice(-train_indices)




"train_indices <- createDataPartition(all_year_summary$mortel, p = 0.75, list = FALSE)

# Créer les sous-ensembles en respectant la distribution
train_set <- all_year_summary %>% slice(train_indices)
test_set <- all_year_summary %>% slice(-train_indices)"

model = randomForest(mortel  ~ ., data = train_set, ntree = 500, do.trace = 100, maxdepth = 5, type = "classification")

predictions_model <- predict(model, newdata = test_set, type = "class")




# Préparer les prédictions et les vérités terrain
predictions <- ifelse(predictions_model >= 0.5, 1, 0)
true_values <- test_set$mortel

levels_unique <- unique(c(predictions, true_values))

predictions_factor <- factor(predictions, levels = levels_unique)
true_values_factor <- factor(true_values, levels = levels_unique)


# Créer la matrice de confusion
confusion_matrix <- confusionMatrix(data = predictions_factor, reference = true_values_factor)

# Afficher la matrice de confusion et les statistiques de performance
print(confusion_matrix)

f1_score <- confusion_matrix$byClass["F1"]
print(f1_score)






############################################# CROSS VALIDATION ############################################


# Réduire le nombre de données pour avoir un nombre de lignes équivalent entre les accidents mortels et non mortels
set.seed(123)

mortel_1_indices <- sample(which(all_year_summary$mortel == 1), 9584)
mortel_0_indices <- sample(which(all_year_summary$mortel == 0), 9584)
all_indices <- c(mortel_1_indices, mortel_0_indices)
selected_data <- all_year_summary %>% slice(all_indices)

# Partition des données pour validation croisée
selected_data <- selected_data %>% sample_n(nrow(selected_data))
n_rows_per_df <- floor(nrow(selected_data) / 4)

# Répartition explicite en 4 groupes égaux
selected_data_list <- split(selected_data, 
                            rep(1:4, each = n_rows_per_df, length.out = nrow(selected_data)))

lst_cm = list()
for (i in 1:4) {
  # Sélectionner les ensembles de formation et de test
  training_indices <- c(1:4)[-i]
  training_data <- do.call(rbind, selected_data_list[training_indices])
  test_data <- selected_data_list[[i]]
  
  # Assurez-vous que 'mortel' est un facteur avec les niveaux appropriés
  training_data$mortel <- factor(training_data$mortel, levels = c(0, 1))
  
  # Entraîner le modèle randomForest
  model = randomForest(mortel ~ ., data = training_data, ntree = 100, do.trace = 25, maxdepth = 5, type = "classification")
  
  # Effectuer les prédictions
  predictions_model <- predict(model, newdata = test_data, type = "class")
  
  # Convertir les prédictions en facteur avec les niveaux corrects
  predictions <- predictions_model  # Pas besoin de ifelse, car 'predictions_model' est déjà un facteur
  
  # Préparer les vérités terrain (true_values)
  true_values <- test_data$mortel
  true_values_factor <- factor(true_values, levels = c(0, 1))
  
  # Créer la matrice de confusion
  confusion_matrix <- confusionMatrix(data = predictions, reference = true_values_factor)
  
  # Afficher la matrice de confusion et les statistiques de performance
  print(confusion_matrix)
  
  # Stocker la matrice de confusion dans la liste
  lst_cm = c(lst_cm, confusion_matrix)
}




# Afficher les résultats de la cross-validation
print(model)

# Afficher les scores de précision pour chaque pli
print(model$resample)

# Calculer la précision moyenne et l'écart-type
mean_accuracy <- mean(model$resample$Accuracy)
sd_accuracy <- sd(model$resample$Accuracy)
print(paste("Précision moyenne:", mean_accuracy))
print(paste("Écart-type de la précision:", sd_accuracy))



############################################# BOOTSTRAP ############################################



"   BOOTSTRAP 
set.seed(123) # Pour répétabilité
mortel_1_indices <- sample(which(all_year_summary$mortel == 1), 9584)
mortel_0_indices <- sample(which(all_year_summary$mortel == 0), 9584)

# Combiner les indices
all_indices <- c(mortel_1_indices, mortel_0_indices)

# Sélectionner les lignes correspondantes
selected_data <- all_year_summary %>% slice(all_indices)

# Nombre de répétitions pour le bootstrapping
n_bootstraps <- 100

# Initialiser les vecteurs pour stocker les résultats
f1_scores <- rep(NA, n_bootstraps)

# Boucle de bootstrapping
for (i in 1:n_bootstraps) {
  # Rééchantillonnage avec remplacement
  bootstrap_indices <- sample(nrow(selected_data), nrow(selected_data), replace = TRUE)
  bootstrap_data <- selected_data %>% slice(bootstrap_indices)

  # Partitionner les données en 75% pour l'entraînement et 25% pour le test
  train_indices <- sample(nrow(bootstrap_data), 0.75 * nrow(bootstrap_data))
  train_set <- bootstrap_data %>% slice(train_indices)
  test_set <- bootstrap_data %>% slice(-train_indices)

  # Construire le modèle de forêt aléatoire
  model <- randomForest(mortel ~ ., data = train_set, ntree = 500, do.trace = 100, maxdepth = 5, type = "classification")

  # Faire des prédictions
  predictions_model <- predict(model, newdata = test_set, type = "class")

  # Préparer les prédictions et les vérités terrain
  predictions <- ifelse(predictions_model == "1", 1, 0)
  true_values <- test_set$mortel

  levels_unique <- unique(c(predictions, true_values))
  predictions_factor <- factor(predictions, levels = levels_unique)
  true_values_factor <- factor(true_values, levels = levels_unique)

  # Créer la matrice de confusion
  confusion_matrix <- confusionMatrix(data = predictions_factor, reference = true_values_factor)

  # Stocker le F1-score
  f1_scores[i] <- confusion_matrix$byClass["F1"]
}

# Afficher les résultats
print(mean(f1_scores)) # Moyenne des F1-scores
print(sd(f1_scores))   # Écart-type des F1-scores
print(quantile(f1_scores)) # Quantiles des F1-scores
"



