library(readr)
library(dplyr)
library(randomForest)
library(caret)

############################################# Chargement et préparation des données ############################################


project_dir <- "C:/Users/gosse/Desktop/ENSG/GDS"

caracteristiques2019 = read.csv(paste0(project_dir,"/data/2019_caracteristiques.csv"), sep = ";")
usagers2019 = read.csv(paste0(project_dir,"/data/2019_usagers.csv"), sep = ";")
lieux2019 = read.csv(paste0(project_dir,"/data/2019_lieux.csv"), sep = ";")
vehicules2019 = read.csv(paste0(project_dir,"/data/2019_vehicules.csv"), sep = ";")

caracteristiques2020 = read.csv(paste0(project_dir,"/data/2020_caracteristiques.csv"), sep = ";")
usagers2020 = read.csv(paste0(project_dir,"/data/2020_usagers.csv"), sep = ";")
usagers2020$id_usager <- NULL
lieux2020 = read.csv(paste0(project_dir,"/data/2020_lieux.csv"), sep = ";")
vehicules2020 = read.csv(paste0(project_dir,"/data/2020_vehicules.csv"), sep = ";")

caracteristiques2021 = read.csv(paste0(project_dir,"/data/2021_caracteristiques.csv"), sep = ";")
usagers2021 = read.csv(paste0(project_dir,"/data/2021_usagers.csv"), sep = ";")
usagers2021$id_usager <- NULL
lieux2021 = read.csv(paste0(project_dir,"/data/2021_lieux.csv"), sep = ";")
vehicules2021 = read.csv(paste0(project_dir,"/data/2021_vehicules.csv"), sep = ";")

caracteristiques2022 = read.csv(paste0(project_dir,"/data/2022_caracteristiques.csv"), sep = ";")
names(caracteristiques2022)[names(caracteristiques2022) == "Accident_Id"] <- "Num_Acc"
usagers2022 = read.csv(paste0(project_dir,"/data/2022_usagers.csv"), sep = ";")
usagers2022$id_usager <- NULL
lieux2022 = read.csv(paste0(project_dir,"/data/2022_lieux.csv"), sep = ";")
vehicules2022 = read.csv(paste0(project_dir,"/data/2022_vehicules.csv"), sep = ";")

caracteristiques2023 = read.csv(paste0(project_dir,"/data/2023_caracteristiques.csv"), sep = ";")
usagers2023 = read.csv(paste0(project_dir,"/data/2023_usagers.csv"), sep = ";")
usagers2023$id_usager <- NULL
lieux2023 = read.csv(paste0(project_dir,"/data/2023_lieux.csv"), sep = ";")
vehicules2023 = read.csv(paste0(project_dir,"/data/2023_vehicules.csv"), sep = ";")

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


year_summary_2020[, "grav"] <- NULL
year_summary_2022[, "grav"] <- NULL
year_summary_2020 <- year_summary_2020[ , -1]
year_summary_2022 <- year_summary_2022[ , -1]


all_year_summary[, "grav"] <- NULL
all_year_summary <- all_year_summary[ , -1]



############################################# RANDOM FOREST ############################################

#Sans équilibrage des données
rf <- function(year_summary) {
  # Sélectionner aléatoirement les indices pour le train et le test
  set.seed(123) # Pour répétabilité
  
  # Créer les ensembles d'entraînement et de test en faisant en sorte que la proportion de mort 
  # ou non soit équitablement répartie (ex: environ +/-75% des accidents mortels dans train_data et +/-25% dans 
  # test_data en fonction de la taille de l'ensemble, pareil pour les accidents non mortels)
  indices <- createDataPartition(year_summary$mortel, p = 0.75, list = FALSE)
  train_data <- year_summary[indices, ]
  test_data <- year_summary[-indices, ]
  
  
  model = randomForest(mortel  ~ ., data = train_data, ntree = 500, do.trace = 100, maxdepth = 5, type = "classification", importance = TRUE, seed = 123)
  varImpPlot(model)
  predictions_model <- predict(model, newdata = test_data, type = "class")
  
  
  # Préparer les prédictions et les vérités terrain
  predictions <- ifelse(predictions_model >= 0.5, 1, 0)
  true_values <- test_data$mortel
  
  levels_unique <- unique(c(predictions, true_values))
  
  predictions_factor <- factor(predictions, levels = levels_unique)
  true_values_factor <- factor(true_values, levels = levels_unique)
  
  
  # Créer la matrice de confusion
  confusion_matrix <- confusionMatrix(data = predictions_factor, reference = true_values_factor)
  
  # Afficher la matrice de confusion et les statistiques de performance
  print(confusion_matrix)
  
  f1_score <- confusion_matrix$byClass["F1"]
  print(f1_score)
  
  return()
}

#Avec équilibrage des données

balanced_rf <- function(year_summary){
  set.seed(123) # Pour répétabilité
  
  mortel_1_indices <- sample(which(all_year_summary$mortel == 1), sum(year_summary$mortel == 1, na.rm = TRUE))
  mortel_0_indices <- sample(which(all_year_summary$mortel == 0), sum(year_summary$mortel == 1, na.rm = TRUE))
  
  # Combiner les indices
  all_indices <- c(mortel_1_indices, mortel_0_indices)
  
  # Sélectionner les lignes correspondantes
  selected_data <- all_year_summary %>% slice(all_indices)
  
  # Partitionner les données en 75% pour l'entraînement et 25% pour le test
  train_indices <- sample(nrow(selected_data), 0.75 * nrow(selected_data))
  train_set <- selected_data %>% slice(train_indices)
  test_set <- selected_data %>% slice(-train_indices)
  
  
  model = randomForest(mortel  ~ ., data = train_set, ntree = 500, do.trace = 100, maxdepth = 5, type = "classification", importance = TRUE, seed = 123)
  varImpPlot(model)
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
  return()
  
}
############################################# RANDOM FOREST années 2020 et 2022 séparées ############################################

# Non-balanced dataset for 2020
rf(year_summary_2020)

# Balanced dataset for 2020
balanced_rf(year_summary_2020)

# Balanced dataset for 2022
balanced_rf(year_summary_2022)


############################################# RANDOM FOREST 2019 - 2023 ############################################

balanced_rf(all_year_summary)

############################################# CROSS VALIDATION et BOOTSTRAP ############################################

set.seed(123) # Pour répétabilité
mortel_1_indices <- sample(which(all_year_summary$mortel == 1), sum(all_year_summary$mortel == 1, na.rm = TRUE))
mortel_0_indices <- sample(which(all_year_summary$mortel == 0), sum(all_year_summary$mortel == 1, na.rm = TRUE))

#                                            ####CROSS VALIDATION####

# Combiner les indices
all_indices <- c(mortel_1_indices, mortel_0_indices)

# Sélectionner les lignes correspondantes
selected_data <- all_year_summary %>% slice(all_indices)


# Partition des données pour validation croisée
selected_data <- selected_data %>% sample_n(nrow(selected_data))
n_rows_per_df <- floor(nrow(selected_data) / 8)

# Répartition explicite en 4 groupes égaux
selected_data_list <- split(selected_data, 
                            rep(1:8, each = n_rows_per_df, length.out = nrow(selected_data)))

accuracies <- numeric(8)
f1score <- numeric(8)

for (i in 1:8) {
  # Sélectionner les ensembles de formation et de test
  training_indices <- c(1:8)[-i]
  training_data <- do.call(rbind, selected_data_list[training_indices])
  test_data <- selected_data_list[[i]]
  
  # Assurez-vous que 'mortel' est un facteur avec les niveaux appropriés
  training_data$mortel <- factor(training_data$mortel, levels = c(0, 1))
  
  # Entraîner le modèle randomForest
  model = randomForest(mortel ~ ., data = training_data, ntree = 500, do.trace = 100, maxdepth = 5, type = "classification")
  
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
  f1score[i] <- confusion_matrix$byClass["F1"]
  accuracies[i] <- confusion_matrix$overall['Accuracy']
}




# Afficher les résultats de la cross-validation

# Calculer la précision moyenne et l'écart-type
mean_accuracy <- mean(accuracies)
sd_accuracy <- sd(accuracies)

mean_fscore <- mean(f1score)
sd_fscore <- sd(f1score)

print(paste("Précision moyenne:", mean_accuracy))
print(paste("Écart-type de la précision:", sd_accuracy))

print(paste("F1 score moyen:", mean_fscore))
print(paste("Écart-type du f1score:", sd_fscore))



#                                            ####BOOTSTRAP####

# Nombre de répétitions pour le bootstrapping
n_bootstraps <- 25

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




