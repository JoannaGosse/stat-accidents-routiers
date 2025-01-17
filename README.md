# Analyse des accidents de la circulation : tendances et facteurs déterminants (2019-2023)

## Contexte
Nous avons réalisée cette étude dans le cadre d'un projet de statistiques commandité par Yann Ménéroux et Juste Raimbault.

## Objectifs
Utiliser nos compétences en analyses statistiques et machine learning pour analyser des données. Nous avons choisi les données des accidents routiers de 2019 à 2024 qui sont en libres accès sur data.gouv.

## Pré-requis
Avant d'utiliser ces fichiers R permettant de calculer plusieurs indicateurs statistiques et de faire des traitements spécifiques, téléchargez tous les fichiers nécessaires grace au script Python download_data.py !


## Architecture du rendu

- Un fichier README.Md
- 
### Dossier annexes
- Un fichier description-des-bases-de-donnees-annuelles-2.pdf: métadonnées des données téléchargées avec les informations correspondant à chaque attribut
- Un fichier rapport_stat_accidents_routiers.pdf contenant le résumé de notre étude
- Un fichier PresentationStat.pdf contenant le support de notre présentation orale

### Dossier download_data
- Un fichier csv download_links.csv contenant les liens utilisés par le fichier python pour télécharger les données
- Un fichier python download_data.py permettant de télécharger automatique toutes les données nécessaires à notre étude

### Dossier Code

#### Dossier Analyses statistiques et descriptives
- table_accidents_carac.R : Nettoyage des données pour créer un dataframe avec seulement ce qui nous intéresse + matrice de corrélation et autres études statistiques qui en découlent
- ACP.R
- Analyse descriptive.R
- Analyse_des_effets_fixes_des_mois_lum_atm_2023.R
- Chi2_Fisher_Anova.R
- KDE_Map.R
- Évolution annuelle du nombre d'accidents (2006-2023).R
- 
#### Dossier MachineLearning_VisuCarto
- RF_CrossValidation_Bootstrap.R: fichier dans lequel on réalise l'étude de prédiction machine learning avec un algorithme random forest, une validation croisée et un bootstrap
- Reduction_Geographique_Departemental.Rmd: fichier qui permet de réaliser les représentations cartographiques (GWR...)
- Representation_carto.R: Representation interactions spatiales

