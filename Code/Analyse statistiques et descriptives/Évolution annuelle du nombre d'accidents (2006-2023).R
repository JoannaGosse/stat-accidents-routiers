library(dplyr)
library(ggplot2)
library(readr)

caracteristiques <- list()

# Importation et harmonisation des donn?es pour les ann?es 2006-2023
for (annee in 2006:2023) {
  file_path <- paste0("C:/Users/axeld/OneDrive/Documents/Accident routier/data/", annee, "_caracteristiques.csv")
  
  if (file.exists(file_path)) {
    df <- read_delim(file_path, delim = ";", show_col_types = FALSE)
    # V?rification et ajout de colonnes si manquantes
    if (!"Num_Acc" %in% colnames(df)) {
      df <- df %>% mutate(Num_Acc = NA) 
    }
    if (!"an" %in% colnames(df)) {
      df <- df %>% mutate(an = annee) 
    }
    df <- df %>% select(Num_Acc, an)
    caracteristiques[[as.character(annee)]] <- df
  }
}

# Fusionner toutes les ann?es en une seule table
caracteristiques_all <- bind_rows(caracteristiques)

# Tcheck des donn?es combin?es
print(head(caracteristiques_all))
print(summary(caracteristiques_all))

# Analyse annuelle : Nombre d'accidents par ann?e
accidents_annuels <- caracteristiques_all %>%
  group_by(an) %>%
  summarize(nb_accidents = n(), .groups = "drop")

# Bandes de confiance avec bootstrap (1000 simulations)
set.seed(123)
n_simulations <- 1000

bootstrap <- replicate(n_simulations, {
  sampled <- sample(accidents_annuels$nb_accidents, replace = TRUE)
  mean(sampled)
})

# Calcul des intervalles de confiance
conf_interval <- quantile(bootstrap, probs = c(0.025, 0.975))

# Ajouter des bandes de confiance aux donn?es
accidents_annuels <- accidents_annuels %>%
  mutate(
    ci_lower = conf_interval[1],
    ci_upper = conf_interval[2]
  )

# Graphique avec evolution annuelle et bandes de confiance
ggplot(accidents_annuels, aes(x = an, y = nb_accidents)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "lightblue", alpha = 0.3) +
  labs(
    title = "?volution annuelle du nombre d'accidents (2006-2023)",
    x = "Ann?e",
    y = "Nombre d'accidents"
  ) +
  theme_minimal()


# Visualisation de la distribution des moyennes bootstrap
ggplot(bootstrap_df, aes(x = mean_bootstrap)) +
  geom_histogram(binwidth = 100, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  geom_vline(xintercept = conf_interval, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Distribution des moyennes bootstrap des accidents",
    subtitle = "Lignes rouges : intervalles de confiance à 95%",
    x = "Moyenne bootstrap",
    y = "Fréquence/Densité"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


# Verif pour 2020
mean_2020 <- accidents_annuels %>% filter(an == 2020) %>% pull(nb_accidents)
p_value <- sum(bootstrap >= mean_2020) / n_simulations
cat("Le nb d'accen 2020 est-il significativement eleve ? P-value :", p_value, "\n")