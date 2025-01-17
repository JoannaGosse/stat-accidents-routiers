library(lme4)
library(dplyr)
library(ggplot2)
library(readr)


# Importation des donn?es 2023
caracteristiques2023 <- read_delim("D:/Accidents_routiers/data/2023_caracteristiques.csv", delim = ";")
lieux2023 <- read_delim("D:/Accidents_routiers/data/2023_lieux.csv", delim = ";")

# Fusionner les donn?es n?cessaires
data_2023 <- caracteristiques2023 %>%
  left_join(lieux2023, by = "Num_Acc") %>%
  select(Num_Acc, jour, mois, lum, atm) %>%
  mutate(
    mois = as.factor(mois),        
    jour = as.numeric(jour),      
    atm = as.factor(atm),
    lum = as.factor(lum)       
  )

# Mapper les conditions atmosph?riques
conditions_atm <- c(
  "-1" = "Non renseign?",
  "1" = "Normale",
  "2" = "Pluie l?g?re",
  "3" = "Pluie forte",
  "4" = "Neige - gr?le",
  "5" = "Brouillard - fum?e",
  "6" = "Vent fort - temp?te",
  "7" = "Temps ?blouissant",
  "8" = "Temps couvert",
  "9" = "Autre"
)

# Mapper les conditions de luminosit?
conditions_lum <- c(
  "-1" = "Non renseign?",
  "1" = "Plein jour",
  "2" = "Aube ou cr?puscule",
  "3" = "Nuit avec ?clairage",
  "4" = "Nuit sans ?clairage",
  "5" = "Inconnu"
)

data_2023 <- data_2023 %>%
  mutate(
    atm_label = recode(atm, !!!conditions_atm),
    lum_label = recode(lum, !!!conditions_lum)
  )

# V?rification des donn?es
print(head(data_2023))
print(summary(data_2023))

#Identifier les anomalies dans la variable lum 
lum_counts <- data_2023 %>%
  group_by(lum) %>%
  summarize(count = n())
print(lum_counts)

# Pr?parer les donn?es pour le mod?le 
data_model <- data_2023 %>%
  filter(!is.na(atm), !is.na(lum))  

# Mod?le ? effets fixes (mois, atm, lum) 
modele_fixe <- lmer(
  jour ~ mois + atm + lum + (1 | Num_Acc), 
  data = data_model
)

# R?sum? du mod?le
summary(modele_fixe)


data_model <- data_model %>%
  mutate(predicted_fixed = predict(modele_fixe, newdata = ., re.form = NULL))

# Visualisation des effets fixes 
ggplot(data_model, aes(x = mois, y = predicted_fixed, color = atm_label)) +
  geom_line(size = 1) +
  facet_wrap(~ lum_label, scales = "free") +  
  labs(
    title = "Analyse des effets fixes des mois, des conditions atmosph?riques et de luminosit? sur les accidents routiers en 2023",
    x = "Mois",
    y = "Nombre pr?vu d'accidents",
    color = "Condition atmosph?rique"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Nombre d'accidents par mois, atm et lum 
monthly_atm_lum_summary <- data_model %>%
  group_by(mois, atm_label, lum_label) %>%
  summarize(nb_accidents = n(), .groups = "drop")

ggplot(monthly_atm_lum_summary, aes(x = mois, y = nb_accidents, fill = atm_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ lum_label, scales = "free") + 
  labs(
    title = "Nombre d'accidents par mois en fonction des conditions atmosph?riques et de luminosit? (2023)",
    x = "Mois",
    y = "Nombre d'accidents",
    fill = "Condition atmosph?rique"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- ?tape 8 : Test de robustesse ---
modele_sans_atm_lum <- lmer(
  jour ~ mois + (1 | Num_Acc), 
  data = data_model
)

# Test de robustesse (ANOVA)
anova_test <- anova(modele_fixe, modele_sans_atm_lum)
print(anova_test)

# Interpr?tation des r?sultats
if (!is.na(anova_test[2, "Pr(>Chisq)"]) && anova_test[2, "Pr(>Chisq)"] < 0.05) {
  cat("Les conditions atmosph?riques et de luminosit? ont un effet significatif sur le nombre d'accidents.\n")
} else {
  cat("Les conditions atmosph?riques et de luminosit? n'ont pas d'effet significatif sur le nombre d'accidents.\n")
}
