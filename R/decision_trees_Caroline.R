library(MASS)
library(dplyr)

# Use path relative to project root (where global.R is)
data_banks <- read.csv("data/TableauDonnees.csv",sep=";")
data_banks

"Formatage des données
agrégation des années en “Avant crise” et “Après crise” (Avant crise : 2005-2010 ; Après crise : 2011-2015 car ce sont des banques européennes qui ont donc subi le krach boursier de 2008 avec un peu de retard + des autres trucs)
création d’un nouveau dataframe avec seulement les variables intéressantes (aka retirer les colonnes vides et le nom de la banque qui n’est pas pertinent)
définition des sets de training, validation et test"

df1 <- data_banks |>
  dplyr::mutate(year_category = case_when(
    year < 2011 ~ "before",
    year >= 2011 ~ "after"
  ))

df1

df2 <- df1 |>
  select(-c(X, X.1, X.2,X.3))
df2

#(finish formatting under df_formatted)
