library(MASS)
library(dplyr)

"Formatage des données
agrégation des années en “Avant crise” et “Après crise” (Avant crise : 2005-2010 ; Après crise : 2011-2015 car ce sont des banques européennes qui ont donc subi le krach boursier de 2008 avec un peu de retard + des autres trucs)
création d’un nouveau dataframe avec seulement les variables intéressantes (aka retirer les colonnes vides et le nom de la banque qui n’est pas pertinent)
définition des sets de training, validation et test"
  
df_formatted <- donnees |>
  dplyr::mutate(year_category = case_when(
    year < 2011 ~ "before",
    year >= 2011 ~ "after"
  )) |>
    dplyr::select(-c(X, X.1, X.2, X.3)) |>
      dplyr::select(-c(year,country_code,institution_name))
    
#(finish formatting under df_formatted)
