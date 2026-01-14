library(MASS)
library(dplyr)
library(rsample)

"Formatage des données
agrégation des années en “Avant crise” et “Après crise” (Avant crise : 2005-2010 ; Après crise : 2011-2015 car ce sont des banques européennes qui ont donc subi le krach boursier de 2008 avec un peu de retard + des autres trucs)
création d’un nouveau dataframe avec seulement les variables intéressantes (aka retirer les colonnes vides et le nom de la banque qui n’est pas pertinent)
définition des sets de training, validation et test"

# -- Data cleaning/formatting --
df_formatted <- donnees |>
  dplyr::mutate(year_category = case_when(
    year < 2011 ~ "before",
    year >= 2011 ~ "after"
  )) |>
    dplyr::select(-c(X, X.1, X.2, X.3)) |>
      dplyr::select(-c(year,country_code,institution_name))

# -- Split data into training and test datasets --
#seed for reproducibility
set.seed(123)

#split object (80% training)
data_split <- initial_split(df_formatted, prop = 0.80)

#extract train and test datasets
train_data <- training(data_split)
test_data  <- testing(data_split)

#get the X and Y matrixes in each
x_train <- train_data |>
  dplyr::select(-year_category)

y_train <- train_data$year_category

x_test <- test_data |>
  dplyr::select(-year_category)

y_test <- test_data$year_category


