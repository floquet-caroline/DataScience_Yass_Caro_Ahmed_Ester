library(rpart)
library(rpart.plot)
library(tidyr)
library(dplyr)
library(rsample)

# -- Data cleaning/formatting --
df_formatted <- donnees |>
  dplyr::mutate(year_category = case_when(
    year < 2011 ~ "before",
    year >= 2011 ~ "after"
  )) |>
    dplyr::select(-c(X, X.1, X.2, X.3)) |>
      dplyr::select(-c(year,country_code,institution_name)) |>
        #force columns (except year_category) to be numeric
        mutate(across(-year_category, ~as.numeric(gsub(",", ".", .)))) |>
          tidyr::drop_na(everything()) |>
            dplyr::filter(rt_rwa  < 3)

# -- Split data into training and test datasets --
#seed for reproducibility
set.seed(123)

#split object (80% training) with strata
#year categories (before or after 2010-2011) have the same proportion in both datasets
#that ensures better performance and less bias
data_split <- initial_split(df_formatted, prop = 0.80, strata = year_category)

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

#train decision tree
#cp = pruning (élaguage)
tree <- rpart(year_category ~ ., data=train_data, method="class", control = rpart.control(cp = 0.005))

#prédiction
predictions <- predict(tree, test_data, type = "class")
prob_predictions <- predict(tree, test_data, type = "prob")

comparison_df <- data.frame(
  Actual = test_data$year_category,
  Predicted = predictions,
  prob_predictions
)

#confusion matrix and accuracy
conf_matrix <- table(Predicted = predictions, Actual = test_data$year_category)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
round_accuracy <- round(accuracy * 100, 2)

#---- second tree ----
# Identify the size of the smallest group
min_size <- min(table(train_data$year_category))

# Down-sample the training data to have equal counts
train_data_balanced <- train_data |>
  group_by(year_category) |>
  sample_n(min_size) |>
  ungroup()

# -- Train the second (balanced) decision tree --
tree_balanced <- rpart(
  year_category ~ ., 
  data = train_data_balanced, 
  method = "class", 
  control = rpart.control(cp = 0.005)
)

# -- Predictions for the balanced tree --
predictions_balanced <- predict(tree_balanced, test_data, type = "class")

conf_matrix_balanced <- table(
  Predicted = predictions_balanced, 
  Actual = test_data$year_category
)

accuracy_balanced <- sum(diag(conf_matrix_balanced)) / sum(conf_matrix_balanced)
round_accuracy_balanced <- round(accuracy_balanced * 100, 2)
