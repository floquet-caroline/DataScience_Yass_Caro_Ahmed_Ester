# --- PARTIE CAROLINE : ARBRES DE DÉCISION (Fichier Unique) ---
library(shiny)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(DT)
library(rsample) # Nécessaire pour le découpage train/test

# ==============================================================================
# 1. PRÉPARATION ET CALCULS (Exécutés au chargement)
# ==============================================================================

# Sécurité : Chargement si lancé hors global.R
if(!exists("donnees")) {
  if(file.exists("data/TableauDonnees.csv")) {
    donnees <- read.csv("data/TableauDonnees.csv", sep=";", dec=",", stringsAsFactors = FALSE)
  }
}

# --- A. Nettoyage des données (Code de Caroline) ---
# On crée un objet spécifique 'caro_df' pour ne pas toucher aux données des autres
caro_df <- donnees %>%
  mutate(year_category = case_when(
    year < 2011 ~ "before",
    year >= 2011 ~ "after"
  )) %>%
  # On retire les colonnes inutiles pour la prédiction
  select(-c(year, country_code, institution_name)) %>%
  # On nettoie les colonnes numériques (virgule -> point)
  mutate(across(-year_category, ~as.numeric(gsub(",", ".", .)))) %>%
  drop_na() %>%
  filter(rt_rwa < 3) # Filtre des outliers (comme Yassine)

# --- B. Création des jeux d'entraînement et de test ---
set.seed(123)
# Split 80% Training / 20% Test
caro_split <- initial_split(caro_df, prop = 0.80, strata = year_category)
caro_train <- training(caro_split)
caro_test  <- testing(caro_split)

# --- C. ARBRE 1 : Standard ---
caro_tree1 <- rpart(year_category ~ ., data = caro_train, method = "class", 
                    control = rpart.control(cp = 0.005))

# Prédictions Arbre 1
caro_pred1 <- predict(caro_tree1, caro_test, type = "class")
caro_conf_mat1 <- table(Predicted = caro_pred1, Actual = caro_test$year_category)
caro_acc1 <- sum(diag(caro_conf_mat1)) / sum(caro_conf_mat1)

# --- D. ARBRE 2 : Équilibré (Balanced) ---
# On règle le problème de déséquilibre des classes
min_size <- min(table(caro_train$year_category))

caro_train_balanced <- caro_train %>%
  group_by(year_category) %>%
  sample_n(min_size) %>%
  ungroup()

caro_tree2 <- rpart(year_category ~ ., data = caro_train_balanced, method = "class", 
                    control = rpart.control(cp = 0.005))

# Prédictions Arbre 2
caro_pred2 <- predict(caro_tree2, caro_test, type = "class")
caro_conf_mat2 <- table(Predicted = caro_pred2, Actual = caro_test$year_category)
caro_acc2 <- sum(diag(caro_conf_mat2)) / sum(caro_conf_mat2)


# ==============================================================================
# 2. INTERFACE UI (ui_caroline)
# ==============================================================================
ui_caroline <- tabPanel(
  "Arbres de Décision (Caroline)",
  
  tabsetPanel(
    
    # --- Onglet 1 : Formatage ---
    tabPanel("Formatage des données",
             br(),
             h3("Préparation du Dataset"),
             p("L'objectif est de prédire si une banque se situe 'Avant' (2005-2010) ou 'Après' (2011-2015) la régulation, en fonction de ses ratios financiers."),
             p("Transformation de la variable 'year' en catégorie 'before/after' et nettoyage."),
             hr(),
             h4("Aperçu des données formatées (Training set)"),
             div(style = "overflow-x: scroll;", tableOutput("caro_table_data"))
    ),
    
    # --- Onglet 2 : Entraînement ---
    tabPanel("Entraînement des Modèles",
             br(),
             fluidRow(
               column(6, 
                      h3("Modèle 1 : Arbre Standard"),
                      p("Arbre entraîné sur les données brutes."),
                      plotOutput("caro_plot_tree1", height = "500px"),
                      h4("Importance des variables (Arbre 1)"),
                      plotOutput("caro_plot_imp1", height = "300px")
               ),
               column(6, 
                      h3("Modèle 2 : Arbre Équilibré"),
                      p("Arbre entraîné avec autant de 'before' que de 'after' pour éviter le biais."),
                      plotOutput("caro_plot_tree2", height = "500px"),
                      h4("Importance des variables (Arbre 2)"),
                      plotOutput("caro_plot_imp2", height = "300px")
               )
             )
    ),
    
    # --- Onglet 3 : Performance ---
    tabPanel("Prédiction & Performance",
             br(),
             h3("Comparaison des résultats sur le jeu de Test"),
             
             fluidRow(
               column(6,
                      div(class = "well",
                          h4("Performance Arbre 1 (Standard)"),
                          plotOutput("caro_heatmap1", height = "300px"),
                          h3(textOutput("caro_text_acc1"), style="color:blue; text-align:center;")
                      ),
                      p("Le modèle standard a un très bon score global mais peine à détecter la classe minoritaire (Before) car il est biaisé par l'abondance de données 'After'.")
               ),
               column(6,
                      div(class = "well",
                          h4("Performance Arbre 2 (Équilibré)"),
                          plotOutput("caro_heatmap2", height = "300px"),
                          h3(textOutput("caro_text_acc2"), style="color:green; text-align:center;")
                      ),
                      p("Le modèle équilibré a une précision globale légèrement inférieure, mais il est beaucoup plus juste : il détecte mieux les périodes 'Before'.")
               )
             ),
             
             hr(),
             h4("Détail des prédictions (Test Set)"),
             DTOutput("caro_table_pred")
    )
  )
)

# ==============================================================================
# 3. LOGIQUE SERVEUR (server_caroline)
# ==============================================================================
server_caroline <- function(input, output, session) {
  
  # --- Onglet 1 : Données ---
  output$caro_table_data <- renderTable({
    head(caro_train, 15)
  })
  
  # --- Onglet 2 : Arbres & Importance ---
  
  # Arbre 1
  output$caro_plot_tree1 <- renderPlot({
    rpart.plot(caro_tree1, type = 4, extra = 104, fallen.leaves = TRUE, 
               main = "Arbre 1 (Standard)", box.palette = "RdYlGn", tweak = 1.1)
  })
  
  output$caro_plot_imp1 <- renderPlot({
    imp <- caro_tree1$variable.importance
    barplot(imp, main = "Importance des variables (Modèle 1)", 
            col = "steelblue", las = 2, cex.names = 0.8)
  })
  
  # Arbre 2 (CORRECTION COULEUR ICI)
  output$caro_plot_tree2 <- renderPlot({
    # J'ai remplacé "BlGnM" par "GnBu" (Vert vers Bleu) qui est valide et joli
    rpart.plot(caro_tree2, type = 4, extra = 104, fallen.leaves = TRUE, 
               main = "Arbre 2 (Équilibré)", box.palette = "GnBu", tweak = 1.1)
  })
  
  output$caro_plot_imp2 <- renderPlot({
    imp <- caro_tree2$variable.importance
    barplot(imp, main = "Importance des variables (Modèle 2)", 
            col = "darkgreen", las = 2, cex.names = 0.8)
  })
  
  # --- Onglet 3 : Matrices de Confusion ---
  
  # Heatmap 1
  output$caro_heatmap1 <- renderPlot({
    df_conf <- as.data.frame(caro_conf_mat1)
    ggplot(df_conf, aes(x = Actual, y = Predicted, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white", size = 8) +
      scale_fill_gradient(low = "#fee0d2", high = "#de2d26") +
      theme_minimal() + labs(x="Réalité", y="Prédiction")
  })
  
  output$caro_text_acc1 <- renderText({
    paste0("Précision (Accuracy) : ", round(caro_acc1 * 100, 2), " %")
  })
  
  # Heatmap 2
  output$caro_heatmap2 <- renderPlot({
    df_conf <- as.data.frame(caro_conf_mat2)
    ggplot(df_conf, aes(x = Actual, y = Predicted, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white", size = 8) +
      scale_fill_gradient(low = "#e5f5e0", high = "#31a354") + 
      theme_minimal() + labs(x="Réalité", y="Prédiction")
  })
  
  output$caro_text_acc2 <- renderText({
    paste0("Précision (Accuracy) : ", round(caro_acc2 * 100, 2), " %")
  })
  
  # Table des prédictions
  output$caro_table_pred <- renderDT({
    res <- data.frame(
      Réalité = caro_test$year_category,
      Pred_Arbre1 = caro_pred1,
      Pred_Arbre2 = caro_pred2
    )
    datatable(res, options = list(pageLength = 10, scrollX = TRUE))
  })
}