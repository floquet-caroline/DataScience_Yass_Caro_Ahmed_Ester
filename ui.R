# ui.R
ui_caroline <- fluidPage(
  h2("Analyse Arbres de Décision"),
  # Add a tab structure that wraps everything
  tabsetPanel(
    # Decision Trees analysis
    tabPanel("Formatage des données",
             p("On formate les données en retirant les colonnes non pertinentes pour cette analyse. On transforme aussi l'année en catégories 'before' (jusqu'à 2010) et 'after' (à partir de 2011)"),
             h4("Données formatées"),
             tableOutput("formatted_data")
    ),
    tabPanel("Training",
             p("L'objectif ici est d'entraîner un arbre de décision pour comprendre les facteurs principaux de la crise."),
             h3("Entraînement"),
             plotOutput("tree_training", height = "800px"),
             plotOutput("importance_plot")
             p("On remarque que les variables les plus importantes pour détecter la crise par arbre de décision sont les capitaux (ass_total et ass_trade). Ils sont suivis de près par le retour sur capital (roa) et le risque (rt_rwa) qui sont encore significatifs (+ de 10%).")
    ),
    tabPanel("Prédiction et Performance",
             h3("Prédiction (dataset test)"),
             verbatimTextOutput("test_predictions"),
             h4("Matrice de confusion"),
             verbatimTextOutput("confusion"),
             h4("Accuracy"),
             textOutput("tree_accuracy")
             p("Ces informations nous disent que l'arbre est capable de prédire plutôt bien si une banque se trouve avant ou après crise via ses résultats.")
    )
    
  )
)