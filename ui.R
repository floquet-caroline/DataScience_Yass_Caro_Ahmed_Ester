# ui.R
library(DT)
ui <- fluidPage(
  h2("Analyse Arbres de Décision"),
  # Add a tab structure that wraps everything
  tabsetPanel(
    # Decision Trees analysis
    tabPanel("Formatage des données",
             h3("Formatage des données"),
             p("On formate les données en retirant les colonnes non pertinentes pour cette analyse. On transforme aussi l'année en catégories 'before' (jusqu'à 2010) et 'after' (à partir de 2011)"),
             h4("Données formatées"),
             tableOutput("formatted_data")
    ),
    tabPanel("Training",
             h3("Entraînement"),
             p("L'objectif ici est d'entraîner un arbre de décision pour comprendre les facteurs principaux de la crise."),
             plotOutput("tree_training", height = "800px"),
             plotOutput("importance_plot"),
             p("On remarque que les variables les plus importantes pour détecter la crise par arbre de décision sont les capitaux (ass_total et ass_trade). Ils sont suivis de près par le retour sur capital (roa) et le risque (rt_rwa) qui sont encore significatifs (+ de 10%)."),
             h3("Deuxième arbre"),
             p("On entraîne un deuxième arbre avec un dataset contenant une quantité à peu près égale de modalités 'before' et 'after'."),
             plotOutput("tree_tr2",height = "800px"),
             plotOutput("importance_plot2"),
             p("On constate cette fois que les variables les plus importantes dans la décision sont plutôt celles autour des capitaux spéculatifs. Le modèle considère cependant plus de variables avec la même importance.")
    ),
    tabPanel("Prédiction et Performance",
             h3("Prédiction (dataset test)"),
             DT::DTOutput("test_predictions"),
             h4("Matrice de confusion"),
             plotOutput("confusion_heatmap"),
             h4("Accuracy"),
             textOutput("tree_accuracy"),
             p("On constate que l'arbre prédit plutôt bien les banques après crise mais a du mal à déceler les banques avant crise. Ceci est dû à l'abondance des résultats après crise dans le dataset de training."),
             p("On va donc entraîner un deuxième arbre en réduisant les données après crise dans les données d'entraînement."),
             h3("Résultats du deuxième arbre"),
             h4("Matrice de confusion"),
             plotOutput("confusion_heatmap2"),
             h4("Accuracy"),
             textOutput("tree_accuracy2"),
             p("On constate que le deuxième arbre est dit moins performant mais reflète bien mieux les données. Il semble mieux réparti en termes d'erreurs.")
    )
    
  )
)