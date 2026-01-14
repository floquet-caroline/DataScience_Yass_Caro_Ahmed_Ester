# ui.R
ui <- fluidPage(
  
  # Add a tab structure that wraps everything
  tabsetPanel(
    
    # Tab 1: Keep the existing Hello World content
    tabPanel("Hello World",
             h1("Hello World !", 
                style = "color: red; font-size: 40px; text-align: center;"),
             br(),
             br(),
             h4("Première colonne du tableau :"),
             tableOutput("ma_colonne")
    ),
    
    # Decision Trees analysis
    tabPanel("Decision Trees",
             h2("Decision Trees Analysis"),
             br(),
             h4("Data Summary"),
             tableOutput("decision_tree_table"),
             br(),
             h4("Formatted data"),
             tableOutput("formatted_data"),
             br(),
             h3("Decision tree training"),
             plotOutput("tree_training", height = "800px"),
             plotOutput("importance_plot"),
             h3("Predicting (test dataset)"),
             verbatimTextOutput("test_predictions"),
             h4("Confusion matrix"),
             verbatimTextOutput("confusion"),
             h4("Accuracy"),
             textOutput("tree_accuracy")
    )
    
  )
)