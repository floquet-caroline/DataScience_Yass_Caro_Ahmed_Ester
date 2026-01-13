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
             h3("Decision Trees Analysis"),
             br(),
             h4("Data Summary:"),
             tableOutput("decision_tree_table"),
             br(),
             h4("Year Categories:"),
             verbatimTextOutput("year_category_summary")
    )
    
  )
)