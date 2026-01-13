# server.R
server <- function(input, output, session) {
  
  # Existing output for Hello World tab
  output$ma_colonne <- renderTable({
    
    # Nom de la première colonne
    nom_colonne <- names(donnees)
    
    # 5 premières valeurs
    resultat <- data.frame(
      Colonne = head(donnees)
    )
    
    # Renommer la colonne
    names(resultat) <- nom_colonne
    
    return(resultat)
  })
  
  # Outputs for Decision Trees tab
  source("R/decision_trees_Caroline.R", local = TRUE)
  
  output$decision_tree_table <- renderTable({
    head(df1, 10)
  })
  
  output$year_category_summary <- renderPrint({
    table(df1$year_category)
  })
  
  output$formatted_data <- renderTable({
    head(df_formatted,10)
  })
  
}