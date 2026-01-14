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
    head(donnees, 10)
  })
  
  output$formatted_data <- renderTable({
    head(df_formatted,10)
  })
  
  output$tree_training <- renderPlot({
    #tweak stretches the tree and varlen doesn't allow variable names to be shrunk
    rpart.plot(tree, 
               type = 4, 
               extra = 104, 
               fallen.leaves = TRUE, 
               cex = 0.8, 
               tweak = 1.1, 
               varlen = 0, 
               shadow.col = "gray", 
               box.palette = "RdYlGn"
    )
  })
  
  output$importance_plot <- renderPlot({
    #importance scores extracted from tree
    importances <- tree$variable.importance
    
    #plot dataframe
    df_imp <- data.frame(
      Variable = names(importances),
      Importance = as.numeric(importances)
    )
    
    #sorted barplot
    df_imp <- df_imp[order(df_imp$Importance, decreasing = TRUE), ]
    
    barplot(df_imp$Importance, 
            names.arg = df_imp$Variable, 
            las = 2, 
            col = "steelblue", 
            main = "Variable Importance")
  })
  
  output$test_predictions <- renderPrint({
    #make predictions
    predictions
  })
  
  output$confusion <- renderPrint({
    #confusion matrix
    conf_matrix
  })
  
  output$tree_accuracy <- renderText({
    #accuracy
    paste0(round_accuracy, "%")
  })
}