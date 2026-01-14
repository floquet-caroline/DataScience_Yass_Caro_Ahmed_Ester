# server.R
library(ggplot2)
source("R/decision_trees_Caroline.R", local = FALSE)
server_caroline <- function(input, output, session) {
  
  # Outputs for Decision Trees tab
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
  
  output$tree_tr2 <- renderPlot({
    rpart.plot(tree_balanced, 
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
  
  output$importance_plot2 <- renderPlot({
    #importance scores extracted from tree
    importances <- tree_balanced$variable.importance
    
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
  
  output$test_predictions <- DT::renderDT({
    #make predictions
    #interactive table
    datatable(comparison_df, 
              filter = 'top', #search boxes
              options = list(
                pageLength = 10,
                autoWidth = TRUE,
                scrollX = TRUE
              ),
              caption = 'Observations VS Prédictions')
  })
  
  output$confusion <- renderPrint({
    #confusion matrix
    conf_matrix
  })
  
  output$confusion_heatmap <- renderPlot({
    # Convert table to dataframe for plotting
    df_conf <- as.data.frame(conf_matrix)
    
    ggplot(df_conf, aes(x = Actual, y = Predicted, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white", size = 8) +
      scale_fill_gradient(low = "#fee0d2", high = "#de2d26") +
      theme_minimal() +
      labs(title = "Confusion Matrix Heatmap", x = "Actual Crisis Period", y = "Predicted Period")
  })
  
  output$tree_accuracy <- renderText({
    #accuracy
    paste0(round_accuracy, "%")
  })
  
  output$confusion2 <- renderPrint({
    #confusion matrix
    conf_matrix_balanced
  })
  
  output$confusion_heatmap2 <- renderPlot({
    # Convert table to dataframe for plotting
    df_conf <- as.data.frame(conf_matrix_balanced)
    
    ggplot(df_conf, aes(x = Actual, y = Predicted, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white", size = 8) +
      scale_fill_gradient(low = "#fee0d2", high = "#de2d26") +
      theme_minimal() +
      labs(title = "Confusion Matrix Heatmap", x = "Actual Crisis Period", y = "Predicted Period")
  })
  
  output$tree_accuracy2 <- renderText({
    #accuracy
    paste0(round_accuracy_balanced, "%")
  })
}