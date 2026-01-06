# ui.R
ui <- fluidPage(
  
  # Hello World
  h1("Hello World !", 
     style = "color: red; font-size: 40px; text-align: center;"),
  
  # Petit espace
  br(),
  br(),
  
  # Titre du tableau
  h4("Première colonne du tableau :"),
  
  # Tableau
  tableOutput("ma_colonne")
  
)