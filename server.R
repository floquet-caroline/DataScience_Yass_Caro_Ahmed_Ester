# server.R
server <- function(input, output) {
  
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
  
}