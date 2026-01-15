# server.R
server <- function(input, output, session) {
  
  # Appel de ta logique clustering
  server_yassine(input, output, session)
  server_ahmed(input, output, session)
  server_caroline(input, output, session)
  
  # Ici viendront les appels pour les autres étudiants
}