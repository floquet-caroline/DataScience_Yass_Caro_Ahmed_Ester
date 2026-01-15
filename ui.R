# ui.R
ui <- navbarPage(
  title = "Analyse Bancaire 2005-2015",
  theme = bs_theme(bootswatch = "flatly"),
  
  # Ton onglet (défini dans partieyassine.R)
  ui_yassine,
  
  ui_ahmed,
  
  ui_caroline,
  
  
)