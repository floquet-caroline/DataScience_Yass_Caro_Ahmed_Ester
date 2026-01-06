# global.R
library(shiny)
library(readxl)

# Charger données
donnees <- read_excel("data/Theme4_coop_zoom_data.xlsx")

cat("Prêt à afficher Hello World !\n")