# global.R
library(shiny)
#library(readxl)

# Charger données
donnees <- read.csv("data/TableauDonnees.csv",sep=";")
cat("Prêt à afficher Hello World !\n")