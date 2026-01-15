# global.R
library(shiny)
library(tidyverse)
library(plotly)
library(cluster)
library(bslib)

# Charger les données avec le bon séparateur décimal
# dec="," est crucial ici !
donnees <- read.csv("data/TableauDonnees.csv", sep=";", dec=",", stringsAsFactors = FALSE)

# Petit nettoyage de sécurité (supprime les colonnes vides inutiles si le csv est sale)
donnees <- donnees %>% select(
  institution_name, year, country_code, 
  ass_total, ass_trade, rt_rwa, in_roe,   # Les 4 de Yassine
  in_roa, inc_trade, in_trade             # Les 3 d'Ahmed (qui manquaient !)
)

# Import de ta partie
source("R/partieyassine.R")
source("R/partie_acp_ahmed.R")