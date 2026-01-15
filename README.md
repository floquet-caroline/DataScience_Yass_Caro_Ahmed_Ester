# Projet DataScience 
## Description
Ce projet a été réalisé dans le cadre d'une UE de DataScience en DaMS4, Polytech Montpellier.

## Méthodes utilisées
Le projet se présente sous forme d'une application RShiny à plusieurs onglets. 
Il se base sur un jeu de données d'actifs bancaires entre 2005 et 2015, étendu ensuite jusque 2025.

#### Objectif
Notre objectif était d'étudier le comportement et le business model des banques européennes vis-à-vis de la crise financière de 2008.

### Clustering
La première partie consiste en une analyse du comportement des banques via du clustering k-means.

### ACP
La deuxième partie consiste en une Analyse en Composantes Principales dans le but the comprendre la structuration de la variance des données.

### Arbres de décision
La troisième partie utilise des arbres de décisions pour identifier des facteurs de détection de crise financière à partir des ratios financiers.

### Analyse combinée des résultats
On a ensuite analysé et combiné les résultats des trois parties pour obtenir une étude plus appronfondie

## Lancement de l'application
Pour lancer l'application, il suffit de la télécharger et de la démarrer depuis le fichier global.R

## Dépendances
En plus de shiny, l'application utilise des librairies de tidyverse telles que tidyr ou dplyr. D'autres packages spécifiques comme DT ou rpart sont utilisés pour des calculs spécifiques.

### Liste des librairies
Shiny
Tidyverse (tidyr, dplyr, ggplot2)
Plotly
Cluster
Bslib
Rsample
Rpart (rpart, rpart.plot)

