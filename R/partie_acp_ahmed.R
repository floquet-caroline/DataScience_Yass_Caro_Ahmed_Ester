# --- PARTIE AHMED : ACP & TRAJECTOIRES (Version Finale : Style Original + 7 Variables) ---
library(shiny)
library(tidyverse)
library(plotly)

# ==============================================================================
# 1. PRÉPARATION DES DONNÉES
# ==============================================================================

# Liste complète des 7 variables (C'est bon, on a tout !)
colonnes_acp <- c("ass_total", "ass_trade", "inc_trade", "in_roa", "rt_rwa", "in_roe", "in_trade")

# Sécurité : Chargement si lancé hors global.R
if(!exists("donnees")) {
  if(file.exists("data/TableauDonnees.csv")) {
    donnees <- read.csv("data/TableauDonnees.csv", sep=";", dec=",", stringsAsFactors = FALSE)
  }
}

# ==============================================================================
# 2. INTERFACE UI (ui_ahmed)
# ==============================================================================
ui_ahmed <- tabPanel(
  "ACP (Ahmed)",
  sidebarLayout(
    sidebarPanel(
      h4("Paramètres"),
      
      p("L'ACP sera réalisée sur les 7 variables financières du jeu de données.", 
        style = "font-size: 0.9em; color: #555;"),
      
      br(),
      
      actionButton("lancer_acp_ahmed", "Lancer l'analyse ACP", 
                   class = "btn-primary",
                   style = "width: 100%; font-size: 14px; padding: 10px;"),
      
      hr(),
      
      h4("Info"),
      p("Comparaison des profils bancaires (taille, risque, rentabilité) et de leur évolution.", 
        style = "font-size: 0.85em; color: #666; font-style: italic;")
    ),
    mainPanel(
      tabsetPanel(
        
        tabPanel("Cercle des corrélations", 
                 br(),
                 h4("Interprétation des axes"),
                 plotOutput("ahmed_cercle_correlations", height = "500px"),
                 br(),
                 verbatimTextOutput("ahmed_interpretation_axes")
        ),
        
        tabPanel("Trajectoires 2005-2015", 
                 br(),
                 h4("Évolution temporelle globale"),
                 plotOutput("ahmed_trajectoires_globales", height = "500px"),
                 br(),
                 p("Cette trajectoire montre le déplacement du point moyen des banques année par année.",
                   style = "font-size: 0.9em; color: #555;")
        ),
        
        tabPanel("Comparaison FR vs DE", 
                 br(),
                 h4("Les banques allemandes vs françaises"),
                 plotOutput("ahmed_comparaison_pays", height = "500px"),
                 br(),
                 # J'ai remis verbatimTextOutput pour avoir le texte détaillé style terminal
                 verbatimTextOutput("ahmed_analyse_pays")
        ),
        
        tabPanel("Qualité de l'ACP", 
                 br(),
                 h4("Variance expliquée par les composantes"),
                 plotOutput("ahmed_variance_expliquee", height = "400px"),
                 br(),
                 h5("Détail des Composantes Principales (PC)"),
                 tableOutput("ahmed_variance_table")
        )
      )
    )
  )
)

# ==============================================================================
# 3. LOGIQUE SERVEUR (server_ahmed)
# ==============================================================================
server_ahmed <- function(input, output, session) {
  
  # --- CALCUL DE L'ACP ---
  resultat_acp <- eventReactive(input$lancer_acp_ahmed, {
    req(donnees)
    
    # 1. Vérification des colonnes
    colonnes_presentes <- colonnes_acp[colonnes_acp %in% names(donnees)]
    
    if(length(colonnes_presentes) < 2) {
      stop("Erreur: Pas assez de colonnes financières trouvées pour faire une ACP !")
    }
    
    # 2. Sélection et Nettoyage
    donnees_num <- donnees[, colonnes_presentes, drop = FALSE]
    donnees_num <- na.omit(donnees_num)
    donnees_num <- donnees_num[!apply(donnees_num, 1, function(x) any(is.infinite(x))), ]
    
    if(nrow(donnees_num) == 0) {
      stop("Erreur: aucune donnée valide après nettoyage!")
    }
    
    # 3. Calcul ACP
    donnees_std <- scale(donnees_num)
    acp <- prcomp(donnees_std, center = FALSE, scale. = FALSE) 
    scores <- predict(acp)
    
    lignes_valides <- as.numeric(rownames(donnees_num))
    donnees_complete <- donnees[lignes_valides, ]
    
    list(
      acp = acp,
      scores = scores,
      donnees_complete = donnees_complete
    )
  })
  
  # --- 1. CERCLE DES CORRÉLATIONS ---
  output$ahmed_cercle_correlations <- renderPlot({
    req(resultat_acp())
    res <- resultat_acp()
    acp <- res$acp
    
    # Style original
    loadings <- acp$rotation[, 1:2]
    var_exp_1 <- round(summary(acp)$importance[2, 1] * 100, 1)
    var_exp_2 <- round(summary(acp)$importance[2, 2] * 100, 1)
    
    plot(NULL, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), asp = 1,
         xlab = paste0("Axe 1 (", var_exp_1, "%)"), # Titre Axe remis comme avant
         ylab = paste0("Axe 2 (", var_exp_2, "%)"),
         main = "Cercle des corrélations")
    
    angles <- seq(0, 2*pi, length.out = 100)
    lines(cos(angles), sin(angles), col = "grey")
    abline(h = 0, v = 0, lty = 2, col = "grey")
    
    arrows(0, 0, loadings[, 1], loadings[, 2], col = "blue", lwd = 2, length = 0.1)
    text(loadings[, 1]*1.15, loadings[, 2]*1.15, rownames(loadings), col = "darkblue", font=2, cex = 0.85)
  })
  
  # --- 2. INTERPRÉTATION TEXTE ---
  output$ahmed_interpretation_axes <- renderText({
    req(resultat_acp())
    res <- resultat_acp()
    loadings <- res$acp$rotation[, 1:2]
    
    # Variables clés (Top 2 ou 3)
    ax1_top <- names(sort(abs(loadings[, 1]), decreasing = TRUE)[1:3])
    ax2_top <- names(sort(abs(loadings[, 2]), decreasing = TRUE)[1:3])
    
    var_exp_1 <- round(summary(res$acp)$importance[2, 1] * 100, 1)
    var_exp_2 <- round(summary(res$acp)$importance[2, 2] * 100, 1)
    
    # Mise en forme exacte comme l'ancien fichier
    paste0(
      "AXE 1 (", var_exp_1, "% variance) :\n",
      "Principales variables : ", paste(ax1_top, collapse=", "), "\n\n",
      
      "AXE 2 (", var_exp_2, "% variance) :\n",
      "Principales variables : ", paste(ax2_top, collapse=", "), "\n"
    )
  })
  
  # --- 3. TRAJECTOIRES ---
  output$ahmed_trajectoires_globales <- renderPlot({
    req(resultat_acp())
    res <- resultat_acp()
    
    df_plot <- data.frame(year = res$donnees_complete$year, PC1 = res$scores[, 1], PC2 = res$scores[, 2])
    centroides <- aggregate(cbind(PC1, PC2) ~ year, df_plot, mean)
    centroides <- centroides[order(centroides$year), ]
    
    var_exp_1 <- round(summary(res$acp)$importance[2, 1] * 100, 1)
    var_exp_2 <- round(summary(res$acp)$importance[2, 2] * 100, 1)
    
    # Style graphique original (Rouge, ligne épaisse)
    plot(centroides$PC1, centroides$PC2, type = "b", pch = 19, col = "red", lwd = 2, cex = 2,
         main = "Trajectoire moyenne (2005-2015)", 
         xlab = paste0("Axe 1 (", var_exp_1, "%)"), 
         ylab = paste0("Axe 2 (", var_exp_2, "%)"))
    
    text(centroides$PC1, centroides$PC2, centroides$year, pos = 3, cex=0.8)
    grid()
  })
  
  # --- 4. COMPARAISON PAYS (Avec les couleurs exactes) ---
  output$ahmed_comparaison_pays <- renderPlot({
    req(resultat_acp())
    res <- resultat_acp()
    
    df_plot <- data.frame(country = res$donnees_complete$country_code, PC1 = res$scores[, 1], PC2 = res$scores[, 2])
    df_fr <- df_plot[df_plot$country == "FR", ]
    df_de <- df_plot[df_plot$country == "DE", ]
    
    var_exp_1 <- round(summary(res$acp)$importance[2, 1] * 100, 1)
    var_exp_2 <- round(summary(res$acp)$importance[2, 2] * 100, 1)
    
    plot(df_plot$PC1, df_plot$PC2, type="n", main="France vs Allemagne", 
         xlab = paste0("Axe 1 (", var_exp_1, "%)"), 
         ylab = paste0("Axe 2 (", var_exp_2, "%)"))
    
    abline(h=0, v=0, col="grey", lty=2)
    
    # J'utilise la transparence (alpha) pour que ce soit lisible avec beaucoup de points, 
    # mais en gardant Bleu et Rouge comme demandé.
    points(df_fr$PC1, df_fr$PC2, col=rgb(0,0,1,0.4), pch=19, cex=1) # Bleu FR
    points(df_de$PC1, df_de$PC2, col=rgb(1,0,0,0.4), pch=19, cex=1) # Rouge DE
    
    legend("topright", legend=c("France", "Allemagne"), col=c("blue", "red"), pch=19)
  })
  
  # --- 5. TEXTE PAYS (RESTAURÉ : Analyse détaillée des moyennes) ---
  output$ahmed_analyse_pays <- renderText({
    req(resultat_acp())
    res <- resultat_acp()
    
    df_plot <- data.frame(country = res$donnees_complete$country_code, PC1 = res$scores[, 1], PC2 = res$scores[, 2])
    
    fr <- df_plot[df_plot$country == "FR", ]
    de <- df_plot[df_plot$country == "DE", ]
    
    # Voici le format "Terminal" détaillé que tu voulais
    paste0(
      "FRANCE\n",
      "Axe 1 moyen: ", round(mean(fr$PC1), 3), "\n",
      "Axe 2 moyen: ", round(mean(fr$PC2), 3), "\n\n",
      
      "ALLEMAGNE\n",
      "Axe 1 moyen: ", round(mean(de$PC1), 3), "\n",
      "Axe 2 moyen: ", round(mean(de$PC2), 3), "\n\n",
      
      "--> Les profils moyens sont distincts."
    )
  })
  
  # --- 6. GRAPHIQUE VARIANCE (Style original avec seuil 70%) ---
  output$ahmed_variance_expliquee <- renderPlot({
    req(resultat_acp())
    res <- resultat_acp()
    eig <- summary(res$acp)$importance
    
    # On affiche tout
    variance_pct <- eig[2, ] * 100
    
    barplot(variance_pct, names.arg = names(variance_pct), col = "steelblue", 
            main = "Variance expliquée", ylab = "% de variance",
            ylim = c(0, max(variance_pct) + 10))
    
    # Ajout de la ligne rouge à 70% si tu l'avais avant, ou courbe cumulée
    lines(x = 1:length(variance_pct), y = cumsum(variance_pct), type = "b", col = "red", lwd=2)
    abline(h=70, col="darkred", lty=2) # Ligne de seuil visuel
    text(1, 72, "Seuil 70%", col="darkred", pos=4, cex=0.8)
  })
  
  # --- 7. TABLEAU VARIANCE (7 Composantes) ---
  output$ahmed_variance_table <- renderTable({
    req(resultat_acp())
    res <- resultat_acp()
    eig <- summary(res$acp)$importance
    
    # On affiche toutes les PC trouvées (donc 7)
    n_comp <- ncol(eig) 
    data.frame(Composante = colnames(eig)[1:n_comp], 
               "Variance (%)" = round(eig[2, 1:n_comp] * 100, 2), 
               "Cumulée (%)" = round(eig[3, 1:n_comp] * 100, 2), check.names = FALSE)
  })
}
