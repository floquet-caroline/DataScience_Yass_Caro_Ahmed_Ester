# --- PARTIE YASSINE : CLUSTERING K-MEANS 
library(shiny)
library(tidyverse)
library(plotly)
library(cluster)

# ==============================================================================
# 1. PRÉPARATION DES DONNÉES
# ==============================================================================

features <- c('ass_total', 'ass_trade', 'rt_rwa', 'in_roe')

if(!exists("donnees")) {
  if(file.exists("data/TableauDonnees.csv")) {
    donnees <- read.csv("data/TableauDonnees.csv", sep=";", dec=",", stringsAsFactors = FALSE)
  }
}

# Nettoyage
df_yassine <- donnees %>%
  drop_na(all_of(features)) %>%
  filter(year >= 2005, year <= 2015) %>%
  filter(rt_rwa < 3)

# Standardisation
data_scaled <- df_yassine %>%
  select(all_of(features)) %>%
  scale()

# ==============================================================================
# 2. CLUSTERING
# ==============================================================================
set.seed(42)
kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 25)
df_yassine$Cluster <- as.factor(kmeans_result$cluster)

# ==============================================================================
# 3. NOMMAGE DES CLUSTERS
# ==============================================================================
means <- df_yassine %>%
  group_by(Cluster) %>%
  summarise(
    avg_assets = mean(ass_total),
    avg_risk = mean(rt_rwa)
  )

cluster_names <- means %>%
  mutate(Name = case_when(
    avg_assets == max(avg_assets) ~ "Géants Systémiques",
    avg_risk == max(avg_risk) ~ "Banques Risquées",
    TRUE ~ "Banques Prudentes"
  ))

df_final <- df_yassine %>%
  left_join(cluster_names %>% select(Cluster, Name), by = "Cluster") %>%
  rename(Business_Model = Name)

# ==============================================================================
# 4. INTERFACE UI
# ==============================================================================
ui_yassine <- tabPanel(
  "Clustering (Yassine)",
  sidebarLayout(
    sidebarPanel(
      h4("Paramètres"),
      sliderInput("yass_year", "Année", min = 2005, max = 2015, value = 2008, sep = ""),
      selectInput("yass_country", "Pays", choices = c("Tous", sort(unique(df_final$country_code)))),
      
      hr(),
      
      # --- AJOUT : TABLEAU DES COMPTAGES ---
      h4("Effectifs (Nombre de banques)"),
      tableOutput("yass_counts"), 
      
      hr(),
      
      # --- TABLEAU DES FLOPS ---
      h4("Zoom : Performance"),
      p("Risque d'être dans les 'flops' (20% moins rentables) :"),
      tableOutput("yass_bad_perf_stats"),
      
      p("Lecture : Si le taux est de 30%, cela signifie que 30% des banques de ce groupe ont une rentabilité très faible.", 
        style = "font-size: 0.85em; color: #666; font-style: italic; margin-top: 10px;")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphique (Risque/Rentabilité)", plotlyOutput("yass_plot")),
        tabPanel("Évolution Temporelle", plotlyOutput("yass_evo")),
        tabPanel("Méthode du Coude", plotOutput("yass_elbow"))
      )
    )
  )
)

# ==============================================================================
# 5. LOGIQUE SERVEUR
# ==============================================================================
server_yassine <- function(input, output, session) {
  
  # --- Graphique 1 ---
  output$yass_plot <- renderPlotly({
    data_filt <- df_final %>% filter(year == input$yass_year)
    if(input$yass_country != "Tous") data_filt <- data_filt %>% filter(country_code == input$yass_country)
    
    p <- ggplot(data_filt, aes(x = rt_rwa, y = in_roe, color = Business_Model, size = ass_total, text = institution_name)) +
      geom_point(alpha=0.6) + theme_minimal() + labs(title="Risque vs Rentabilité", x="Risque (RWA)", y="Rentabilité (ROE)")
    ggplotly(p)
  })
  
  # --- Graphique 2 ---
  output$yass_evo <- renderPlotly({
    data_evo <- df_final
    if(input$yass_country != "Tous") data_evo <- data_evo %>% filter(country_code == input$yass_country)
    
    p <- ggplot(data_evo, aes(x = year, fill = Business_Model)) +
      geom_bar(position="fill") + theme_minimal() + labs(y="Proportion", x="Année") +
      scale_y_continuous(labels = function(x) paste0(x*100, "%"))
    ggplotly(p)
  })
  
  # --- Graphique 3 ---
  output$yass_elbow <- renderPlot({
    wss <- sapply(1:8, function(k) kmeans(data_scaled, centers = k, nstart=25)$tot.withinss)
    plot(1:8, wss, type="b", pch=19, main="Méthode du Coude", xlab="K", ylab="Inertie")
  })
  
  # --- AJOUT : LOGIQUE POUR LES COMPTAGES ---
  output$yass_counts <- renderTable({
    d_yr <- df_final %>% filter(year == input$yass_year)
    if(input$yass_country != "Tous") d_yr <- d_yr %>% filter(country_code == input$yass_country)
    
    if(nrow(d_yr) == 0) return(NULL)
    
    # On compte simplement combien il y a de banques par groupe
    d_yr %>%
      group_by(Business_Model) %>%
      count() %>%
      rename("Modèle" = Business_Model, "Nombre" = n) %>%
      arrange(desc(Nombre)) # On trie du plus grand au plus petit
  })
  
  # --- Tableau Flops ---
  output$yass_bad_perf_stats <- renderTable({
    d_yr <- df_final %>% filter(year == input$yass_year)
    if(input$yass_country != "Tous") d_yr <- d_yr %>% filter(country_code == input$yass_country)
    
    if(nrow(d_yr) < 5) return(NULL)
    
    seuil_flop <- quantile(d_yr$in_roe, 0.20, na.rm = TRUE)
    
    d_yr %>%
      group_by(Business_Model) %>%
      summarise(
        Total = n(),
        Nb_Mauvaises = sum(in_roe <= seuil_flop),
        .groups = 'drop'
      ) %>%
      mutate(Taux_Echec = Nb_Mauvaises / Total * 100) %>%
      arrange(desc(Taux_Echec)) %>%
      mutate(Affichage = paste0(round(Taux_Echec, 1), " %")) %>%
      select("Business Model" = Business_Model, "Taux d'échec interne" = Affichage)
  })
}