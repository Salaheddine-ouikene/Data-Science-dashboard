library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

if (!require("shiny")) install.packages("shiny")
if (!require("caret")) install.packages("caret")
if (!require("ROSE")) install.packages("ROSE")

library(caret)
library(ROSE)

shinyUI(
  dashboardPage(
    
# ---------------------------------------------------------------------- HEADER -------------------
    dashboardHeader(title = "Projet Prog Web"), 
    
# ---------------------------------------------------------------------- Menu ------------------
    dashboardSidebar(
      sidebarMenu(
        menuItem("Load data", tabName = "load", icon = icon("database")),
        menuItem("Nettoyage des données", tabName = "nettoyage", icon = icon("broom")
                 , startExpanded = FALSE,
                 menuSubItem("Types des variables", tabName = "type"),
                 menuSubItem("Valeurs manquantes", tabName = "manquantes"),
                 menuSubItem("Outliers", tabName = "outliers"),
                 menuSubItem("Normalisation", tabName = "normalisation"),
                 menuSubItem("Dummification", tabName = "dummification"),
                 menuSubItem("Déséquilibre des classes", tabName = "desequilibre")
        ),
        menuItem("Analyse expolaratoire", icon = icon("magnifying-glass"), tabName = "analyse"
                 # , startExpanded = TRUE,
                 # menuSubItem("Exemple", tabName = "exemple_analyse")
        ),
        menuItem("entraînement de modèles", icon = icon("brain"), tabName = "model",
                 menuSubItem("Logistic Regression", tabName = "logistic_regression", icon = icon("chart-line")),
                 menuSubItem("Decision Trees", tabName = "decision_trees", icon = icon("tree")),
                 menuSubItem("Random Forests", tabName = "random_forests", icon = icon("leaf")),
                 menuSubItem("SVM", tabName = "svm", icon = icon("project-diagram"))
        )
      )
    ),
    
# ---------------------------------------------------------------------- Content ------------------
    dashboardBody(
      tabItems(
        
# ---------------------------------------------------------------------- Load data ----------------
        tabItem(tabName = "load",
                  fluidPage(
                    sidebarLayout(
                      sidebarPanel(
                        fileInput("file", "Choisir un fichier csv",
                                  accept = c("text/plain",".csv")
                        ),
                        numericInput("rows_to_show", "Nombre de lignes à afficher", 10, min = 1),
                        checkboxInput("header", "Le fichier CSV, contient-il des en-têtes (Headers)?", TRUE)),
                      mainPanel(
                        DTOutput("table")
                      )
                    )
                  )
                ),
        
        
# ----------------------------------------------------------------------- Type de variable --------
        tabItem(tabName = "type",
                fluidPage(
                  sidebarLayout(
                    sidebarPanel(
                      numericInput("unique_threshold", "Précisez un seuil des valeurs uniques pour les colonnes catégorielles:", 10),
                      actionButton("submit_type", "Envoyer")
                    ),
                    mainPanel(
                      DTOutput("cat_columns_table"),
                      br(),
                      br(),
                      DTOutput("num_columns_table")
                    )
                  )
                )
              ),

# ----------------------------------------------------------------------- Missing values --------
    tabItem(tabName = "manquantes",
            fluidPage(
              
              sidebarLayout(
                sidebarPanel(
                  selectInput("missing_method", "Méthode de traitement des valeurs manquantes:",
                              c("Supprimer les lignes" = "delete",
                                "Remplacer par la Moyenne" = "mean",
                                "Remplacer par la médiane" = "median",
                                "Remplissage avant" = "ffill")),
                  actionButton("submit_missing", "Envoyer")
                ),
                mainPanel(
                  DTOutput("data_table")
                )
              )
            )
    ),


    
# ----------------------------------------------------------------------- Outliers --------
    tabItem(tabName = "outliers",
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  selectInput("numeric_column", "Choisir une colonne", ""),
                  checkboxInput("remove_outliers", "Supprimer les valeurs aberrantes", FALSE),
                  actionButton("submit_outlier", "Envoyer")
                ),
                mainPanel(
                  plotOutput("boxplot_outlier"),
                  br(),
                  br(),
                  DTOutput("table_outlier")
                )
              )
            )
    ),
    

# ----------------------------------------------------------------------- Normalisation --------

    tabItem(tabName = "normalisation",
          fluidPage(
            sidebarLayout(
              sidebarPanel(
                selectInput("numeric_column_normalise", "Choisir une colonne", ""),
                actionButton("normalize_button", "Normaliser"),
                textOutput("status_norm")
              ),
              mainPanel(
                DTOutput("table_normalise")
              )
            )
          )
    ),



# ----------------------------------------------------------------------- Dummification --------

    tabItem(tabName = "dummification",
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  selectInput("column_dummifie", "Choisir une colonne", ""),
                  actionButton("dummifie_button", "Dummifie"),
                  textOutput("status_dummifie")
                ),
                mainPanel(
                  DTOutput("table_dummifie")
                )
              )
            )
    ),



# ----------------------------------------------------------------------- desequilibre --------

# ----------------------------------------------------------------------- desequilibre --------

tabItem(tabName = "desequilibre",
        fluidPage(
          sidebarLayout(
            sidebarPanel(
              selectInput("column_desequilibre", "Choisir la variable cible", ""),
              selectInput("sampling_method", "Choisir la méthode d'échantillonnage:",
                          choices = c("Over-sampling" = "over", 
                                      "Under-sampling" = "under"
                          )),
              actionButton("desequilibre_button", "Envoyer")
            ),
            mainPanel(
              DTOutput("table_desequilibre")
            )
          )
        )
),
    

        
# ----------------------------------------------------------------------- Analyse exploratoire ----
        tabItem(tabName = "exemple_analyse", "analyse"),
        
        
# ----------------------------------------------------------------------- Entrainement ------------
#----------------------------------- LOGISTIC REGRESSION -----------------------------------------        
# Logistic Regression tab
tabItem(tabName = "logistic_regression",
        fluidRow(
          
          column(4,
                 
                 
                 fluidRow
                 (
                   titlePanel("Settings"), 
                   wellPanel(
                     selectInput("logistic_response_var", "Select Response Variable", choices = NULL), # For response variable
                     
                     selectInput("logistic_vars", "Select Predictor Variables", choices = NULL, multiple = TRUE), # For predictor variables
                     sliderInput("threshold", "Threshold for Classification:", 
                                 min = 0, max = 1, value = 0.5, step = 0.001)
                     
                   )
                 ),
                 fluidRow
                 (
                   column(12,
                          titlePanel("Performance metrics"),
                          # Add CSS to increase the size of the table
                          tags$style(HTML("
                                              #logistic_performance_metrics {
                                                width: 80px;   /* Set the width */
                                                height: 60px;  /* Set the height */
                                              }
                                            ")),
                          tableOutput("logistic_performance_metrics")
                   )
                   
                   
                   
                 )
                 
                 
          ),
          column(8,
                 titlePanel("Summary"), 
                 div(
                   verbatimTextOutput(outputId = "summarymodel"),
                   style = "overflow-y: scroll; max-height: 350px;"  # Adjust max-height as needed
                 )
                 # Add any other UI components here
          )
          
          
          
          
        ),
        fluidRow(
          column(6,
                 titlePanel("ROC Curve"),
                 plotOutput("logistic_roc_curve")
                 
          )
          ,
          column(6,
                 titlePanel("Predicted probabilities plot"),
                 plotOutput("predicted_probabilities_plot")
                 
          )
          
        ),
        # Inside the logistic regression tab
        fluidRow(titlePanel("Feature Importance"),
                 column(4,
                        
                        tableOutput("logistic_coef")
                 ),
                 column(8,
                        
                        plotOutput("logistic_coef_plot")
                 )
                 
                 
        )
        
        
        
        
        
),
#--------------------------------- DECISION TREES ---------------------------------------------------------------        
# Ajouter un nouvel onglet pour les arbres de décision
tabItem(tabName = "decision_trees",
        fluidRow(
          column(4,
                 fluidRow(
                   titlePanel("Settings"), 
                   wellPanel(
                     selectInput("tree_response_var", "Sélectionner la variable de réponse", choices = NULL), # Variable de réponse
                     selectInput("tree_vars", "Sélectionner les variables prédictives", choices = NULL, multiple = TRUE) # Variables prédictives
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          titlePanel("Performance metrics"),
                          tableOutput("tree_performance_metrics")
                   )
                 )
          ),
          column(8,
                 titlePanel("Résumé du Modèle"),
                 div(
                   verbatimTextOutput("tree_summary"), # Résumé du modèle
                   style = "overflow-y: scroll; max-height: 400px;"  # Adjust max-height as needed
                 )
                 
          )
        ),
        fluidRow(
          
          column(6,
                 titlePanel("Graphique de l'Arbre de Décision"),
                 plotOutput("tree_plot") # Visualisation de l'arbre
          ),
          column(6,
                 titlePanel("ROC Curve (Decision Tree)"),
                 plotOutput("tree_roc_curve")
          )
        )
        
        
),
#---------------------------------------- RANDOM FOREST ------------------------------------------------        
# Inside the Random Forests tab
tabItem(tabName = "random_forests",
        fluidRow(
          column(4,
                 fluidRow(
                   titlePanel("Settings"),
                   wellPanel(
                     selectInput("rf_response_var", "Select Response Variable", choices = NULL),
                     selectInput("rf_vars", "Select Predictor Variables", choices = NULL, multiple = TRUE),
                     numericInput("rf_ntree", "Number of Trees", value = 500, min = 1)
                     
                   )
                 ),
                 fluidRow(
                   column(12,
                          titlePanel("Performance Metrics"),
                          tableOutput("rf_performance_metrics")
                   )
                 )
                 
          ),
          column(8,
                 titlePanel("Model Summary"),
                 verbatimTextOutput("rf_summary")
          )
        ),
        
        fluidRow(
          
          column(6,
                 titlePanel("Feature Importance"),
                 plotOutput("rf_importance_plot")
          ),
          column(6,
                 titlePanel("ROC Curve"),
                 plotOutput("rf_roc_curve")
          )
        )
        
        
)




      )
    )
  )
)