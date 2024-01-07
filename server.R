library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
if (!require("shiny")) install.packages("shiny")
if (!require("caret")) install.packages("caret")
if (!require("ROSE")) install.packages("ROSE")
library(caret)
library(ROSE)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT) # for data tables
library(caret) # for modeling
library(pROC)  # For ROC curve
library(ROCR)  # For ROC curve
library(rpart) # Pour les arbres de décision
library(plotly)
library(rpart.plot)
library(randomForest)
library(reshape2)

shinyServer(
  function(input, output, session) {
    
# ------------------------------------------------------------------ load data partie ---------------------

    data <- reactive({
      inFile <- input$file
      if (is.null(inFile)) return(NULL)
      read.csv(input$file$datapath, header = input$header)
      
    })
    
    values <- reactiveValues(df_data = NULL,df_numeric_col = NULL, df_categor_col = NULL)
    
    observeEvent(input$file, {
      
      values$df_data <- read.csv(input$file$datapath, header = input$header)
       values$df_data <- fastDummies::dummy_cols(values$df_data, select_columns = 'Class', remove_first_dummy = TRUE)
       values$df_data = values$df_data[, -which(names(values$df_data) == 'Class')]
       colnames(values$df_data)[colnames(values$df_data) == names(values$df_data)[length(names(values$df_data))]] <- 'Class'
    })

    output$table <-  renderDT({
      datatable(
        values$df_data,
        options = list(
          scrollX = TRUE,
          scrollCollapse = TRUE
        )
      )
    })
    
# ------------------------------------------------------------------ type de variables partie ---------
    observeEvent(input$submit_type, {
    cat_columns <- sapply(values$df_data, function(col) {
      unique_values <- length(unique(col))
      unique_values <= input$unique_threshold
    })
    
    cat_columns <- names(cat_columns[cat_columns])
    num_columns <- setdiff(names(values$df_data), cat_columns)
    values$df_numeric_col <- num_columns
    values$df_categor_col <- cat_columns
    
    unique_values_cat <- lapply(values$df_data[, cat_columns, drop = FALSE], function(col) unique(na.omit(col)))
    num_categories_cat <- sapply(unique_values_cat, length)
    unique_values_cat <- lapply(unique_values_cat, function(x) paste(sort(x), collapse = " || "))

    
    cat_data <- data.frame(
      Colonnes_catégorielles = cat_columns,
      Nombre_de_catégories = num_categories_cat,
      Valeurs_uniques = unlist(unique_values_cat)
    )    
    rownames(cat_data) <- NULL   
    
    output$cat_columns_table <- renderDT({
      datatable(
        cat_data,
        options = list(
          pageLength = 15
        )
      )
    })
    
    output$num_columns_table <- renderDT({
      data.frame(Colonnes_numériques = num_columns)
    })
    })

    
# ------------------------------------------------------------------ Missing values ---------
    observeEvent(input$submit_missing, {
      
    if (input$missing_method == "delete") {
      temp <- na.omit(values$df_data)
      values$df_data <- temp
    } else if (input$missing_method == "mean") {
      temp <- apply(values$df_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
      values$df_data <- data.frame(temp)
    } else if (input$missing_method == "median") {
      temp <- apply(values$df_data, 2, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
      values$df_data <- data.frame(temp)
    } else if (input$missing_method == "ffill") {
      temp <- zoo::na.locf(values$df_data)
      values$df_data <- temp
    }
      
    })
    
    output$data_table <- renderDT({
      datatable(
        values$df_data,
        options = list(
          scrollX = TRUE,
          scrollCollapse = TRUE
        )
      )
      })
  

    
# ------------------------------------------------------------------ Outliers partie ---------
    
    observe({
      updateSelectInput(session, "numeric_column", choices = values$df_numeric_col)
    })
    
    observeEvent(input$submit_outlier, {
      
      if (input$remove_outliers) {
        q75 <- quantile(values$df_data[[input$numeric_column]], 0.75)
        q25 <- quantile(values$df_data[[input$numeric_column]], 0.25)
        iqr <- q75 - q25
        values$df_data <- values$df_data[values$df_data[[input$numeric_column]] <= q75 + 1.5 * iqr & values$df_data[[input$numeric_column]] >= q25 - 1.5 * iqr, ]
      }
    })
    
    output$boxplot_outlier <- renderPlot({
        ggplot(values$df_data, aes_string(x = input$numeric_column)) +
          geom_boxplot()
      })
      
    output$table_outlier <- renderDT({
      datatable(
        values$df_data,
        options = list(
          scrollX = TRUE,
          scrollCollapse = TRUE
        )
      )
    })
    
    
    
# ------------------------------------------------------------------ Normalisation --------- 
    observe({
      updateSelectInput(session, "numeric_column_normalise", choices = values$df_numeric_col)
    })
    
    observeEvent(input$normalize_button, {
      req(input$numeric_column_normalise)
      
      normalized_column <- scale(values$df_data[, input$numeric_column_normalise])
      values$df_data[, input$numeric_column_normalise] <- round(normalized_column,3)
      
      output$status_norm <- renderText({
        paste("Colonne", input$numeric_column_normalise, "normalisé.")
      })
    })
    
    output$table_normalise <- renderDT({
      datatable(
        values$df_data,
        options = list(
          scrollX = TRUE,
          scrollCollapse = TRUE
        )
      )
    })
    
    
# ------------------------------------------------------------------ Dummification --------- 
    observe({
      updateSelectInput(session, "column_dummifie", choices = values$df_categor_col)
    })
    
    observeEvent(input$dummifie_button, {
      req(input$column_dummifie)
      
      
      values$df_data <- fastDummies::dummy_cols(values$df_data, select_columns = input$column_dummifie, remove_first_dummy = TRUE)
      values$df_data = values$df_data[, -which(names(values$df_data) == input$column_dummifie)]
      
      output$status_dummifie <- renderText({
        paste("Colonne", input$column_dummifie, "dummifié")
      })
    })
    
    output$table_dummifie <- renderDT({
      datatable(
        values$df_data,
        options = list(
          scrollX = TRUE,
          scrollCollapse = TRUE
        )
      )
    })
    

# ------------------------------------------------------------------ desequilibre --------- 
    # ------------------------------------------------------------------ desequilibre --------- 
    observe({
      updateSelectInput(session, "column_desequilibre", choices = names(data()))
    })
    
    observeEvent(input$desequilibre_button, {
      
      selectedColumn <- input$column_desequilibre
      method <- input$sampling_method
      
      if (method == "over") {
        
        # Over-sampling
        majority_class <- which.max(table(values$df_data[[selectedColumn]]))
        minority_class <- which.min(table(values$df_data[[selectedColumn]]))
        sample_size <- table(values$df_data[[selectedColumn]])[majority_class]
        minority_samples <- values$df_data[values$df_data[[selectedColumn]] == names(minority_class), ]
        over_samples <- minority_samples[sample(nrow(minority_samples), sample_size, replace = TRUE), ]
        values$df_data <- rbind(values$df_data, over_samples)
        
      } else if (method == "under") {
        
        # Under-sampling
        majority_class <- which.max(table(values$df_data[[selectedColumn]]))
        minority_class <- which.min(table(values$df_data[[selectedColumn]]))
        sample_size <- table(values$df_data[[selectedColumn]])[minority_class]
        majority_samples <- values$df_data[values$df_data[[selectedColumn]] == names(majority_class), ]
        under_samples <- majority_samples[sample(nrow(majority_samples), sample_size), ]
        values$df_data <- rbind(under_samples, values$df_data[values$df_data[[selectedColumn]] != names(majority_class), ])
        
      } else if (method == "smote") {
        
        # SMOTE
        df_smote <- ROSE(Class ~ ., data = values$df_data, seed = 123)$data
        values$df_data <- df_smote
        
      }
      
    })
    
    output$table_desequilibre <- renderDT({
      datatable(
        values$df_data,
        options = list(
          scrollX = TRUE,
          scrollCollapse = TRUE
        )
      )
    })
    
    
#-------------------------------------------------------------------------
    output$test <- renderDataTable({
      
      values$df_data
      
      
    })
    # Update variable choices based on the dataframe from datatable
    observe({
      # Ensure that the dataframe is available
      req(values$df_data)
      
      # Update the input selections with the names of the columns from values$df_data
      updateSelectInput(session, "logistic_response_var", choices = names(values$df_data))
      updateSelectInput(session, "logistic_vars", choices = names(values$df_data))
    })
    
    
    
    # Observe and print the input selections for debugging
    observe({
      print(paste("Response variable:", input$logistic_response_var))
      print(paste("Predictor variables:", input$logistic_vars))
    })
    
    # Create the logistic model
    logisticModel <- reactive({
      req(input$logistic_response_var) # Ensure response variable is selected
      req(input$logistic_vars) # Ensure predictor variables are selected
      set.seed(123)  # For reproducible results
      data_partition <- createDataPartition(values$df_data[[input$logistic_response_var]], p =input$logistic_split_ratio, list = FALSE)
      train_data <- values$df_data[data_partition, ]
      test_data <- values$df_data[-data_partition, ]
     
     x<-paste(input$logistic_response_var, "~", paste(input$logistic_vars, collapse = " + "))
     model <- glm(x, data = train_data, family = "binomial")
     model
    })
    # Render logistic model summary
    output$summarymodel <- renderPrint({
      summary(logisticModel())
    })
    # # ROC Curve for Logistic Regression
    # output$logistic_roc_curve <- renderPlot({
    #   model <- logisticModel()  # Get the model object
    #   
    #   if (!is.null(model)) {
    #     # Predicting probabilities
    #     predictedscores <- predict(model,newdata=values$df_data, type = "response")
    #     
    #     # Creating a prediction object
    #     ROCRpred <- prediction(predictedscores, values$df_data$input$logistic_response_var)
    #     
    #     # Creating the ROC curve
    #     perf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
    #     plot(perf)
    #     abline(a=0, b=1, col="red")
    #   }
    # })
    
    # ROC Curve for Decision Tree
    output$logistic_roc_curve <- renderPlot({

      model <- logisticModel()
      actual_response <- values$df_data[, input$logistic_response_var]
      predicted_probs <- predict(model,newdata=values$df_data, type = "response")

      pred <- prediction(predicted_probs, actual_response)
      perf <- performance(pred, "tpr", "fpr")
      plot(perf)
      abline(a = 0, b = 1, col = "red")

      # Add AUC to the plot
      auc <- performance(pred, "auc")@y.values[[1]]
      legend("bottomright", legend = paste("AUC =", round(auc, 2)))
    })
    
    # Performance Metrics for Logistic Regression
    output$logistic_performance_metrics <- renderTable({
      req(logisticModel())  # Ensure the logistic model is ready
      model <- logisticModel()  # Get the model object
      
      if (!is.null(model)) {
        actual_response <- values$df_data[, input$logistic_response_var]
        predicted_classes <- ifelse(predict(model, newdata = values$df_data, type = "response") > input$threshold, 1, 0)  # Updated threshold
        
        confusion_matrix <- confusionMatrix(factor(predicted_classes), factor(actual_response))
        data.frame(Precision = confusion_matrix$byClass['Precision'],
                   Recall = confusion_matrix$byClass['Recall'],
                   F1 = 2 * (confusion_matrix$byClass['Precision'] * confusion_matrix$byClass['Recall']) / (confusion_matrix$byClass['Precision'] + confusion_matrix$byClass['Recall']))
      } else {
        data.frame(Precision = NA, Recall = NA, F1 = NA)  # Return NA if the model is not available
      }
    })
    #predixted plot
    output$predicted_probabilities_plot <- renderPlot({
      model <- logisticModel()  # Get the logistic model object
      
      if (!is.null(model)) {
        predicted.data <- data.frame(
          probability.of.targetVar = predict(model, newdata = values$df_data, type = "response"),
          target = values$df_data[, input$logistic_response_var]  # Use logistic_response_var here
        )
        
        predicted.data <- predicted.data[order(predicted.data$probability.of.targetVar, decreasing = FALSE), ]
        predicted.data$rank <- 1:nrow(predicted.data)
        
        ggplot(data = predicted.data, aes(x = rank, y = probability.of.targetVar)) +
          geom_point(aes(color = target), alpha = 1, shape = 4, stroke = 2) +
          xlab("Index") +
          ylab("Predicted probability of target Var")
      }
    })
    
    # Calculate and display logistic regression coefficients
    output$logistic_coef <- renderTable({
      req(logisticModel())  # Ensure the logistic model is ready
      model <- logisticModel()  # Get the model object
      
      if (!is.null(model)) {
        # Extract coefficients and convert to a data frame
        coef_df <- as.data.frame(coef(model))
        
        # Add a new column for the variable names
        coef_df$Variable <- rownames(coef_df)
        
        # Rename the column for coefficients
        names(coef_df)[1] <- "Coefficient"
        
        # Optionally, reorder the columns to have the variable names first
        coef_df <- coef_df[, c("Variable", "Coefficient")]
        
        # Return the modified data frame
        coef_df
      } else {
        data.frame(Coefficient = NA)  # Return NA if the model is not available
      }
    })
    library(ggplot2)
    
    output$logistic_coef_plot <- renderPlot({
      req(logisticModel())
      model <- logisticModel()
      
      if (!is.null(model)) {
        # Create a data frame of coefficients
        coef_df <- as.data.frame(coef(model))
        coef_df$Feature <- rownames(coef_df)
        names(coef_df)[1] <- "Coefficient"
        
        # Use ggplot2 to create the plot
        ggplot(coef_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          coord_flip() +  # Flips the axes for horizontal bars
          labs(y = "Coefficient", x = "Feature", title = "Feature Importance (Logistic Regression)")
      }
    })
    #------------------------------------- DECISION TREES --------------------------------------------------------------------
    observe({
      updateSelectInput(session, "tree_response_var", choices = names(values$df_data))
      updateSelectInput(session, "tree_vars", choices = names(values$df_data))
    })
    
    # Ajout de la réactivité pour le modèle d'arbre de décision
    treeModel <- reactive({
      req(input$tree_response_var) # Assurez-vous que la variable de réponse est sélectionnée
      req(input$tree_vars) # Assurez-vous que les variables prédictives sont sélectionnées
      formula <- as.formula(paste(input$tree_response_var, "~", paste(input$tree_vars, collapse = " + ")))
      set.seed(123)
      data_partition <- createDataPartition(values$df_data[[input$tree_response_var]], p = input$tree_split_ratio, list = FALSE)
      train_data <- values$df_data[data_partition, ]
      test_data <- values$df_data[-data_partition, ]
      model<-rpart(formula, data = train_data,method="class")
      model
    })
    
    # Afficher le résumé du modèle d'arbre de décision
    output$tree_summary <- renderPrint({
      summary(treeModel())
      #  capture.output(summary(treeModel()))
    })
    
    # Afficher le graphique de l'arbre de décision
    
    output$tree_plot <- renderPlot({
      tree <- treeModel()
      if (!is.null(tree)) {
        rpart.plot(tree, type = 1, box.palette = "RdBu", extra = 101)
        
      }
    })
    # ROC Curve for Decision Tree
    output$tree_roc_curve <- renderPlot({
      req(treeModel())
      model <- treeModel()
      actual_response <- values$df_data[, input$tree_response_var]
      predicted_probs <- predict(model, newdata = values$df_data, type = "prob")[,2]
      
      pred <- prediction(predicted_probs, actual_response)
      perf <- performance(pred, "tpr", "fpr")
      plot(perf)
      abline(a = 0, b = 1, col = "red")
      
      # Add AUC to the plot
      auc <- performance(pred, "auc")@y.values[[1]]
      legend("bottomright", legend = paste("AUC =", round(auc, 2)))
    })
    
    
    # Métriques de Performance pour l'Arbre de Décision
    output$tree_performance_metrics <- renderTable({
      req(treeModel())  # Assurez-vous que le modèle d'arbre de décision est prêt
      model <- treeModel()  # Récupérez l'objet du modèle
      # summary(model)
      if (!is.null(model)) {
        req(values$df_data)
        predicted_classes <- predict(model, newdata = values$df_data, type = "class")
        actual_classes1 <- values$df_data[, input$tree_response_var]
        
        # Calculer la matrice de confusion
        confusion_matrix <- confusionMatrix(factor(predicted_classes), factor(actual_classes1))
        
        # Retourner les métriques calculées
        data.frame(
          Precision = confusion_matrix$byClass['Precision'],
          Recall = confusion_matrix$byClass['Recall'],
          F1 = 2 * (confusion_matrix$byClass['Precision'] * confusion_matrix$byClass['Recall']) / (confusion_matrix$byClass['Precision'] + confusion_matrix$byClass['Recall'])
        )
      } else {
        data.frame(Precision = NA, Recall = NA, F1 = NA)  # Retourner NA si le modèle n'est pas disponible
      }
    })
    
    observe({
      updateSelectInput(session, "rf_response_var", choices = names(values$df_data))
      updateSelectInput(session, "rf_vars", choices = names(values$df_data))
    })
    #------------------------------------------------------ RANDOM FOREST ----------------------------------------------------------  
    # Reactive expression for Random Forest model
    # Create the Random Forest model
    rfModel <- reactive({
      req(input$rf_response_var)  # Ensure response variable is selected
      req(input$rf_vars)  # Ensure predictor variables are selected
      
      
      
      
      formula <- as.formula(paste(input$rf_response_var, "~", paste(input$rf_vars, collapse = " + ")))
      data_modified <- values$df_data
      data_modified[[input$rf_response_var]] <- as.factor(data_modified[[input$rf_response_var]])
      set.seed(123)
      data_partition <- createDataPartition(data_modified[[input$rf_response_var]], p = input$rf_split_ratio, list = FALSE)
      train_data <- data_modified[data_partition, ]
      test_data <- data_modified[-data_partition, ]
      randomForest(formula, data = train_data, ntree = input$rf_ntree)
    })
    
    
    # Model Summary
    output$rf_summary <- renderPrint({
      req(rfModel())
      rfModel()
    })
    # ROC Curve for Random Forest
    output$rf_roc_curve <- renderPlot({
      req(rfModel())
      model <- rfModel()
      actual_response <- values$df_data[, input$rf_response_var]
      predicted_probs <- predict(model, newdata = values$df_data, type = "prob")[,2]
      
      pred <- prediction(predicted_probs, actual_response)
      perf <- performance(pred, "tpr", "fpr")
      plot(perf)
      abline(a = 0, b = 1, col = "red")
      
      # Add AUC to the plot
      auc <- performance(pred, "auc")@y.values[[1]]
      legend("bottomright", legend = paste("AUC =", round(auc, 2)))
    })
    
    
    # Feature Importance Plot
    output$rf_importance_plot <- renderPlot({
      req(rfModel())
      model <- rfModel()
      
      # Extracting importance data
      importance_data <- as.data.frame(importance(model))
      importance_data$Feature <- rownames(importance_data)
      importance_data <- melt(importance_data, id.vars = "Feature")
      
      # Using ggplot2 for plotting
      ggplot(importance_data, aes(x = Feature, y = value, fill = variable)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x labels
        labs(y = "Importance", x = "Features", title = "Feature Importance", fill = "Metric")
    })
    
    
    output$rf_importance <- renderTable({
      req(rfModel())  # Ensure the Random Forest model is ready
      model <- rfModel()  # Get the model object
      
      if (!is.null(model)) {
        # Extract feature importance and convert to a data frame
        importance_df <- as.data.frame(importance(model))
        
        # Add a new column for the feature names
        importance_df$Feature <- rownames(importance_df)
        
        # Optionally, reorder the columns to have the feature names first
        importance_df <- importance_df[, c("Feature", names(importance_df)[1:length(importance_df)-1])]
        
        # Return the modified data frame
        importance_df
      } else {
        data.frame(Feature = NA, Importance = NA)  # Return NA if the model is not available
      }
    })
    
    # Performance Metrics
    output$rf_performance_metrics <- renderTable({
      req(rfModel())
      model <- rfModel()
      actual_response <- values$df_data[, input$rf_response_var]
      predicted_classes <- predict(model, newdata = values$df_data)
      
      confusion_matrix <- confusionMatrix(factor(predicted_classes), factor(actual_response))
      data.frame(
        Precision = confusion_matrix$byClass['Precision'],
        Recall = confusion_matrix$byClass['Recall'],
        F1 = 2 * (confusion_matrix$byClass['Precision'] * confusion_matrix$byClass['Recall']) / (confusion_matrix$byClass['Precision'] + confusion_matrix$byClass['Recall'])
      )
    })
    
    
    
      
})
