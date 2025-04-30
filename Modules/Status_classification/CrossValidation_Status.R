cross_validate_model <- function(data, method, tune_grid = NULL, ...) {
  library(caret)
  
  if ("Country" %in% colnames(data)) {
    data <- data %>% select(-Country)
  }
  
  data$Status <- factor(data$Status, levels = c(0, 1))
  levels(data$Status) <- make.names(levels(data$Status))
  
  set.seed(123) 
  folds <- 5
  
  # Setup 
  ctrl <- trainControl(
    method = "cv",
    number = folds,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )
  
  # Training
  model <- train(
    Status ~ .,
    data = data,
    method = method,
    trControl = ctrl,
    metric = "ROC",
    tuneGrid = tune_grid
  )
  
  return(model)
}

compare_models_roc <- function(models, model_names) {
  library(pROC)

  colors <- c("blue", "green", "orange", "purple", "brown", "cyan")
  auc_values <- numeric(length(models))
  
  # ROC curves comparison
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
       xlab = "False Positive Rate", ylab = "True Positive Rate",
       main = "ROC Curves Comparison")
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    probs <- model$pred
    if (is.null(probs)) {
      stop("Le probabilità di classe non sono disponibili.")
    }
    
    positive_class <- model$levels[2]
    if (!positive_class %in% colnames(probs)) {
      stop("La classe positiva non è presente nei dati di previsione.")
    }
    
    roc_obj <- roc(probs$obs, probs[[positive_class]])
    
    auc_values[i] <- round(auc(roc_obj), 3)
    
    lines(roc_obj, col = colors[i], lwd = 2, alpha = 0.7)
    model_names[i] <- paste0(model_names[i], " (AUC = ", auc_values[i], ")")
  }
  
  legend("bottomright", legend = model_names, col = colors[1:length(models)], lwd = 2)
}

  
compare_models_vars <- function(models, model_names, top_n = 8) {
  library(ggplot2)
  library(dplyr)
  
  important_vars_list <- lapply(seq_along(models), function(i) {
    model <- models[[i]]
    if (!is.null(model$important_vars) && nrow(model$important_vars) > 0) {
      model$important_vars$Model <- model_names[i]
      model$important_vars <- model$important_vars %>%
        arrange(desc(abs(Coefficient))) %>%
        slice_head(n = top_n)
      return(model$important_vars)
    } else {
      return(NULL)
    }
  })
  
  important_vars_df <- do.call(rbind, important_vars_list)
  
  if (!is.null(important_vars_df) && nrow(important_vars_df) > 0) {
    ggplot(important_vars_df, aes(x = reorder(Feature, Coefficient), y = Coefficient, fill = Model)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      theme_minimal() +
      xlab("Feature") +
      ylab("Importance Coefficient") +
      ggtitle(paste("Most Important Variables per Model")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    print("No important variables found for the models.")
  }
}
