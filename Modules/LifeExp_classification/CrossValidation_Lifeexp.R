cross_validate_model <- function(data, method, tune_grid = NULL) {
  library(caret)
  set.seed(123)
  
  data_split <- prepare_lifeexp_data(data)
  train_data <- data_split$train
  
  ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final")
  
  model <- train(
    lifeexp_cat ~ .,
    data = train_data,
    method = method,
    trControl = ctrl,
    tuneGrid = tune_grid
  )
  
  return(model)
}

compare_models_multiclass <- function(models_list, model_names) {
  library(caret)
  library(ggplot2)
  library(dplyr)
  
  results <- data.frame(
    Model = character(),
    Accuracy = numeric(),
    Kappa = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(models_list)) {
    model <- models_list[[i]]
    
    pred <- model$pred
    if ("Resample" %in% colnames(pred)) {
      pred <- pred %>%
        group_by(rowIndex) %>%
        filter(row_number() == 1) %>% 
        ungroup()
    }
    
    cm <- confusionMatrix(pred$pred, pred$obs)
    
    results <- rbind(results, data.frame(
      Model = model_names[i],
      Accuracy = cm$overall["Accuracy"],
      Kappa = cm$overall["Kappa"]
    ))
  }
  
  # Bar Plot
  results_long <- tidyr::pivot_longer(results, cols = c("Accuracy", "Kappa"), names_to = "Metric", values_to = "Value")
  
  ggplot(results_long, aes(x = Model, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ylim(0, 1) +
    labs(title = "Multiclass Model Comparison", y = "Metric Value") +
    theme_minimal()
}

