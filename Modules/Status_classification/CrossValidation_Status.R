cross_validate_model <- function(data, method, tune_grid = NULL, ...) {
  library(caret)
  
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
  
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
       xlab = "False Positive Rate", ylab = "True Positive Rate",
       main = "ROC Curves Comparison")
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    probs <- model$pred
    positive_class <- model$levels[2]
    roc_obj <- roc(probs$obs, probs[[positive_class]])
    
    auc_values[i] <- round(auc(roc_obj), 3)
    
    lines(roc_obj, col = colors[i], lwd = 2)
    model_names[i] <- paste0(model_names[i], " (AUC = ", auc_values[i], ")")
  }
  
  legend("bottomright", legend = model_names, col = colors[1:length(models)], lwd = 2)
}
