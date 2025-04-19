run_status_rf <- function(cleaned_data) {
  library(randomForest)
  library(caret)
  
  source("Modules/Utils.R")
  data_split <- prepare_status_data(cleaned_data)
  train_data <- data_split$train
  test_data  <- data_split$test
  
  rf_model <- randomForest(as.factor(Status) ~ ., data = train_data, ntree = 100, importance = TRUE)
  rf_pred <- predict(rf_model, newdata = test_data)
  
  y_test <- factor(test_data$Status)
  rf_pred <- factor(rf_pred, levels = levels(y_test))
  
  cm <- confusionMatrix(rf_pred, y_test)
  
  return(list(
    model = rf_model,
    confusion = cm,
    importance_plot = function() varImpPlot(rf_model)
  ))
}
