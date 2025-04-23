run_status_rf <- function(cleaned_data) {
  library(randomForest)
  library(caret)
  
  source("Modules/Utils.R")
  data_split <- prepare_status_data(cleaned_data)
  train_data <- data_split$train
  test_data  <- data_split$test
  
  train_data$Status <- factor(train_data$Status, levels = c(0, 1), labels = c("Class0", "Class1"))
  test_data$Status <- factor(test_data$Status, levels = c(0, 1), labels = c("Class0", "Class1"))
  
  ctrl <- trainControl(
    method = "cv", 
    number = 10, 
    classProbs = TRUE, 
    summaryFunction = twoClassSummary)
  
  rf_model_cv <- train(
    Status ~ ., 
    data = train_data, 
    method = "rf", 
    trControl = ctrl, 
    tuneGrid = expand.grid(mtry = c(2, 3, 4, 5)),
    metric = "ROC",  
    ntree = 100
  )
  
  # Variables' importance
  importance <- varImp(rf_model_cv, scale = FALSE)
  rf_df <- data.frame(
    Feature = rownames(importance$importance),
    Coefficient = as.numeric(importance$importance[, 1])
  )
  
  pred <- predict(rf_model_cv, newdata = test_data)
  cm <- confusionMatrix(pred, test_data$Status)
  
  return(list(
    model = rf_model_cv,
    confusion = cm,
    important_vars = rf_df,
    predictions = pred
  ))
}
