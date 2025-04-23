run_status_tree <- function(cleaned_data) {
  library(rpart)
  library(rpart.plot)
  library(caret)
  
  source("Modules/Utils.R")
  data_split <- prepare_status_data(cleaned_data)
  train_data <- data_split$train
  test_data  <- data_split$test
  
  train_data$Status <- factor(train_data$Status, levels = c(0, 1), labels = c("Class0", "Class1"))
  test_data$Status <- factor(test_data$Status, levels = c(0, 1), labels = c("Class0", "Class1"))
  
  X <- model.matrix(Status ~ ., data = train_data)[, -1]
  Y <- as.factor(train_data$Status)
  
  ctrl <- trainControl(
    method = "cv", 
    number = 10, 
    classProbs = TRUE, 
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )
  
  tree_model_cv <- train(
    Status ~ ., 
    data = train_data, 
    method = "rpart", 
    trControl = ctrl, 
    metric = "ROC"  
  )
  
  # Variables' importance
  importance <- varImp(tree_model_cv, scale = FALSE)
  tree_df <- data.frame(
    Feature = rownames(importance$importance),
    Coefficient = as.numeric(importance$importance[, 1])
  )
  
  return(list(
    model = tree_model_cv,
    important_vars = tree_df
  ))
}
