run_status_tree <- function(cleaned_data) {
  library(rpart)
  library(rpart.plot)
  library(caret)
  
  source("Modules/Utils.R")
  data_split <- prepare_status_data(cleaned_data)
  train_data <- data_split$train
  test_data  <- data_split$test
  
  train_data <- train_data %>% select(-Country_std)
  test_data <- test_data %>% select(-Country_std)
  
  # Assicurati che Status sia un fattore con i livelli giusti
  train_data$Status <- factor(train_data$Status, levels = c(0, 1), labels = c("Class0", "Class1"))
  test_data$Status <- factor(test_data$Status, levels = c(0, 1), labels = c("Class0", "Class1"))
  
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
  
  pred <- predict(tree_model_cv, newdata = test_data, type = "prob")
  pred_class <- factor(ifelse(pred$Class1 > 0.5, "Class1", "Class0"), levels = c("Class0", "Class1"))
  
  common_indices <- which(!is.na(pred_class) & !is.na(test_data$Status))
  pred_class <- pred_class[common_indices]
  test_data$Status <- test_data$Status[common_indices]
  
  cm <- confusionMatrix(pred_class, test_data$Status)
  
  return(list(
    model = tree_model_cv,
    confusion = cm,
    important_vars = tree_df,
    predictions = pred
  ))
}


