run_status_tree <- function(cleaned_data) {
  library(rpart)
  library(rpart.plot)
  library(caret)
  
  source("Modules/Utils.R")
  data_split <- prepare_status_data(cleaned_data)
  train_data <- data_split$train
  test_data  <- data_split$test
  
  tree_model <- rpart(Status ~ ., data = train_data, method = "class")
  tree_pred <- predict(tree_model, newdata = test_data, type = "class")
  
  y_test <- factor(test_data$Status)
  tree_pred <- factor(tree_pred, levels = levels(y_test))
  
  cm <- confusionMatrix(tree_pred, y_test)
  
  importance <- tree_model$variable.importance
  tree_df <- data.frame(
    Feature = names(importance),
    Coefficient = as.numeric(importance)
  )
  
  return(list(
    model = tree_model,
    confusion = cm,
    important_vars = tree_df
  ))
}
