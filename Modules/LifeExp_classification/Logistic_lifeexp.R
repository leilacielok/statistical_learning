run_lifeexp_logistic <- function(cleaned_data) {
  library(nnet)  
  library(caret)
  
  source("Modules/Utils.R")
  data_split <- prepare_lifeexp_data(cleaned_data)
  train_data <- data_split$train
  test_data <- data_split$test
  
  model <- multinom(lifeexp_cat ~ ., data = train_data)
  
  pred <- predict(model, newdata = test_data)
  pred <- factor(pred, levels = levels(test_data$lifeexp_cat))
  
  cm <- confusionMatrix(pred, test_data$lifeexp_cat)
  
  return(list(
    model = model,
    confusion = cm,
    predictions = pred
  ))
}
run_lifeexp_logistic(cleaned_data)
