run_lifeexp_rf <- function(cleaned_data) {
  library(randomForest)
  library(caret)
  
  source("Modules/Utils.R", local = new.env())
  data_split <- prepare_lifeexp_data(cleaned_data)
  train_data <- data_split$train
  test_data <- data_split$test
  
  model <- randomForest(lifeexp_cat ~ ., data = train_data, ntree = 100, importance = TRUE)
  pred <- predict(model, newdata = test_data)
  pred <- factor(pred, levels = levels(test_data$lifeexp_cat))
  
  cm <- confusionMatrix(pred, test_data$lifeexp_cat)
  
  return(list(
    model = model,
    confusion = cm,
    predictions = pred
  ))
}
