run_lifeexp_tree <- function(cleaned_data) {
  library(rpart)
  library(rpart.plot)
  library(caret)
  
  source("Modules/Utils.R", local = new.env())
  data_split <- prepare_lifeexp_data(cleaned_data)
  train_data <- data_split$train
  test_data <- data_split$test
  
  model <- rpart(lifeexp_cat ~ ., data = train_data, method = "class")
  pred <- predict(model, newdata = test_data, type = "class")
  pred <- factor(pred, levels = levels(test_data$lifeexp_cat))
  
  cm <- confusionMatrix(pred, test_data$lifeexp_cat)
  
  return(list(
    model = model,
    confusion = cm,
    predictions = pred
  ))
}
