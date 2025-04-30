run_lifeexp_logistic <- function(cleaned_data) {
  library(caret)
  
  source("Modules/Utils.R")
  data_split <- prepare_lifeexp_data(cleaned_data)
  train_data <- data_split$train
  test_data <- data_split$test
  
  train_data <- train_data %>% select(-any_of(c("Country", "Country_std")))
  test_data  <- test_data %>% select(-any_of(c("Country", "Country_std")))

  model <- train(
    lifeexp_cat ~ .,
    data = train_data,
    method = "multinom",
    trControl = trainControl(method = "none")
  )
  
  pred <- predict(model, newdata = test_data)
  pred <- factor(pred, levels = levels(test_data$lifeexp_cat))
  
  cm <- confusionMatrix(pred, test_data$lifeexp_cat)
  
  return(list(
    model = model,
    confusion = cm,
    predictions = pred
  ))
}
