run_lifeexp_rfreg <- function(data) {
  library(randomForest)
  library(caret)
  library(Metrics)  
  
  source("Modules/Utils.R", local = new.env())
  set.seed(123)
  trainIndex <- createDataPartition(data$life_expectancy, p = 0.7, list = FALSE)
  train_data <- data[trainIndex, ]
  test_data <- data[-trainIndex, ]
  
  
  set.seed(123)
  model <- randomForest(
    life_expectancy ~ ., 
    data = train_data,
    ntree = 100,
    importance = TRUE
  )
  
  pred <- predict(model, newdata = test_data)
  
  rmse <- RMSE(pred, test_data$life_expectancy)
  r2 <- R2(pred, test_data$life_expectancy)
  
  cat("RMSE: ", rmse, "\n")
  cat("R²: ", r2, "\n")
  
  return(list(
    model = model,
    predictions = pred,
    actual = test_data$life_expectancy,
    rmse = rmse,
    r2 = r2
  ))
}
