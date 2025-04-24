run_lifeexp_rfreg <- function(cleaned_data) {
  library(randomForest)
  library(caret)
  library(Metrics)  
  
  source("Modules/Utils.R", local = new.env())
  set.seed(123)
  trainIndex <- createDataPartition(cleaned_data$life_expectancy, p = 0.7, list = FALSE)
  train_data <- cleaned_data[trainIndex, ]
  test_data <- cleaned_data[-trainIndex, ]
  
  
  set.seed(123)
  model <- randomForest(
    life_expectancy ~ ., 
    data = train_data,
    ntree = 100,
    importance = TRUE
  )
  
  pred <- predict(model, newdata = test_data)
  
  rmse_val <- RMSE(pred, test_data$life_expectancy)
  r2_val <- R2(pred, test_data$life_expectancy)
  
  cat("RMSE: ", rmse_val, "\n")
  cat("R²: ", r2_val, "\n")
  
  return(list(
    model = model,
    predictions = pred,
    actual = test_data$life_expectancy,
    rmse = rmse_val,
    r2 = r2_val
  ))
}
