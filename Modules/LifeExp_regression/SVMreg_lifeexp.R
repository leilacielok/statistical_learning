run_lifeexp_svm <- function(data) {
  library(e1071)
  library(caret)
  
  model <- svm(life_expectancy ~ ., data = data, type = "eps-regression", kernel = "radial")

  preds <- predict(model, newdata = data)
  
  rmse <- RMSE(preds, data$life_expectancy)
  r_squared <- R2(preds, data$life_expectancy)
  
  cat("RMSE: ", rmse, "\n")
  cat("R²: ", r_squared, "\n")
  
  plot(data$life_expectancy, preds,
       main = "SVM Regression: Predicted vs Real",
       xlab = "Observed Values", ylab = "Predicted Values",
       pch = 19, col = "darkgreen")
  abline(0, 1, col = "red", lwd = 2)
  
  residuals <- data$life_expectancy - preds
  hist(residuals, breaks = 20, col = "lightblue",
       main = "SVM Residuals", xlab = "Prediction Errors")
  
  return(list(
    svm_model = model,
    rmse = rmse,
    r2 = r_squared
  ))
}
