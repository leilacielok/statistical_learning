run_lifeexp_svm <- function(data) {
  library(e1071)
  library(Metrics)
  model <- svm(life_expectancy ~ ., data = data, type = "eps-regression", kernel = "radial")

  preds <- predict(model, newdata = data)
  rmse_svm <- rmse(actual = cleaned_data$life_expectancy, predicted = preds)
  r_squared <- cor(cleaned_data$life_expectancy, preds)^2
  
  cat("RMSE: ", rmse_svm, "\n")
  cat("R²: ", r_squared, "\n")
  
  plot(data$life_expectancy, preds,
       main = "SVM Regression: Predicted vs Real",
       xlab = "Observed Values", ylab = "Predicted Values",
       pch = 19, col = "darkgreen")
  abline(0, 1, col = "red", lwd = 2)
  
  residuals <- data$life_expectancy - preds
  hist(residuals, breaks = 20, col = "lightblue",
       main = "SVM Residuals", xlab = "Prediction Errors")
  
  return(model)
}
