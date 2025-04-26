run_lifeexp_ridge <- function(data) {
  library(glmnet)
  
  x <- model.matrix(life_expectancy ~ ., data = data)[, -1]
  y <- data$life_expectancy
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  plot(ridge_model)
  title("Ridge Regression - Coefficients vs Log(Lambda)")
  
  preds <- predict(ridge_model, s = "lambda.min", newx = x)
  r_squared <- cor(y, preds)^2
  
  cat("Optimal Lambda:", ridge_model$lambda.min, "\n")
  cat("Lambda 1SE:", ridge_model$lambda.1se, "\n")
  cat("R squared: ", round(r_squared, 4), "\n")
  
  plot(y, preds,
       main = "Ridge Regression: Predicted vs Real",
       xlab = "Observed Values", ylab = "Predicted Values",
       pch = 19, col = "blue")
  abline(0, 1, col = "red", lwd = 2)
  
  return(ridge_model)
}

plot_ridge_predictions <- function(ridge_model, data) {
  library(glmnet)
  
  x <- model.matrix(life_expectancy ~ ., data = data)[, -1]
  y <- data$life_expectancy
  
  preds_min <- predict(ridge_model, s = "lambda.min", newx = x)
  preds_1se <- predict(ridge_model, s = "lambda.1se", newx = x)
  
  # Setup grafico: 1 riga, 2 colonne
  par(mfrow = c(1, 2))
  
  # Plot lambda.min
  plot(y, preds_min,
       main = "Predetti vs Reali (lambda.min)",
       xlab = "Valori Reali", ylab = "Valori Predetti",
       pch = 19, col = "blue")
  abline(0, 1, col = "red", lwd = 2)
  
  # Plot lambda.1se
  plot(y, preds_1se,
       main = "Predetti vs Reali (lambda.1se)",
       xlab = "Valori Reali", ylab = "Valori Predetti",
       pch = 19, col = "green")
  abline(0, 1, col = "red", lwd = 2)
  
  par(mfrow = c(1, 1))
  
  # RMSE 
  rmse_min <- sqrt(mean((y - preds_min)^2))
  rmse_1se <- sqrt(mean((y - preds_1se)^2))
  
  # Output 
  cat("RMSE lambda.min:", round(rmse_min, 4), "\n")
  cat("RMSE lambda.1se:", round(rmse_1se, 4), "\n")
  
  return(list(
    preds_min = preds_min,
    preds_1se = preds_1se,
    rmse_min = rmse_min,
    rmse_1se = rmse_1se
  ))
}

