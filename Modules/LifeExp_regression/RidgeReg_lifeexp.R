run_lifeexp_ridge <- function(data) {
  library(glmnet)
  
  x <- model.matrix(life_expectancy ~ ., data = data)[, -1]
  y <- data$life_expectancy
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  plot(ridge_model)
  title("Ridge Regression - Coefficients vs Log(Lambda)")
  
  preds <- predict(ridge_model, s = "lambda.min", newx = x)
  
  plot(y, preds,
       main = "Ridge Regression: Predicted vs Real",
       xlab = "Observed Values", ylab = "Predicted Values",
       pch = 19, col = "blue")
  abline(0, 1, col = "red", lwd = 2)
  
  return(ridge_model)
}

