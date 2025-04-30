run_lifeexp_ridge <- function(data) {
  library(glmnet)
  library(ggplot2)
  
  x <- model.matrix(life_expectancy ~ . - Country -Country_std, data = data)[, -1]
  y <- data$life_expectancy
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  plot(ridge_model)
  title("Ridge Regression - Coefficients vs Log(Lambda)", line = 2.5)
  
  preds_min <- predict(ridge_model, s = "lambda.min", newx = x)
  rmse_min <- sqrt(mean((y - preds_min)^2))  
  r_squared_min <- cor(y, preds_min)^2
  
  preds_1se <- predict(ridge_model, s = "lambda.1se", newx = x)
  rmse_1se <- sqrt(mean((y - preds_1se)^2))
  r_squared_1se <- cor(y, preds_1se)^2
  
  cat("Optimal Lambda:", ridge_model$lambda.min, "\n")
  cat("Lambda 1SE:", ridge_model$lambda.1se, "\n")
  cat("R squared for minimum lambda: ", round(r_squared_min, 4), "\n")
  cat("R squared for lambda 1se: ", round(r_squared_1se, 4), "\n")
  cat("RMSE lambda.min:", round(rmse_min, 4), "\n")
  cat("RMSE lambda.1se:", round(rmse_1se, 4), "\n")
  
  plot(y, preds_min,
       main = "Ridge Regression: Predicted vs Real",
       xlab = "Observed Values", ylab = "Predicted Values",
       pch = 19, col = "blue")
  abline(0, 1, col = "red", lwd = 2)
  
  coefficients_ridge <- coef(ridge_model, s = "lambda.min")
  coefficients_ridge_df <- data.frame(Feature = rownames(coefficients_ridge), 
                                      Coefficient = as.vector(coefficients_ridge))
  coefficients_ridge_df <- coefficients_ridge_df[!(coefficients_ridge_df$Feature %in% c("(Intercept)", "Country")), ]
    
  # Order by importance
  coefficients_ridge_df$abs_coefficient <- abs(coefficients_ridge_df$Coefficient)
  coefficients_ridge_df <- coefficients_ridge_df[order(-coefficients_ridge_df$abs_coefficient), ]
    
  # Visualization
  importance_plot_ridge <- ggplot(coefficients_ridge_df, aes(x = reorder(Feature, abs_coefficient), y = abs_coefficient)) +
    geom_col(fill = "skyblue") +
    coord_flip() +
    ggtitle("Ridge Regression: Feature Importance") +
    xlab("Features") +
    ylab("Absolute Coefficients") +
    theme_minimal()
  
  return(list(
    ridge_model = ridge_model,
    rmse_min = rmse_min,
    rmse_1se = rmse_1se,
    r_squared_min = r_squared_min,
    r_squared_1se = r_squared_1se,
    imp_plot = importance_plot_ridge
  ))
}

plot_ridge_predictions <- function(ridge_model, data) {
  library(glmnet)
  
  x <- model.matrix(life_expectancy ~ . - Country -Country_std, data = data)[, -1]
  y <- data$life_expectancy
  
  preds_min <- predict(ridge_model, s = "lambda.min", newx = x)
  preds_1se <- predict(ridge_model, s = "lambda.1se", newx = x)
  
  par(mfrow = c(1, 2))
  
  # Plot lambda.min
  plot(y, preds_min,
       main = "Predicted vs Observed (lambda.min)",
       xlab = "Observed Values", ylab = "Predicted Values",
       pch = 19, col = "lightgreen")
  abline(0, 1, col = "blue", lwd = 2)
  
  # Plot lambda.1se
  plot(y, preds_1se,
       main = "Predicted vs Observed (lambda.1se)",
       xlab = "Observed Values", ylab = "Predicted Values",
       pch = 19, col = "lightgreen")
  abline(0, 1, col = "blue", lwd = 2)
  
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