compare_regression_models <- function(linreg_result, ridge_result, svm_result, rf_result) {
  library(ggplot2)
  library(gridExtra)
  
  results_comparison <- data.frame(
    Model = c("Linear Regression", "Ridge Regression", "SVM Regression", "Random Forest"),
    RMSE = c(
      linreg_result$rmse, 
      ridge_result$rmse_1se, 
      svm_result$rmse, 
      rf_result$rmse
    ),
    R_squared = c(
      linreg_result$r2, 
      ridge_result$r_squared_1se, 
      svm_result$r2, 
      rf_result$r2
    ),
    stringsAsFactors = FALSE
  )
  
  print(results_comparison)
  
  # RSME visualization
  p1 <- ggplot(results_comparison, aes(x = Model, y = RMSE, fill = Model)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_text(aes(label = round(RMSE, 2)), vjust = -0.3) +
    labs(title = "Models Comparison: RMSE", y = "RMSE", x = "Model") +
    theme_minimal()
  
  # R squared visualization
  p2 <- ggplot(results_comparison, aes(x = Model, y = R_squared, fill = Model)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_text(aes(label = round(R_squared, 2)), vjust = -0.3) +
    labs(title = "Models Comparison: R²", y = "R²", x = "Model") +
    theme_minimal()
  
  grid.arrange(p1, p2, nrow = 2)
  
  return(results_comparison)
}

