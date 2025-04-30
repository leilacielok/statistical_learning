run_lifeexp_svm <- function(data) {
  library(caret)
  library(ggplot2)
  
  data <- data[, !(colnames(data) %in% c("Country", "Country_std"))]
  
  train_control <- trainControl(method = "none")
  
  model <- train(
    life_expectancy ~ ., 
    data = data, 
    method = "svmRadial", 
    trControl = train_control
  )

  preds <- predict(model, newdata = data)

  rmse <- RMSE(preds, data$life_expectancy)
  r_squared <- R2(preds, data$life_expectancy)
  
  cat("RMSE: ", rmse, "\n")
  cat("R²: ", r_squared, "\n")
  
  p1 <- ggplot(data = data.frame(Observed = data$life_expectancy, Predicted = preds), aes(x = Observed, y = Predicted)) +
    geom_point(color = "lightgreen", size = 2) +
    geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed", linewidth = 1) +
    ggtitle("SVM Regression: Predicted vs Observed") +
    xlab("Observed Values") +
    ylab("Predicted Values") +
    theme_minimal() +
    annotate("text", x = min(data$life_expectancy), y = max(preds), 
             label = paste("R² =", round(r_squared, 2)), hjust = 0, size = 4, color = "blue")
  
  importance <- varImp(model, scale = TRUE)$importance
  importance$Feature <- rownames(importance)
  
  p2 <- ggplot(importance, aes(x = reorder(Feature, Overall), y = Overall)) +
    geom_col(fill = "skyblue") +
    coord_flip() +
    ggtitle("Feature Importance (SVM Approximation)") +
    xlab("Features") +
    ylab("Importance") +
    theme_minimal()
  
  return(list(
    svm_model = model,
    rmse = rmse,
    r2 = r_squared,
    plot = p1,
    imp_plot = p2
  ))
}
