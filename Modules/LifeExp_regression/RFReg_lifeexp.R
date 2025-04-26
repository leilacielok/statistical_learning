run_lifeexp_rfreg <- function(data) {
  library(randomForest)
  library(caret)
  library(Metrics)  
  library(ggplot2)
  
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
  
  plot_data <- data.frame(
    Observed = test_data$life_expectancy,
    Predicted = pred
  )
  
  plot_rf <- ggplot(plot_data, aes(x = Observed, y = Predicted)) +
    geom_point(color = "forestgreen", alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Random Forest: Observed vs Predicted",
      x = "Observed Life Expectancy",
      y = "Predicted Life Expectancy"
    ) +
    theme_minimal()
  
  imp <- importance(model)
  imp_data <- data.frame(
    Variable = rownames(imp),
    Importance = imp[, 1]
  )
  
  plot_imp <- ggplot(imp_data, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "darkblue") +
    coord_flip() +
    labs(
      title = "Random Forest:Variables Importance",
      x = "Variable",
      y = "Importance"
    ) +
    theme_minimal()
  
  
  return(list(
    model = model,
    predictions = pred,
    actual = test_data$life_expectancy,
    rmse = rmse,
    r2 = r2,
    plot = plot_rf,
    var_imp_plot = plot_imp
  ))
}
