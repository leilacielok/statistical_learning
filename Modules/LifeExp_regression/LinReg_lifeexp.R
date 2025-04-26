run_lifeexp_linreg <- function(data) {
  library(caret)
  library(ggplot2)
  
  target_var <- "life_expectancy"
  predictors <- setdiff(names(data), c(target_var, "Country"))
  
  formula <- as.formula(paste(target_var, "~", paste(predictors, collapse = " + ")))
  
  set.seed(123)
  model <- train(
    formula,
    data = data,
    method = "lm",
    trControl = trainControl(method = "cv", number = 10)
  )
  
  predictions <- predict(model, newdata = data)
  
  rmse <- RMSE(predictions, data[[target_var]])
  r2 <- R2(predictions, data[[target_var]])
  
  # Dataframe for visualization
  plot_data <- data.frame(
    Observed = data[[target_var]],
    Predicted = predictions
  )
  
  plot <- ggplot(plot_data, aes(x = Observed, y = Predicted)) +
    geom_point(color = "steelblue", alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
    labs(
      title = "Observations vs predictions - Life Expectancy",
      x = "Observed Values",
      y = "Predicted Values"
    ) +
    theme_minimal()
  
  cat("Linear Regression RMSE:", round(rmse, 4), "\n")
  cat("Linear Regression R²:", round(r2, 4), "\n")
  
  coefficients <- summary(model$finalModel)$coefficients
  coefficients_df <- data.frame(Feature = rownames(coefficients), 
                                Coefficient = coefficients[, 1])
  
  # Order by importance
  coefficients_df$abs_coefficient <- abs(coefficients_df$Coefficient)
  coefficients_df <- coefficients_df[order(-coefficients_df$abs_coefficient), ]
  
  # Visualization
  importance_plot <- ggplot(coefficients_df, aes(x = reorder(Feature, abs_coefficient), y = abs_coefficient)) +
    geom_col(fill = "skyblue") +
    coord_flip() +
    ggtitle("Linear Regression: Feature Importance") +
    xlab("Features") +
    ylab("Absolute Coefficients") +
    theme_minimal()
  
  return(list(
    linreg_model = model,
    linreg_plot = plot,
    imp_plot = importance_plot,
    rmse = rmse,
    r2 = r2
    ))
}

