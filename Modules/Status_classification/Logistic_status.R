run_status_logistic <- function(cleaned_data) {
  library(glmnet)
  library(detectseparation)
  library(caret)

  source("Modules/Utils.R")
  data_split <- prepare_status_data(cleaned_data)
  train_data <- data_split$train
  test_data  <- data_split$test
  
  X <- model.matrix(Status ~ ., data = train_data)[, -1]
  Y <- as.factor(train_data$Status)
  
  detect_separation(x = X, y = Y, family = binomial())
  
  # LASSO
  cv_fit <- cv.glmnet(X, Y, family = "binomial", alpha = 1)
  best_model <- glmnet(X, Y, family = "binomial", alpha = 1, lambda = cv_fit$lambda.min)
  
  x_test <- model.matrix(Status ~ ., test_data)[, -1]
  y_test <- as.factor(test_data$Status)
  prob_pred <- predict(best_model, newx = x_test, type = "response")
  class_pred <- as.factor(ifelse(prob_pred >= 0.5, 1, 0))
  
  # Confusion Matrix
  cm <- confusionMatrix(class_pred, y_test)
  
  # Variables importance
  coef_lasso <- coef(best_model)
  lasso_df <- data.frame(
    Feature = rownames(coef_lasso),
    Coefficient = as.numeric(coef_lasso)
  ) %>% filter(Coefficient != 0 & Feature != "(Intercept)")
  
  return(list(
    model = best_model,
    confusion = cm,
    important_vars = lasso_df
  ))
}

