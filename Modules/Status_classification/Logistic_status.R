run_status_logistic <- function(cleaned_data) {
  library(glmnet)
  library(detectseparation)
  library(caret)
  library(dplyr)
  library(pROC)
  
  source("Modules/Utils.R")
  data_split <- prepare_status_data(cleaned_data)
  train_data <- data_split$train
  test_data  <- data_split$test
  
  train_data <- train_data %>% select(-any_of(c("Country", "Country_std")))
  test_data  <- test_data %>% select(-any_of(c("Country", "Country_std")))
  
  dummy_encoder <- dummyVars(Status ~ ., data = train_data, fullRank = TRUE)
  X <- predict(dummy_encoder, newdata = train_data) %>% as.matrix()
  mode(X) <- "numeric"
  Y <- as.factor(train_data$Status)
  
  detect_separation(x = X, y = Y, family = binomial())
  
  # LASSO
  cv_fit <- cv.glmnet(X, Y, family = "binomial", alpha = 1)
  best_model <- glmnet(X, Y, family = "binomial", alpha = 1, lambda = cv_fit$lambda.min)
  
  x_test <- predict(dummy_encoder, newdata = test_data) %>% as.data.frame()
  missing_cols <- setdiff(colnames(X), colnames(x_test))
  for (col in missing_cols) {
    x_test[[col]] <- 0
  }
  x_test <- x_test[, colnames(X)] %>% as.matrix()
  mode(x_test) <- "numeric"
  y_test <- factor(ifelse(test_data$Status == 1, "Class1", "Class0"))
  
  # Probabilities prediction
  prob_pred <- predict(best_model, newx = x_test, type = "response")
  
  probs <- data.frame(
    Class0 = 1 - prob_pred[, 1],
    Class1 = prob_pred[, 1],
    obs_num = ifelse(prob_pred[, 1] >= 0.5, 1, 0)
  )
  
  # ROC
  roc_obj <- roc(probs$obs_num, probs$Class1)
  plot(roc_obj)
  
  # Confusion Matrix
  class_pred <- factor(ifelse(prob_pred >= 0.5, "Class1", "Class0"))
  cm <- confusionMatrix(class_pred, y_test)
  
  # Important variables
  coef_lasso <- coef(best_model)
  lasso_df <- data.frame(
    Feature = rownames(coef_lasso),
    Coefficient = as.numeric(coef_lasso)
  ) %>% filter(Coefficient != 0 & Feature != "(Intercept)")
  
  return(list(
    model = best_model,
    confusion = cm,
    important_vars = lasso_df,
    roc_obj = roc_obj
  ))
}

