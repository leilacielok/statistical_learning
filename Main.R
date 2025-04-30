--------------------------------------------------------------------------------
  # CLEANING THE DATASET
--------------------------------------------------------------------------------
data_cleaning_result <- source("Modules/Data_Cleaning.R", local = new.env())$value
cleaned_data <- data_cleaning_result$cleaned_data
cleaned_data_original <- cleaned_data
data_numeric <- cleaned_data_original[, sapply(cleaned_data_original, is.numeric)]

--------------------------------------------------------------------------------
  # UNSUPERVISED LEARNING: STATUS VARIABLE
--------------------------------------------------------------------------------
# ==========
# PCA
# ==========
source("Modules/Unsupervised/Analysis_PCA.R")
pca_analysis_result <- run_pca_analysis(cleaned_data_original)
pca_loadings <- pca_analysis_result$loadings

# ==========
# t-SNE
# ==========
source("Modules/Unsupervised/Analysis_tSNE.R")
tsne_analysis_result <- run_tsne_analysis(cleaned_data_original)
tsne_result <- tsne_analysis_result$tsne_result

# =============
# K-Means Clustering
# =============
source("Modules/Unsupervised/Clustering_KMeans.R")
source("Modules/Utils.R")
kmeans_result <- run_kmeans_clustering(cleaned_data_original)

pca_map <- generate_kmeans_map(kmeans_result$kmeans_data, method = "pca")
tsne_map <- generate_kmeans_map(kmeans_result$kmeans_data, method = "tsne")

# Validation
source("Modules/Unsupervised/Clustering_Validation.R")
eval_pca <- evaluate_clustering(data_numeric, kmeans_result$kmeans_data$kcluster_pca)
eval_tsne<- evaluate_clustering(data_numeric, kmeans_result$kmeans_data$kcluster_tsne)

# =============
# Hierarchical Clustering
# =============
source("Modules/Unsupervised/Clustering_Hierarchical.R")
hierarchical_result <- run_hierarchical_clustering(cleaned_data_original)

# Validation
source("Modules/Unsupervised/Clustering_Validation.R")
eval_ward <- evaluate_clustering(data_numeric, as.integer(as.character(hierarchical_result$hc_data$cluster_ward)))
eval_avg  <- evaluate_clustering(data_numeric, as.integer(as.character(hierarchical_result$hc_data$cluster_avg)))
eval_com  <- evaluate_clustering(data_numeric, as.integer(as.character(hierarchical_result$hc_data$cluster_com)))

# =============
# Clustering Evaluation
# =============

source("Modules/Unsupervised/Clustering_validation.R")

results_list <- list(
  eval_pca, 
  eval_tsne, 
  eval_avg, 
  eval_com, 
  eval_ward
)

method_names <- c(
  "KMeans + PCA", 
  "KMeans + t-SNE", 
  "HC Average", 
  "HC Complete", 
  "HC Ward"
)

validation_plots <- plot_clustering_validation(results_list, method_names)

--------------------------------------------------------------------------------
  # SUPERVISED LEARNING: STATUS
--------------------------------------------------------------------------------
# ===============
# Logistic Regression
# ===============
source("Modules/Status_classification/Logistic_status.R")
log_status <- run_status_logistic(cleaned_data_original)
log_status$confusion
log_status$model
log_status$important_vars
log_status$roc_obj

# Cross-Validation
source("Modules/Status_classification/CrossValidation_Status.R")
logit_status_cv <- cross_validate_model(
  data = cleaned_data_original,
  method = "glmnet",
  tune_grid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, length = 10))
)

# ===============
# Decision Tree
# ===============
source("Modules/Status_classification/DT_status.R")
tree_status <- run_status_tree(cleaned_data_original)
rpart.plot(tree_status$model$finalModel)


# Cross-Validation
tree_status_cv <- cross_validate_model(
  data = cleaned_data_original,
  method = "rpart"
)

# ===============
# Random Forest
# ===============
source("Modules/Status_classification/RF_status.R")
rf_status <- run_status_rf(cleaned_data_original)

# Cross-Validation
rf_status_cv <- cross_validate_model(
  data = cleaned_data_original,
  method = "rf"
)

# ===============
# ROC models comparison
# ===============
source("Modules/Status_classification/CrossValidation_Status.R")
models_list_status <- list(logit_status_cv, tree_status_cv, rf_status_cv)
model_names_status <- c("Logistic Regression", "Decision Tree", "Random Forest")

compare_models_roc(models_list_status, model_names_status)
compare_models_vars(list(rf_status, tree_status, log_status), c("Random Forest", "Decision Tree", "Logistic Regression"))

--------------------------------------------------------------------------------
  # SUPERVISED LEARNING: LIFE EXPECTANCY 
--------------------------------------------------------------------------------
# ===============
# Logistic
# ===============
source("Modules/LifeExp_classification/Logistic_lifeexp.R")
log_lifeexp <- run_lifeexp_logistic(cleaned_data)

# Cross-Validation
source("Modules/LifeExp_classification/CrossValidation_Lifeexp.R")
logit_lifeexp_cv <- cross_validate_model(
  data = cleaned_data,
  method = "multinom"
)

# ===============
# Decision Tree
# ===============
source("Modules/LifeExp_classification/DT_lifeexp.R")
tree_lifeexp <- run_lifeexp_tree(cleaned_data)
rpart.plot(tree_lifeexp$model$finalModel)

tree_lifeexp_cv <- cross_validate_model(
  data = cleaned_data,
  method = "rpart"
)

# ===============
# Random Forest
# ===============
# Classification
source("Modules/LifeExp_classification/RF_lifeexp.R")
rf_lifeexp <- run_lifeexp_rf(cleaned_data)

rf_lifeexp_cv <- cross_validate_model(
  data = cleaned_data,
  method = "rf"
)

# RF Regression
source("Modules/LifeExp_regression/RFReg_lifeexp.R")
rfreg_lifeexp <- run_lifeexp_rfreg(cleaned_data)

# ===============
# Linear Regression
# ===============
source("Modules/LifeExp_regression/LinReg_lifeexp.R")
linreg_lifeexp <- run_lifeexp_linreg(cleaned_data)

# ===============
# Ridge Regression
# ===============
source("Modules/LifeExp_regression/RidgeReg_lifeexp.R")
ridge_lifeexp <- run_lifeexp_ridge(cleaned_data)
plot_ridge_predictions(ridge_lifeexp$ridge_model, cleaned_data)

# ===============
# SVM Regression
# ===============
source("Modules/LifeExp_regression/SVMReg_lifeexp.R")
svm_lifeexp <- run_lifeexp_svm(cleaned_data)
svm_lifeexp$imp_plot

# ===============
# ROC: classification models comparison
# ===============
models_list_lifeexp <- list(logit_lifeexp_cv, tree_lifeexp_cv, rf_lifeexp_cv)
model_names_lifeexp <- c("Multinomial Logistic", "Decision Tree", "Random Forest")

compare_models_multiclass(models_list_lifeexp, model_names_lifeexp)

# ===============
# RMSE and R2: regression models comparison
# ===============
linreg_lifeexp <- run_lifeexp_linreg(cleaned_data)
ridge_lifeexp <- run_lifeexp_ridge(cleaned_data)
svm_lifeexp <- run_lifeexp_svm(cleaned_data)
rfreg_lifeexp <- run_lifeexp_rfreg(cleaned_data)

source("Modules/LifeExp_regression/Regressions_Comparison.R")
results_comparison <- compare_regression_models(
  linreg_lifeexp, 
  ridge_lifeexp, 
  svm_lifeexp, 
  rfreg_lifeexp
)
