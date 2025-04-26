--------------------------------------------------------------------------------
  # CLEANING THE DATASET
--------------------------------------------------------------------------------
data_cleaning_result <- source("Modules/Data_Cleaning.R", local = new.env())$value
cleaned_data <- data_cleaning_result$cleaned_data

--------------------------------------------------------------------------------
  # UNSUPERVISED LEARNING: STATUS VARIABLE
--------------------------------------------------------------------------------
# ==========
# PCA
# ==========
plot_pca_graphs <- FALSE
pca_analysis_result <- source("Modules/Analysis_PCA.R")$value
pca_loadings <- pca_analysis_result$loadings

# ==========
# t-SNE
# ==========
plot_tsne_graphs <- FALSE
tsne_analysis_result <- source("Modules/Analysis_tSNE.R")$value
tsne_result <- tsne_analysis_result$tsne_result

# =============
# K-Means Clustering
# =============
source("Modules/Clustering_KMeans.R")
clustering_result <- run_kmeans_clustering()

cleaned_data <- clustering_result$cleaned_data
pca_data <- clustering_result$pca_data
tsne_data <- clustering_result$tsne_data

# =============
# Hierarchical Clustering
# =============
source("Modules/Clustering_Hierarchical.R")
hierarchical_result <- run_hierarchical_clustering()

cleaned_data <- hierarchical_result$hc_data
hcpc_result <- hierarchical_result$hcpc

# Static map
print(hierarchical_result$world_map)
#  HCPC dendogram
fviz_dend(hcpc_result, rect = TRUE)

# HCPC clusters 
fviz_cluster(hcpc_result, repel = TRUE)

--------------------------------------------------------------------------------
  # SUPERVISED LEARNING: STATUS
--------------------------------------------------------------------------------
# ===============
# Logistic Regression
# ===============
source("Modules/Status_classification/Logistic_status.R")
log_status <- run_status_logistic(cleaned_data)

# Cross-Validation
source("Modules/Status_classification/CrossValidation_Status.R")
logit_status_cv <- cross_validate_model(
  data = cleaned_data,
  method = "glmnet",
  tune_grid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, length = 10))
)

# ===============
# Decision Tree
# ===============
source("Modules/Status_classification/DT_status.R")
tree_status <- run_status_tree(cleaned_data)
rpart.plot(tree_status$model$finalModel)

# Cross-Validation
tree_status_cv <- cross_validate_model(
  data = cleaned_data,
  method = "rpart"
)

# ===============
# Random Forest
# ===============
source("Modules/Status_classification/RF_status.R")
rf_status <- run_status_rf(cleaned_data)

# Cross-Validation
rf_status_cv <- cross_validate_model(
  data = cleaned_data,
  method = "rf"
)

# ===============
# ROC models comparison
# ===============
source("Modules/Status_classification/CrossValidation_Status.R")
models_list_status <- list(logit_status_cv, tree_status_cv, rf_status_cv)
model_names_status <- c("Logistic Regression", "Decision Tree", "Random Forest")

compare_models_roc(models_list_status, model_names_status)

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

# Regression
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
plot_ridge_predictions(ridge_lifeexp, cleaned_data)

# ===============
# SVM Regression
# ===============
source("Modules/LifeExp_regression/SVMReg_lifeexp.R")
svm_lifeexp <- run_lifeexp_svm(cleaned_data)

# ===============
# ROC models comparison
# ===============
models_list_lifeexp <- list(logit_lifeexp_cv, tree_lifeexp_cv, rf_lifeexp_cv)
model_names_lifeexp <- c("Multinomial Logistic", "Decision Tree", "Random Forest")

compare_models_multiclass(models_list_lifeexp, model_names_lifeexp)