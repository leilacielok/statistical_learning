--------------------------------------------------------------------------------
  # CLEANING THE DATASET
--------------------------------------------------------------------------------

plot_cleaning_graphs <- FALSE
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
  # SUPERVISED LEARNING
--------------------------------------------------------------------------------
# ===============
# Logistic Regression
# ===============
source("Modules/Status_classification/Logistic_status.R")
log_result <- run_status_logistic(cleaned_data)
log_result$confusion
plot(log_result$roc)

# ===============
# Decision Tree
# ===============
source("Modules/Status_classification/DT_status.R")
tree_result <- run_status_tree(cleaned_data)
rpart.plot(tree_result$model)

# ===============
# Random Forest
# ===============
source("Modules/Status_classification/RF_status.R")
rf_result <- run_status_rf(cleaned_data)
rf_result$importance_plot()

--------------------------------------------------------------------------------
  # UNSUPERVISED LEARNING: LIFE EXPECTANCY VARIABLE
--------------------------------------------------------------------------------
# ===============
# Logistic
# ===============
source("Modules/LifeExp_classification/Logistic_lifeexp.R")
lifeexp_log <- run_lifeexp_logistic(cleaned_data)
lifeexp_log$confusion

# ===============
# Decision Tree
# ===============
source("Modules/LifeExp_classification/DC_lifeexp.R")
lifeexp_tree <- run_lifeexp_tree(cleaned_data)
rpart.plot(lifeexp_tree$model)
lifeexp_tree$confusion

# ===============
# Random Forest
# ===============
source("Modules/LifeExp_classification/RF_lifeexp.R")
lifeexp_rf <- run_lifeexp_rf(cleaned_data)
lifeexp_rf$confusion
varImpPlot(lifeexp_rf$model)
