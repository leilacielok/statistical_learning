# ===============
# CLEANING THE DATASET
# ===============
plot_cleaning_graphs <- FALSE
data_cleaning_result <- source("Modules/Data_Cleaning.R", local = new.env())$value
cleaned_data <- data_cleaning_result$cleaned_data

--------------------------------------------------------------------------------
  # UNSUPERVISED LEARNING
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


