# =============================
# K-Means Clustering function
# =============================

run_kmeans_clustering <- function() {
  # --- Libraries ---
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(countrycode)
  
  # --- PCA, t-SNE, Utils ---
  pca_analysis_result <- source("Modules/Analysis_PCA.R", local=new.env())$value
  tsne_analysis_result <- source("Modules/Analysis_tSNE.R", local(new.env()))$value
  source("Modules/Utils.R")
  
  pca_result <- pca_analysis_result$pca_result
  pca_loadings <- pca_analysis_result$loadings
  tsne_result <- tsne_analysis_result$tsne_result
  
  # --- Cleaned data ---
  data_cleaning_result <- source("Modules/Data_Cleaning.R", local = new.env())$value
  cleaned_data <- data_cleaning_result$cleaned_data
  
  # --- wss: Elbow Method ---
  wss <- numeric(10)
  for (k in 1:10) {
    wss[k] <- sum(kmeans(cleaned_data[,-1], centers = k, nstart = 25)$withinss)
  }

  # --- K-Means on PCA ---
  set.seed(123)
  k_opt <- 4
  km_model <- kmeans(cleaned_data[,-1], centers = k_opt, nstart = 25)
  cleaned_data$kcluster_pca <- as.factor(km_model$cluster)
  
  # --- K-Means on t-SNE ---
  set.seed(123)
  k_tsne <- kmeans(tsne_result$Y, centers = k_opt)
  cleaned_data$kcluster_tsne <- as.factor(k_tsne$cluster)
  
  # --- PCA data (for visualization) ---
  pca_data <- data.frame(
    Country = cleaned_data$Country,
    kcluster_pca = cleaned_data$kcluster_pca,
    X = pca_result$x[, 1],
    Y = pca_result$x[, 2],
    PC3 = pca_result$x[, 3]
  )
  
  # --- tSNE data (for visualization) ---
  tsne_data <- data.frame(
    Country = cleaned_data$Country,
    kcluster_tsne = cleaned_data$kcluster_tsne,
    X = tsne_result$Y[, 1],
    Y = tsne_result$Y[, 2]
  )
  
  # --- Standardized Country names ---
  pca_data <- standardize_country_names(pca_data)
  tsne_data <- standardize_country_names(tsne_data)
  cleaned_data$Country_std <- pca_data$Country_std
  
  # --- Output ---
  result <- list(
    cleaned_data = cleaned_data,
    pca_data = pca_data,
    tsne_data = tsne_data,
    pca_result = pca_result,
    pca_loadings = pca_loadings,
    tsne_result = tsne_result,
    elbow_wss = wss
  )
  
  return(result)
}
