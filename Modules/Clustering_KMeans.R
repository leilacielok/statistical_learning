run_kmeans_clustering <- function(cleaned_data) {
  # --- Libraries ---
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(countrycode)
  library(plotly)
  
  # --- PCA, t-SNE, Utils ---
  source("Modules/Analysis_PCA.R")
  source("Modules/Analysis_tSNE.R")
  source("Modules/Utils.R")
  
  pca_analysis_result <- run_pca_analysis(cleaned_data)
  tsne_analysis_result <- run_tsne_analysis(cleaned_data)
  
  pca_result <- pca_analysis_result$pca_result
  pca_loadings <- pca_analysis_result$loadings
  tsne_result <- tsne_analysis_result$tsne_result
  
  prop_var_explained <- (pca_result$sdev)^2 / sum((pca_result$sdev)^2)
  
  # --- wss: Elbow Method ---
  wss <- numeric(10)
  for (k in 1:10) {
    wss[k] <- sum(kmeans(cleaned_data[,-c(1, ncol(cleaned_data))], centers = k, nstart = 25)$withinss)
  }

  # --- K-Means on PCA ---
  set.seed(123)
  k_opt <- 4
  km_model <- kmeans(cleaned_data[,-c(1, ncol(cleaned_data))], centers = k_opt, nstart = 25)
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
  
  # Visualization on PCA: 2d
  loadings_df <- as.data.frame(pca_loadings[, 1:2])
  colnames(loadings_df) <- c("PC1", "PC2")
  loadings_df$varnames <- rownames(loadings_df)
    
  plot2dpca <- ggplot(data = pca_data, aes(x = X, y = Y, color = kcluster_pca, label = Country)) +
    geom_point(size = 3) +
    geom_text_repel(size = 3, max.overlaps = 20) +
    geom_segment(
      data = loadings_df, 
      aes(x = 0, y = 0, xend = PC1*5, yend = PC2*5),
      arrow = arrow(length = unit(0.3, "cm")), 
      color = "darkorange",
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    geom_text_repel(
      data = loadings_df, 
      aes(x = PC1*5.2, y = PC2*5.2, label = varnames),
      inherit.aes = FALSE,
      color = "darkorange", 
      size = 3,
      fontface = "bold"
    ) +
    theme_minimal() +
    labs(
      title = "KMeans Clustering on PCA (2D)", 
      x = paste0("PC1 (", round(prop_var_explained[1] * 100, 1), "%)"),
      y = paste0("PC2 (", round(prop_var_explained[2] * 100, 1), "%)"), 
      color = "Cluster") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Visualization on PCA: 3d
  plot3dpca <- plot_ly(
    pca_data, x = ~X, y = ~Y, z = ~PC3, 
    color = ~kcluster_pca, 
    colors = "Set1",
    text = ~Country, 
    type = "scatter3d", 
    mode = "markers",
    marker = list(size = 4.5)
  ) %>%
    layout(
      title = "KMeans Clustering on PCA (3D)",
      scene = list(
        xaxis = list(title = paste0("PC1 (", round(prop_var_explained[1] * 100, 1), "%)")),
        yaxis = list(title = paste0("PC2 (", round(prop_var_explained[2] * 100, 1), "%)")),
        zaxis = list(title = paste0("PC3 (", round(prop_var_explained[3] * 100, 1), "%)"))
      )
    )
  
  # Visualization on tSNE
  plottsne <- ggplot(data = tsne_data, aes(x = X, y = Y, color = kcluster_tsne, label = Country)) +
    geom_point(size = 3) +
    geom_text_repel(size = 3, max.overlaps = 20) +
    theme_minimal() +
    labs(
      title = "KMeans Clustering on t-SNE (2D)",
      x = "t-SNE 1",
      y = "t-SNE 2",
      color = "Cluster"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # --- Output ---
  result <- list(
    kmeans_data = cleaned_data,
    pca_data = pca_data,
    tsne_data = tsne_data,
    pca_result = pca_result,
    pca_loadings = pca_loadings,
    tsne_result = tsne_result,
    elbow_wss = wss,
    plot_2dpca = plot2dpca,
    plot_3dpca = plot3dpca,
    plot_tsne = plottsne
  )
  
  return(result)
}
