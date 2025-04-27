run_hierarchical_clustering <- function(cleaned_data) {
  library(cluster)
  library(factoextra)
  library(FactoMineR)
  library(dplyr)
  library(countrycode)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  
  source("Modules/Utils.R")
  source("Modules/Analysis_PCA.R")
  
  # PCA (for HCPC)
  pca_analysis_result <- run_pca_analysis(cleaned_data[,-ncol(cleaned_data)])
  pca_result <- pca_analysis_result$pca_result
  df_pca <- data.frame(pca_result$x[, 1:5])
  res_hcpc <- HCPC(df_pca, nb.clust = -1, graph = FALSE)
  
  # Clustering
  hc_data <- cleaned_data[, -c(1, ncol(cleaned_data))]
  dist_matrix <- dist(hc_data)
  
  # Linkages
  hc_avg <- hclust(dist_matrix, method = "average")
  hc_com <- hclust(dist_matrix, method = "complete")
  hc_ward <- hclust(dist_matrix, method = "ward.D2")
  
  # Labels to the dendograms
  hc_avg$labels <- cleaned_data$Country_std
  hc_com$labels <- cleaned_data$Country_std
  hc_ward$labels <- cleaned_data$Country_std
  
  # Cut in k cluster
  k <- 3
  clusters_avg <- cutree(hc_avg, k)
  clusters_com <- cutree(hc_com, k)
  clusters_ward <- cutree(hc_ward, k)
  
  
  cleaned_data <- cleaned_data %>%
    mutate(
      cluster_avg = as.factor(clusters_avg),
      cluster_com = as.factor(clusters_com),
      cluster_ward = as.factor(clusters_ward)
    )
  
  # World map 
  world <- ne_countries(scale = "medium", returnclass = "sf")
  map_data <- cleaned_data %>%
    select(Country_std, cluster_avg, cluster_com, cluster_ward)
  
  world_map <- left_join(world, map_data, by = c("name" = "Country_std"))
  
  hc_avg_map <- ggplot(data = world_map) +
    geom_sf(aes(fill = cluster_avg), color = "white", size = 0.1) +
    scale_fill_brewer(palette = "Set2", name = "Cluster") +
    theme_minimal() +
    labs(title = "World Map - Hierarchical Clustering (Average)")
  
  hc_com_map <- ggplot(data = world_map) +
    geom_sf(aes(fill = cluster_com), color = "white", size = 0.1) +
    scale_fill_brewer(palette = "Set2", name = "Cluster") +
    theme_minimal() +
    labs(title = "World Map - Hierarchical Clustering (Complete)")
  
  hc_ward_map <- ggplot(data = world_map) +
    geom_sf(aes(fill = cluster_ward), color = "white", size = 0.1) +
    scale_fill_brewer(palette = "Set2", name = "Cluster") +
    theme_minimal() +
    labs(title = "World Map - Hierarchical Clustering (Ward.D2)")
  
  world_map <- list(hc_avg_map, hc_com_map, hc_ward_map)
  
  # Compare linkage methods
  compare_avg_com <- table(clusters_avg, clusters_com)
  compare_avg_ward <- table(clusters_avg, clusters_ward)
  compare_com_ward <- table(clusters_com, clusters_ward)
  
  # Dendograms
  dendro_avg <- fviz_dend(hc_avg, k = k, rect = TRUE, main = "Dendrogram Average", cex = 0.4, horiz = FALSE)
  dendro_com <- fviz_dend(hc_com, k = k, rect = TRUE, main = "Dendrogram Complete", cex = 0.4, horiz = FALSE)
  dendro_ward <- fviz_dend(hc_ward, k = k, rect = TRUE, main = "Dendrogram Ward.D2", cex = 0.4, horiz = FALSE)
  
  result <- list(
    hc_data = cleaned_data,
    dendrograms = list(avg = dendro_avg, complete = dendro_com, ward = dendro_ward),
    tables = list(avg_vs_com = compare_avg_com,
                  avg_vs_ward = compare_avg_ward,
                  com_vs_ward = compare_com_ward),
    hcpc = res_hcpc,
    world_map = world_map
  )
  
  return(result)
}
