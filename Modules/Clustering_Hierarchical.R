run_hierarchical_clustering <- function() {
  # Libraries
  library(cluster)
  library(factoextra)
  library(FactoMineR)
  library(dplyr)
  library(countrycode)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  
  # --- Cleaned data ---
  data_cleaning_result <- source("Modules/Data_Cleaning.R", local = new.env())$value
  cleaned_data <- data_cleaning_result$cleaned_data
  hc_data <- cleaned_data[ , -1]
  
  # --- Import utils ---
  source("Modules/Utils.R")
  
  # --- Distances ---
  dist_matrix <- dist(hc_data)
  
  # --- Hierarchical Clustering ---
  hc_avg <- hclust(dist_matrix, method = "average")
  hc_com <- hclust(dist_matrix, method = "complete")
  hc_ward <- hclust(dist_matrix, method = "ward.D2")
  
  # --- Cut in 4 cluster ---
  k <- 3
  clusters_avg <- cutree(hc_avg, k)
  clusters_com <- cutree(hc_com, k)
  clusters_ward <- cutree(hc_ward, k)
  
  # --- Add clusters ---
  cleaned_data <- cleaned_data %>%
    mutate(
      cluster_avg = as.factor(clusters_avg),
      cluster_com = as.factor(clusters_com),
      cluster_ward = as.factor(clusters_ward)
    )
  
  # --- Standardize Country names ---
  cleaned_data <- standardize_country_names(cleaned_data)
  
  # --- World map ---
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
  
  # --- Compare linkage methods ---
  compare_avg_com <- table(clusters_avg, clusters_com)
  compare_avg_ward <- table(clusters_avg, clusters_ward)
  compare_com_ward <- table(clusters_com, clusters_ward)
  
  # --- Dendograms Visualization ---
  par(mfrow = c(1, 3))
  plot(hc_avg, main = "Average Linkage")
  rect.hclust(hc_avg, k = k, border = "blue")
  plot(hc_com, main = "Complete Linkage")
  rect.hclust(hc_com, k = k, border = "green")
  plot(hc_ward, main = "Ward.D2 Linkage")
  rect.hclust(hc_ward, k = k, border = "red")
  par(mfrow = c(1, 1))  # Reset layout
  
  # --- HCPC (Hierarchical Clustering on Principal Components) ---
  pca_result <- source("Modules/Analysis_PCA.R")$value$pca_result
  df_pca <- data.frame(pca_result$x[, 1:5])  
  res_hcpc <- HCPC(df_pca, nb.clust = -1, graph = FALSE)
  
  # --- Output ---
  result <- list(
    hc_data = cleaned_data,
    dendrograms = list(avg = hc_avg, complete = hc_com, ward = hc_ward),
    tables = list(avg_vs_com = compare_avg_com,
                  avg_vs_ward = compare_avg_ward,
                  com_vs_ward = compare_com_ward),
    hcpc = res_hcpc,
    world_map = list(hc_avg_map,
                     hc_com_map,
                     hc_ward_map)
  )
  
  return(result)
}
