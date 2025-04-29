evaluate_clustering <- function(data, cluster_labels) {
  library(cluster)
  library(factoextra)
  library(clusterCrit)
  
  cluster_labels <- as.integer(as.character(cluster_labels))
  
  # Silhouette Score
  silhouette_vals <- silhouette(cluster_labels, dist(data))
  avg_silhouette <- mean(silhouette_vals[, 3])
  
  # Davies-Bouldin Index
  db_index <- intCriteria(as.matrix(data), as.integer(cluster_labels), c("Davies_Bouldin"))
  
  return(list(
    avg_silhouette = avg_silhouette,
    db_index = db_index$davies_bouldin
  ))
}


plot_clustering_validation <- function(results_list, method_names) {
  library(ggplot2)
  
  if (length(results_list) != length(method_names)) {
    stop("results_list and method_names must have the same length")
  }
  
  df <- data.frame(
    Method = method_names,
    Silhouette = sapply(results_list, function(x) x$avg_silhouette),
    DB_Index = sapply(results_list, function(x) x$db_index)
  )
  
  # Silhouette Score plot
  silhouette_plot <- ggplot(df, aes(x = reorder(Method, -Silhouette), y = Silhouette, fill = Method)) +
    geom_bar(stat = "identity", width = 0.6) +
    ggtitle("Silhouette Score by clustering method") +
    ylab("Silhouette") +
    xlab("Method") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = "none")
  
  # Davies-Bouldin Index plot
  db_plot <- ggplot(df, aes(x = reorder(Method, DB_Index), y = DB_Index, fill = Method)) +
    geom_bar(stat = "identity", width = 0.6) +
    ggtitle("Davies-Bouldin Index by clustering method") +
    ylab("DB Index") +
    xlab("Method") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = "none")
  
  return(list(
    silhouette_plot = silhouette_plot,
    db_plot = db_plot,
    data = df
  ))
}
