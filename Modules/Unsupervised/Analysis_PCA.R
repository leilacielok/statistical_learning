run_pca_analysis <- function(cleaned_data, plot_pca_graphs = FALSE) {
  library(ggplot2)
  library(ggrepel)
  library(plotly)

  pca_result <- prcomp(cleaned_data[,-c(1, ncol(cleaned_data))], scale. = TRUE)

  loadings <- as.data.frame(pca_result$rotation[, 1:3])
  loadings$Variable <- rownames(loadings)
  
  pca_var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  var_df <- data.frame(
    PC = paste0("PC", 1:length(pca_var_explained)),
    VarianceExplained = pca_var_explained
  )
  var_df <- var_df[order(-var_df$VarianceExplained), ]
  var_df$PC <- factor(var_df$PC, levels = var_df$PC)
  
  var_plot <- ggplot(var_df[1:10, ], aes(x = PC, y = VarianceExplained)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = scales::percent(VarianceExplained, accuracy = 0.1)),
              vjust = -0.5, size = 3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = "Variance Explained by Principal Components",
         x = "Principal Components",
         y = "Explained Variance") +
    theme_minimal()
  
  
  result <- list(
    pca_result = pca_result,
    pca_plot = var_plot,
    loadings = loadings
  )

  return(result)
}