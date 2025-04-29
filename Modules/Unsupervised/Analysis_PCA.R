run_pca_analysis <- function(cleaned_data, plot_pca_graphs = FALSE) {
  library(ggplot2)
  library(ggrepel)
  library(plotly)

  pca_result <- prcomp(cleaned_data[,-c(1, ncol(cleaned_data))], scale. = TRUE)

  loadings <- as.data.frame(pca_result$rotation[, 1:3])
  loadings$Variable <- rownames(loadings)
  
  result <- list(
    pca_result = pca_result,
    loadings = loadings
  )

  return(result)
}