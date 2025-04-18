library(ggplot2)
library(ggrepel)
library(plotly)

if (!exists("plot_pca_graphs")) plot_pca_graphs <- FALSE

# 1. PCA
pca_result <- prcomp(cleaned_data[,-1], scale. = TRUE)

# 2. Loadings
loadings <- as.data.frame(pca_result$rotation[, 1:3])
loadings$Variable <- rownames(loadings)

# Return useful results
result <- list(
  pca_result = pca_result,
  loadings = loadings
)

return(result)
