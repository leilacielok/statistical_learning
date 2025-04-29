run_tsne_analysis <- function(cleaned_data, plot_tsne_graphs = FALSE) {
  library(Rtsne)

  set.seed(42)
  tsne_result <- Rtsne(cleaned_data[,-c(1, ncol(cleaned_data))], dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)

  result <- list(
    tsne_result = tsne_result)
  
  return(result)
}