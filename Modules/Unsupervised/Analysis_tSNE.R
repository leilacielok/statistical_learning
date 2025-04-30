run_tsne_analysis <- function(cleaned_data, labels = NULL) {
  library(Rtsne)
  library(ggplot2)
  library(ggrepel)
  
  set.seed(42)
  tsne_result <- Rtsne(
    cleaned_data[, -c(1, ncol(cleaned_data))],
    dims = 2,
    perplexity = 30,
    verbose = TRUE,
    max_iter = 500
  )
  
  # DataFrame t-SNE
  tsne_df <- as.data.frame(tsne_result$Y)
  colnames(tsne_df) <- c("Dim1", "Dim2")
  
  # Aggiungi colonne da usare per etichette e colori
  tsne_df$Country <- cleaned_data$Country
  
  if (!is.null(labels)) {
    tsne_df$Label <- labels
    projection <- ggplot(tsne_df, aes(x = Dim1, y = Dim2, color = Label)) +
      geom_point(alpha = 0.7) +
      geom_text_repel(aes(label = Country), size = 3, max.overlaps = 20) +
      labs(title = "t-SNE Projection", x = "Dimension 1", y = "Dimension 2") +
      theme_minimal() +
      theme(legend.title = element_blank())
  } else {
    projection <- ggplot(tsne_df, aes(x = Dim1, y = Dim2)) +
      geom_point(alpha = 0.7, color = "steelblue") +
      geom_text_repel(aes(label = Country), size = 3, max.overlaps = 20) +
      labs(title = "t-SNE Projectioncon", x = "Dimension 1", y = "Dimension 2") +
      theme_minimal()
  }
  
  # Plot del costo
  cost <- NULL
  if (!is.null(tsne_result$costs)) {
    cost_df <- data.frame(
      Iteration = seq_along(tsne_result$costs),
      Cost = tsne_result$costs
    )
    cost <- ggplot(cost_df, aes(x = Iteration, y = Cost)) +
      geom_line(color = "darkred") +
      labs(title = "t-SNE Cost over time", x = "Iteration", y = "Cost") +
      theme_minimal()
  }
  
  return(list(
    tsne_result = tsne_result,
    plot = list(
      projection = projection,
      cost = cost
    )
  ))
}
