library(Rtsne)

if (!exists("plot_tsne_graphs")) plot_tsne_graphs <- FALSE

plot_cleaning_graphs <- FALSE
data_cleaning_result <- source("Modules/Data_Cleaning.R", local = new.env())$value
cleaned_data <- data_cleaning_result$cleaned_data

set.seed(42)
tsne_result <- Rtsne(cleaned_data[,-1], dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)


# Return useful results
result <- list(
  tsne_result = tsne_result,
)

return(result)