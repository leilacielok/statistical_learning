# =======================
# Useful functions
# =======================

library(ggplot2)
library(dplyr)
library(tidyr)
library(countrycode)
library(caret)
library(ggrepel)

# -----------------------
# 1. Country names standardization
# -----------------------
standardize_country_names <- function(df, country_col = "Country") {
  df[[country_col]] <- recode(df[[country_col]],
                           "CentralAfricanRepublic" = "Central African Republic",
                           "DominicanRepublic" = "Dominican Republic",
                           "LaoPeople'sDemocraticRepublic" = "Lao PDR",
                           "SaintLucia" = "Saint Lucia",
                           "SouthAfrica" = "South Africa",
                           "UnitedStatesofAmerica" = "United States of America"
  )
                           
  df$Country_std <- countrycode(df[[country_col]],
                                origin = "country.name",
                                destination = "country.name")
  
  df$Country_std <- ifelse(is.na(df$Country_std), df[[country_col]], df$Country_std)
  
  return(df)
}

# -----------------------
# 2. Confusion Matrix Heatmap
# -----------------------
plot_confusion_heatmap <- function(true_labels, predicted_labels, title = "") {
  cm_table <- table(Predicted = predicted_labels, Actual = true_labels)
  cm_df <- as.data.frame(cm_table)
  
  ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 5) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = paste("Confusion Matrix -", title), x = "Actual", y = "Predicted") +
    theme_minimal()
}

# -----------------------
# 3. Variable Importance Plot
# -----------------------
plot_variable_importance <- function(model, title = "Variable Importance") {
  varImpPlot(model, main = title)
}

# -----------------------
# 4. World Map by Clusters
# -----------------------
plot_cluster_world_map <- function(world, data, cluster_col, title, palette = "Set3") {
  map_df <- data %>% select(Country_std, !!sym(cluster_col))
  world_data <- left_join(world, map_df, by = c("name" = "Country_std"))
  
  ggplot(data = world_data) +
    geom_sf(aes(fill = .data[[cluster_col]]), color = "white", size = 0.1) +
    scale_fill_brewer(palette = palette, name = cluster_col) +
    theme_minimal() +
    labs(title = title)
}

# -----------------------
# 5. Model Evaluation Summary
# -----------------------
evaluate_model <- function(true_labels, predicted_labels, model_name = "") {
  cat("\n=== Evaluation:", model_name, "===\n")
  cm <- confusionMatrix(predicted_labels, true_labels)
  print(cm)
}

# ----------------------
# 6. Data preparation for supervised learning: variable 'Status'
# ----------------------
prepare_status_data <- function(cleaned_data, seed = 123) {
  cleaned_data <- cleaned_data[, !(names(cleaned_data) %in% c("kcluster_pca", "kcluster_tsne", "cluster_avg", "cluster_com", "cluster_ward"))]
  
  # Split dataset
  set.seed(seed)
  split_index <- createDataPartition(cleaned_data[,-1]$Status, p = 0.7, list = FALSE)
  
  train_data <- cleaned_data[split_index, -1] 
  test_data  <- cleaned_data[-split_index, -1]
  
  return(list(train = train_data, test = test_data))
}

# ----------------------
# 7.Data preparation for supervised learning: variable 'life expectancy', factorized
# ----------------------
prepare_lifeexp_data <- function(cleaned_data, seed = 123) {
  quantiles <- quantile(cleaned_data$life_expectancy, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  
  cleaned_data$lifeexp_cat <- cut(
    cleaned_data$life_expectancy,
    breaks = quantiles,
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE
  )
  
  cleaned_data$lifeexp_cat <- factor(cleaned_data$lifeexp_cat, levels = c("Low", "Medium", "High"))
  cleaned_data <- cleaned_data[, !(names(cleaned_data) %in% c("life_expectancy", "Status", "kcluster_pca", "kcluster_tsne", "Country"))]
  
  # 3. Train/test split
  set.seed(seed)
  split_index <- caret::createDataPartition(cleaned_data$lifeexp_cat, p = 0.7, list = FALSE)
  
  train_data <- cleaned_data[split_index, ]
  test_data  <- cleaned_data[-split_index, ]
  
  return(list(train = train_data, test = test_data))
}

# ---------------------
# 8. Visualization Maps Kmeans
# ---------------------
generate_kmeans_map <- function(cleaned_data, method = "pca") {
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  if (method == "pca") {
    map_data <- cleaned_data %>%
      select(Country_std, kcluster_pca)
    fill_var <- "kcluster_pca"
    title_map <- "World Map - KMeans Clustering (PCA)"
  } else if (method == "tsne") {
    map_data <- cleaned_data %>%
      select(Country_std, kcluster_tsne)
    fill_var <- "kcluster_tsne"
    title_map <- "World Map - KMeans Clustering (t-SNE)"
  } else {
    stop("Method not valid: use 'pca' or 'tsne'")
  }
  
  # --- Join ---
  world_map <- left_join(world, map_data, by = c("name" = "Country_std"))
  
  # --- Plot ---
  map_plot <- ggplot(data = world_map) +
    geom_sf(aes_string(fill = fill_var), color = "white", size = 0.1) +
    scale_fill_brewer(palette = "Set2", name = "Cluster") +
    theme_minimal() +
    labs(title = title_map)
  
  return(map_plot)
}

