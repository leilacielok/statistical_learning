library(ggplot2)
library(ggrepel)
library(plotly)
library(dplyr)
library(cluster)
library(factoextra)

# ===============
# CLEANING THE DATASET
# ===============
plot_cleaning_graphs <- FALSE
data_cleaning_result <- source("Modules/Data_Cleaning.R", local = new.env())$value
cleaned_data <- data_cleaning_result$cleaned_data

--------------------------------------------------------------------------------
# UNSUPERVISED LEARNING
--------------------------------------------------------------------------------
# ==========
# PCA
# ==========
plot_pca_graphs <- FALSE
pca_analysis_result <- source("Modules/Analysis_PCA.R")$value

# ===========
# K_MEANS CLUSTERING
# ===========
wss <- numeric(10)  
for (k in 1:10) {
  wss[k] <- sum(kmeans(cleaned_data[,-1], centers = k, nstart = 25)$withinss)
}

# Plot
plot(1:10, wss, type="b", pch=19, frame=FALSE,
     xlab="Number of Clusters", ylab="Within-cluster Sum of Squares",
     main="Elbow Method for Optimal k")


set.seed(123)  
k_opt <- 4
km_model <- kmeans(cleaned_data[,-1], centers = k_opt, nstart = 25)

cleaned_data$kcluster_pca <- as.factor(km_model$cluster)
table(cleaned_data$kcluster_pca)

# Means of clusters
pca_kcluster_means <- aggregate(cleaned_data[,-c(1, ncol(cleaned_data))], by = list(kcluster_pca = km_model$cluster), mean)
print(pca_kcluster_means)

# =============
# PCA calculation
# =============
pca_data <- data.frame(Country = cleaned_data$Country, Cluster = cleaned_data$kcluster_pca, PC1 = pca_result$x[,1], PC2 = pca_result$x[,2], PC3 = pca_result$x[,3])

# Cluster plot
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = Country), size = 3) +
  theme_minimal() +
  labs(title = "K-Means Clustering Visualization", x = "Principal Component 1", y = "Principal Component 2")

# Plot with loadings
arrow_scale <- 3.5

ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(data = pca_data, aes(x = PC1, y = PC2, color = Cluster), size = 3) +
  geom_text_repel(data = pca_data, aes(x = PC1, y = PC2, label = Country), size = 3) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1 * arrow_scale, yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "#e6550d", size = 0.7, alpha = 0.9) +
  geom_text_repel(data = loadings,
            aes(x = PC1 * arrow_scale * 1.1, y = PC2 * arrow_scale * 1.1, label = Variable),
            color = "#e6550d", size = 3.5, fontface = "bold") +
  xlab(paste0("PC1 (", round(summary(pca_result)$importance[2,1] * 100, 1), "%)")) +
  ylab(paste0("PC2 (", round(summary(pca_result)$importance[2,2] * 100, 1), "%)")) +
  theme_minimal() +
  labs(title = "K-Means Clustering Visualization with PCA Loadings",
       x = "Principal Component 1", y = "Principal Component 2")
  

"The graph represents a biplot of the PCA with the results of the k-means clustering on life 
expectancy in various Word Countries.
1. Clusters distribution
  The red cluster seems to group countries with middle developing levels.
  The blue cluster includes more developed countries such as USA, Germany, Spain 
  and Australia, which tend to have a higher life expectancy rate.
  The purple cluster groups Countries with higher mortality rates and lower life
  expectancy, such as Nigeria, Pakistan, Afghanistan and other African Countries.
  The green cluster includes Countries with higher rates in deseases.
2.Variables vectors
  The blue arrows indicate the variables influencing countries distribution.
  AIDS, infant deaths, adult mortality, and deaths under five are strongly correlated 
  and point all to the area of the blue cluster, meaning that these factors contribute
  to a lower life expectancy.
  Conversely, cluster green countries are far from these variables, suggesting lower
  mortality rates and deaseses.
3. Principal Components
  PC1 explains the majority of the variance (36.7%) and it looks like it distinguishes
  countries with high mortality rates from those with a longer life expectancy.
  PC2 explains the 11.8% of the variance and could represent another factor like 
  health services or the economy."

"I want to try and plot a 3D graph to explain more variance."

# 3D graph
fig <- plot_ly(
  data = pca_data,
  x = ~PC1, y = ~PC2, z = ~PC3,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 6,
    opacity = 0.8  # Rende i punti leggermente trasparenti per evitare sovrapposizioni
  ),
  color = ~Cluster, 
  colors = c("red", "green", "blue", "purple"),
  text = ~paste("Country:", Country, "<br>Cluster:", Cluster),  # Hover con più info
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = "K-means 3D visualization",
      font = list(size = 18, fontface = "bold")
    ),
    scene = list(
      xaxis = list(
        title = paste0("PC1 (", round(summary(pca_result)$importance[2,1] * 100, 1), "%)"),
        showgrid = TRUE,
        gridcolor = "lightgray"
      ),
      yaxis = list(
        title = paste0("PC2 (", round(summary(pca_result)$importance[2,2] * 100, 1), "%)"),
        showgrid = TRUE,
        gridcolor = "lightgray"
      ),
      zaxis = list(
        title = paste0("PC3 (", round(summary(pca_result)$importance[2,3] * 100, 1), "%)"),
        showgrid = TRUE,
        gridcolor = "lightgray"
      ),
      bgcolor = "white" 
    )
  )

fig

# Create 3D PCA variable plot (loadings as arrows)
PCA_plot_3d <- plot_ly()

for (i in 1:nrow(loadings)) {
  PCA_plot_3d <- PCA_plot_3d %>%
    add_trace(
      type = "scatter3d",
      mode = "lines+text",
      x = c(0, loadings$PC1[i]),
      y = c(0, loadings$PC2[i]),
      z = c(0, loadings$PC3[i]),
      text = c("", rownames(loadings)[i]),
      textposition = "top center",
      textfont = list(color = "#e6550d", size = 12),
      line = list(color = "orange", width = 4),
      marker = list(size = 4, color = "orange"),
      showlegend = FALSE
    )
}

# Add axis labels with explained variance
explained_var <- summary(pca_result)$importance[2, 1:3]
PCA_plot_3d <- PCA_plot_3d %>%
  layout(
    title = "3D PCA Variable Biplot",
    scene = list(
      xaxis = list(title = paste0("PC1 (", round(explained_var[1]*100, 1), "%)")),
      yaxis = list(title = paste0("PC2 (", round(explained_var[2]*100, 1), "%)")),
      zaxis = list(title = paste0("PC3 (", round(explained_var[3]*100, 1), "%)"))
    )
  )

PCA_plot_3d

# Interpretation:
"
Adult Mortality, AIDS, and Infant Deaths point strongly in a similar direction 
→ they are correlated and contribute heavily to the same dimension (PC1/PC3).
GDPpc, Schooling, and Life Expectancy cluster in another direction 
→ they are likely positively correlated with each other, and explain variance 
on another axis (possibly opposite to mortality variables).
- PC1 (40%) might represent a wealth-health axis, contrasting development indicators 
(e.g., GDP, life expectancy) with mortality/disease indicators.
- PC2 (11.2%) and PC3 probably capture finer aspects, maybe separating child vs. 
adult health factors, or healthcare spending.
Along PC2, here's what we notice: Strong positive contributors:
infant_deaths, measles, deaths_under_five, pop
→ These indicators are often associated with poor pediatric health and high population pressure.
oderate to weak contributors in the opposite direction:
Not strongly opposing in your plot, but countries with lower values on these might 
have better child health systems or smaller populations.
Interpretation: PC2 could reflect a child health burden vs. demographic stability axis
- PC3 (9.4%) – Maybe an immunization vs. nutrition/age health axis
Positive side:pop, measles, infant_deaths, etc., again pushing upward (they appear influential here too).
Negative side (more distinct):thinness_5_9_years, thinness_1_19_years, diphtheria, polio, hepatitisB
→ This cluster seems to relate more to immunization coverage and nutritional deficiencies in specific age groups.
Interpretation: PC3 may distinguish nutritional health and immunization patterns 
in a population — with high negative values suggesting greater challenges in those areas.
"
--------------------------------------------------------------------------------
# =============
# TSNE
# =============
library(Rtsne)

set.seed(42)
tsne_result <- Rtsne(cleaned_data[,-1], dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)

# Create a dataframe with results
tsne_data <- data.frame(
  X = tsne_result$Y[, 1],
  Y = tsne_result$Y[, 2],
  Cluster = cleaned_data$kcluster_pca,
  Country = cleaned_data$Country
)

# Plot with country labels
ggplot(tsne_data, aes(x = X, y = Y, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text_repel(aes(label = Country), size = 3, max.overlaps = 20) +
  theme_minimal() +
  labs(
    title = "t-SNE Visualization of Countries",
    x = "t-SNE Dimension 1", y = "t-SNE Dimension 2"
  )

# ==============
# K-MEANS ON tSNE
# ==============
set.seed(123)
k_tsne <- kmeans(tsne_result$Y, centers = 4)  # or however many clusters you want

tsne_data <- data.frame(
  X = tsne_result$Y[, 1],
  Y = tsne_result$Y[, 2],
  kcluster_tSNE = as.factor(k_tsne$cluster),
  Country = cleaned_data$Country
)

world <- ne_countries(scale = "medium", returnclass = "sf")

## t-SNE Dimensions
map_tsne <- tsne_data %>%
  select(Country, kcluster_tSNE)
world_data_tsne <- left_join(world, map_tsne, by = c("name" = "Country"))

# Plot
ggplot(data = world_data_tsne) +
  geom_sf(aes(fill = as.factor(kcluster_tSNE)), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "Set3", name = "t-SNE Cluster") +
  theme_minimal() +
  labs(title = "World Map Colored by t-SNE Clusters",
       subtitle = "Grouping based on t-SNE of socioeconomic indicators")


--------------------------------------------------------------------------------
# =============
# World Maps on K-means
# =============
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)

map_data<- life_expectancy_dataset %>%
  select(Country, Cluster_PCA, Cluster_tSNE)

world_data_pca <- left_join(world, 
                            life_expectancy_dataset %>% select(Country, Cluster_PCA), 
                            by = c("name" = "Country"))
world_data_tsne <- left_join(world, 
                             life_expectancy_dataset %>% select(Country, Cluster_tSNE), 
                             by = c("name" = "Country"))

map1 <- ggplot(world_data_pca) +
  geom_sf(aes(fill = Cluster_PCA), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "PCA Clustering") +
  theme_minimal()

map2 <- ggplot(world_data_tsne) +
  geom_sf(aes(fill = Cluster_tSNE), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "t-SNE Clustering") +
  theme_minimal()

print(map1)
print(map2)

# See them combined
map1 <- map1 + theme(legend.position = "none")
map2 <- map2 + theme(legend.position = "right")
combined_map <- plot_grid(map1, map2, ncol = 2, rel_widths = c(1, 1))
print(combined_map)

--------------------------------------------------------------------------------
# =============
# HIERARCHICAL CLUSTERING
# =============
# Compare the results of hierarchical clustering based on average, complete and ward linkage methods.
# Build the dendograms for each method

#Average, Complete, Ward linkage plot
h1<-hclust(dist(cleaned_data), method="average")
rect.hclust(h1, 4)

h2<-hclust(dist(cleaned_data), method="complete")
rect.hclust(h2, 4)

h3<-hclust(dist(cleaned_data), method="ward.D2")
rect.hclust(h3, 4)

# Try with 4 clusters 
# After trying the table method we see that Ward needs only 2

average <- cutree(h1, k=3)
complete<- cutree(h2, k=3)
ward<- cutree(h3, k=3)

# Compare the linkage methods in pairs
table(average,complete)
table(average, ward)
table(complete, ward)
table(average, complete)

# Visualize all three plots together
opar <- par(mfrow = c(1, 3))   
plot(h1, main="average linkage")
plot(h2, main="complete linkage")
plot(h3, main="Ward linkage")

table(average)
table(complete) 
table(ward)

# With 4 clusters, using the average and complete cluster methods creates a fourth group with only 
# one observation: try with three clusters


## Hierarchical clustering needs a check
## Add another heatmap
## Check dimensionality with TSNE

------------------------------------------------------------------------------------
# =================== 
# SUPERVISED LEARNING: predicting Status  ---- remember: remove last three variables (Clusters from scaled)
# ===================

library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(nnet)

cleaned_data <- cleaned_data[, !names(cleaned_data) %in% "Country"]

set.seed(123) 

# === Train / Test Split ===
split_index <- createDataPartition(cleaned_data[,-1]$Status, p = 0.7, list = FALSE)
train_data <- cleaned_data[split_index, -1]
test_data  <- cleaned_data[-split_index, -1]


# ================
# LOGISTIC REGRESSION
# ================

# 1. Check separation 
library(detectseparation)
X <- model.matrix(Status ~ ., data = train_data)[, -1]  # remove intercept column
y <- as.factor(train_data$Status)

separation_check <- detect_separation(x = X, y = y, family = binomial())
print(separation_check)

# To avoid separation problems: 
library(glmnet)

# Fit logistic regression with LASSO (L1 penalty)
cv_fit <- cv.glmnet(x, y, family = "binomial", alpha = 1)

# Best model
best_model <- glmnet(x, y, family = "binomial", alpha = 1, lambda = cv_fit$lambda.min)

# Check coefficients
coef(best_model)

# check on test data
# Create x_test matrix (must match training features)
x_test <- model.matrix(Status ~ ., test_data)[, -1]  # Remove intercept
# Get true labels
y_test <- as.factor(test_data$Status)

# Predict probabilities
prob_pred <- predict(best_model, newx = x_test, type = "response")

# Classify with threshold 0.5
class_pred <- ifelse(prob_pred >= 0.5, 1, 0)
class_pred <- as.factor(class_pred)

# confusion matrix
confusionMatrix(class_pred, y_test, positive = "1")

# ROC curve
library(pROC)

roc_obj <- roc(y_test, as.numeric(prob_pred))
plot(roc_obj, col = "blue", main = "ROC Curve")
auc(roc_obj)

--------------------------------------------------------------------------------
logit_model <- glm(Status ~ ., data = train_data, family = binomial)
summary(logit_model)

# Predict & evaluate
logit_probs <- predict(logit_model, newdata = test_data, type = "response")
logit_pred <- ifelse(logit_probs > 0.5, 1, 0)

cat("\n=== Logistic Regression ===\n")
print(table(Predicted = logit_pred, Actual = test_data$Status))
cat("Accuracy:", mean(logit_pred == test_data$Status), "\n")

# ===============
# DECISION TREE
# ===============
tree_model <- rpart(Status ~ ., data = train_data, method = "class")
rpart.plot(tree_model, main = "Decision Tree")

tree_pred <- predict(tree_model, newdata = test_data, type = "class")

cat("\n=== Decision Tree ===\n")
print(table(Predicted = tree_pred, Actual = test_data$Status))
cat("Accuracy:", mean(tree_pred == test_data$Status), "\n")

# ===============
# RANDOM FOREST
# ===============
rf_model <- randomForest(as.factor(Status) ~ ., data = train_data, ntree = 100, importance = TRUE)
rf_pred <- predict(rf_model, newdata = test_data)

cat("\n=== Random Forest ===\n")
print(table(Predicted = rf_pred, Actual = test_data$Status))
cat("Accuracy:", mean(rf_pred == test_data$Status), "\n")

# Feature Importance Plot
varImpPlot(rf_model, main = "Variable Importance (Random Forest)")

--------------------------------------------------------------------------------

# =================== 
# SUPERVISED LEARNING: predicting life expectancy  ---- remember: remove last three variables (Clusters from scaled)
# ===================

# === Data preparation pipeline ===

# 1. Categorize the continuous target into 3 balanced classes
quantiles <- quantile(scaled_lifeexp$life_expectancy, probs = c(0, 1/3, 2/3, 1))
scaled_lifeexp$lifeexp_cat <- cut(
  scaled_lifeexp$life_expectancy,
  breaks = quantiles,
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)
table(scaled_lifeexp$lifeexp_cat)  # Check the class distribution

# 2. Select only the relevant variables (drop target and clustering-related columns)
vars_to_use <- setdiff(
  names(scaled_lifeexp),
  c("life_expectancy", "lifeexp_cat", "Status", "Cluster")
)
model_data <- scaled_lifeexp[, c(vars_to_use, "lifeexp_cat")]

# 3. Make sure your target is a proper factor
model_data$lifeexp_cat <- factor(model_data$lifeexp_cat, levels = c("Low", "Medium", "High"))

# 4. Train/test split
set.seed(123)
split_index <- createDataPartition(model_data$lifeexp_cat, p = 0.7, list = FALSE)
train_data <- model_data[split_index, ]
test_data <- model_data[-split_index, ]

# ==============
# LOGISTIC REGRESSION (multinomial)
# ==============
logit_model <- multinom(lifeexp_cat ~ ., data = train_data)
logit_pred <- predict(logit_model, newdata = test_data)
logit_pred <- factor(logit_pred, levels = levels(test_data$lifeexp_cat))

cat("Logistic Regression Accuracy:", mean(logit_pred == test_data$lifeexp_cat), "\n")

# ===============
# DECISION TREE 
# ===============
tree_model <- rpart(lifeexp_cat ~ ., data = train_data, method = "class")
tree_pred <- predict(tree_model, newdata = test_data, type = "class")
tree_pred <- factor(tree_pred, levels = levels(test_data$lifeexp_cat))

cat("Decision Tree Accuracy:", mean(tree_pred == test_data$lifeexp_cat), "\n")
rpart.plot(tree_model)

# ===============
# RANDOM FOREST 
# ===============
rf_model <- randomForest(lifeexp_cat ~ ., data = train_data, ntree = 100, importance = TRUE)
rf_pred <- predict(rf_model, newdata = test_data)
rf_pred <- factor(rf_pred, levels = levels(test_data$lifeexp_cat))

cat("Random Forest Accuracy:", mean(rf_pred == test_data$lifeexp_cat), "\n")
varImpPlot(rf_model)



# ==================
# MODELS EVALUATION
# ==================
library(e1071)        # needed for confusionMatrix in caret

# Function to evaluate classification model
evaluate_model <- function(true_labels, predicted_labels, model_name = "") {
  cat("\n=== Evaluation:", model_name, "===\n")
  cm <- confusionMatrix(predicted_labels, true_labels)
  print(cm)
}

# Logistic Regression, Decision Tree, Random Forest Evaluations
evaluate_model(test_data$lifeexp_cat, logit_pred, "Logistic Regression")
evaluate_model(test_data$lifeexp_cat, tree_pred, "Decision Tree")
evaluate_model(test_data$lifeexp_cat, rf_pred, "Random Forest")


# === Model Tuning (Random Forest) ===
tuned_rf <- randomForest(
  lifeexp_cat ~ ., 
  data = train_data, 
  ntree = 500,       # Try more trees
  mtry = 5,          # Number of features tried at each split (can tune)
  importance = TRUE
)

# Evaluate
tuned_rf_pred <- predict(tuned_rf, newdata = test_data)
evaluate_model(test_data$lifeexp_cat, tuned_rf_pred, "Tuned Random Forest")

# Feature importance plot
varImpPlot(tuned_rf)


# ================
# VISUALIZATION
# ================

dev.off()

# === Confusion Matrix heatmap ===
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

# Example for Random Forest:
plot_confusion_heatmap(test_data$lifeexp_cat, rf_pred, "Random Forest")


# === Feature Importance ===
# Ex RF: which features are most important in predicting life expectancy categories.

# Already part of randomForest output
varImpPlot(rf_model, main = "Random Forest - Variable Importance")


# === Visualization of Class Distributions ===
# Check target class balance
ggplot(model_data, aes(x = lifeexp_cat, fill = lifeexp_cat)) +
  geom_bar() +
  labs(title = "Life Expectancy Class Distribution", x = "Category", y = "Count") +
  theme_minimal()


