library(plotly)
library(dplyr)
library(e1071)

if (!exists("plot_cleaning_graphs")) plot_cleaning_graphs <- FALSE

# 1. Import the dataset
life_expectancy_dataset <- read.csv("life.expectancy.csv")

# 2. Fix character variables
life_expectancy_dataset <- life_expectancy_dataset %>%
  mutate_at(vars(GDPpc, pop, hepatitisB, diphtheria, polio, deaths_under_five, 
                 measles, adult_mortality, infant_deaths), 
            ~ as.numeric(gsub(",", "", .)))

# 3. Substitute NA with variables' means
life_expectancy_dataset <- data.frame(lapply(
  life_expectancy_dataset, 
  function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)
  ))

# 4. Logarithmic transformation of skewed variables 
skew_vals <- sapply(life_expectancy_dataset[3:20], skewness)
skewed_pos <- names(skew_vals[skew_vals > 1])
skewed_neg <- names(skew_vals[skew_vals < -1])

for (var in skewed_pos) {
  life_expectancy_dataset[[var]] <- log1p(life_expectancy_dataset[[var]])
} 
for (var in skewed_neg) {
  life_expectancy_dataset[[var]] <- log1p(max(life_expectancy_dataset[[var]], na.rm = TRUE) + 1 - life_expectancy_dataset[[var]])
}

# 5. Standardization of the data
num_vars <- life_expectancy_dataset[, sapply(life_expectancy_dataset, is.numeric)]
num_vars_scaled <- scale(num_vars)

scaled_lifeexp <- cbind(
  life_expectancy_dataset[, !sapply(life_expectancy_dataset, is.numeric)], 
  as.data.frame(num_vars_scaled)
)

# 6. Encode Status in a dummy
scaled_lifeexp$Status <- ifelse(life_expectancy_dataset$Status == "Developed", 1, 0)

# 7. Handle multicollinearity: drop highly correlated variables
corr_matrix <- cor(scaled_lifeexp[, -1])
high_corr_pairs <- which(abs(corr_matrix) >= 0.9 & abs(corr_matrix) < 1, arr.ind = TRUE)
high_corr_df <- data.frame(
  Var1 = rownames(corr_matrix)[high_corr_pairs[, 1]],
  Var2 = colnames(corr_matrix)[high_corr_pairs[, 2]],
  Correlation = corr_matrix[high_corr_pairs]
)
high_corr_df <- high_corr_df[high_corr_df$Var1 < high_corr_df$Var2, ]

vars_to_drop <- c("infant_deaths", "diphtheria", "thinness5_9years", "inc_composition")
scaled_lifeexp_final <- scaled_lifeexp[, !(names(scaled_lifeexp) %in% vars_to_drop)]

# 8. Graphs: do not print them when the module is called
if (plot_cleaning_graphs) {
  library(gplots)
  
  # Check normality: QQ plot and normality test
  qqnorm(life_expectancy_dataset$life_expectancy)
  qqline(life_expectancy_dataset$life_expectancy, col="blue")
  print(shapiro.test(life_expectancy_dataset$life_expectancy))
  
  # Histograms for variables' distributions
  par(mfrow=c(4,5))  
  for(i in 3:20) {
    hist(life_expectancy_dataset[, i], main=names(life_expectancy_dataset)[i], col="lightblue")
  }
  par(mfrow=c(1,1))
  
  # Correlation map
  heatmap.2(corr_matrix, 
            main="Correlation map", 
            cex.main=0.8, 
            cexRow=0.6, 
            cexCol=0.6, 
            lwd=0.5,   
            trace="none", 
            sepwidth=c(0.01, 0.01), 
            colsep=1:ncol(corr_matrix), 
            rowsep=1:nrow(corr_matrix),
            srtCol=45,
            srtRow=45)
}

# 9. Return only useful objects
result <- list(
  cleaned_data = scaled_lifeexp_final,
  original_data = life_expectancy_dataset,
  dropped_variables = vars_to_drop,
  correlation_matrix = corr_matrix
)

return(result)
