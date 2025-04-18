# ===============
# Cleaning the dataset
# ===============
life_expectancy_dataset <- read.csv("life.expectancy.csv")
str(life_expectancy_dataset)

# Fix character variables
life_expectancy_dataset <- life_expectancy_dataset %>%
  mutate_at(vars(GDPpc, pop, hepatitisB, diphtheria, polio, deaths_under_five, 
                 measles, adult_mortality, infant_deaths), 
            ~ as.numeric(gsub(",", "", .)))

# Substitute NA with variable mean
life_expectancy_dataset <- data.frame(lapply(life_expectancy_dataset, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))


# =============
# DESCRIPTIVE ANALYSIS
# =============
head(life_expectancy_dataset)
summary(life_expectancy_dataset[, 3:20]) 

# Check normality
qqnorm(life_expectancy_dataset$life_expectancy)
qqline(life_expectancy_dataset$life_expectancy, col="blue")
shapiro.test(life_expectancy_dataset$life_expectancy)library(e1071)

# Histograms
par(mfrow=c(4,5))  
for(i in 3:20) {
  hist(life_expectancy_dataset[, i], main=names(life_expectancy_dataset)[i], col="lightblue")
}
par(mfrow=c(1,1))  

# Logarithmic transformation of skewed variables 
library(e1071)
skew_vals <- sapply(life_expectancy_dataset[3:20], skewness)
print(skew_vals)
skewed_pos <- names(skew_vals[skew_vals > 1])
skewed_neg <- names(skew_vals[skew_vals < -1])
for (var in skewed_pos) {
  life_expectancy_dataset[[var]] <- log1p(life_expectancy_dataset[[var]])} 
for (var in skewed_neg) {
  life_expectancy_dataset[[var]] <- log1p(max(life_expectancy_dataset[[var]], na.rm = TRUE) + 1 - life_expectancy_dataset[[var]])
}

# Standardize the data
num_vars <- life_expectancy_dataset[, sapply(life_expectancy_dataset, is.numeric)]
num_vars_scaled <- scale(num_vars)

scaled_lifeexp <- cbind(
  life_expectancy_dataset[, !sapply(life_expectancy_dataset, is.numeric)], 
  as.data.frame(num_vars_scaled)
)

# Encode Status in a dummy
scaled_lifeexp$Status <- ifelse(life_expectancy_dataset$Status == "Developed", 1, 0)

# Verify correlation between variables to avoid multicollinearity
corr_matrix <- cor(scaled_lifeexp[, -1])
heatmap(corr_matrix, 
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
