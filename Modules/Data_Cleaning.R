library(plotly)
library(dplyr)
library(e1071)
library(countrycode)
library(stringr)
library(rnaturalearth)

if (!exists("plot_cleaning_graphs")) plot_cleaning_graphs <- FALSE

# 1. Import the dataset
life_expectancy_dataset <- read.csv("life.expectancy.csv")

life_expectancy_dataset <- life_expectancy_dataset %>%
  mutate(Country = recode(Country,
                          "AntiguaandBarbuda" = "Antigua and Barbuda",
                          "BosniaandHerzegovina" = "Bosnia and Herzegovina",
                          "DemocraticPeople'sRepublicofKorea" = "Democratic People's Republic of Korea",
                          "Bolivia(PlurinationalStateof)" = "Bolivia",
                          "BruneiDarussalam" = "Brunei Darussalam",
                          "CaboVerde" = "Cabo Verde",
                          "CentralAfricanRepublic" = "Central African Republic", 
                          "Congo" = "Republic of the Congo",
                          "DemocraticRepublicoftheCongo" = "Democratic Republic of the Congo",
                          "DominicanRepublic" = "Dominican Republic",             
                          "EquatorialGuinea" = "Equatorial Guinea",  
                          "Iran(IslamicRepublicof)" = "Iran",
                          "LaoPeople'sDemocraticRepublic" = "Laos",
                          "Micronesia(FederatedStatesof)" = "Federated States of Micronesia", 
                          "Myanmar" = "Myanmar",
                          "PapuaNewGuinea" = "Papua New Guinea",
                          "RussianFederation" = "Russian Federation",
                          "SaintLucia" = "Saint Lucia",
                          "SaintVincentandtheGrenadines" = "Saint Vincent and the Grenadines",
                          "SaoTomeandPrincipe" = "Sao Tome and Principe",
                          "SolomonIslands" = "Solomon Islands",
                          "SouthAfrica" = "South Africa",
                          "SouthSudan" = "South Sudan",
                          "SriLanka" = "Sri Lanka",
                          "TrinidadandTobago" = "Trinidad and Tobago",
                          "UnitedArabEmirates" = "United Arab Emirates",
                          "UnitedKingdomofGreatBritainandNorthernIreland" = "United Kingdom",
                          "UnitedRepublicofTanzania" = "United Republic of Tanzania",
                          "UnitedStatesofAmerica" = "United States of America",
                          "Venezuela(BolivarianRepublicof)" = "Venezuela"))

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

# 7. Checking Status bilancement 
prop.table(table(scaled_lifeexp$Status))

# 8. Handle multicollinearity: drop highly correlated variables
corr_matrix <- cor(scaled_lifeexp[, -1])
high_corr_pairs <- which(abs(corr_matrix) >= 0.9 & abs(corr_matrix) < 1, arr.ind = TRUE)
high_corr_df <- data.frame(
  Var1 = rownames(corr_matrix)[high_corr_pairs[, 1]],
  Var2 = colnames(corr_matrix)[high_corr_pairs[, 2]],
  Correlation = corr_matrix[high_corr_pairs]
)
high_corr_df <- high_corr_df[high_corr_df$Var1 < high_corr_df$Var2, ]

vars_to_drop <- c("infant_deaths", "diphtheria", "thinness5_9years", "inc_composition")
life_expectancy_dataset <- scaled_lifeexp[, !(names(scaled_lifeexp) %in% vars_to_drop)]

world = ne_countries(scale = "medium", returnclass = "sf")

# 9. Add the function to standardize country names
standardize_country_names <- function(df, country_col = "Country") {
  df$Country_std <- countrycode(df[[country_col]],
                                origin = "country.name",
                                destination = "country.name")
  
  df$Country_std <- ifelse(is.na(df$Country_std), df[[country_col]], df$Country_std)

  correction_map <- c(
    "Antigua & Barbuda" = "Antigua and Barb.",
    "Bosnia & Herzegovina" = "Bosnia and Herz.",
    "Cape Verde" = "Cabo Verde",
    "Central African Republic" = "Central African Rep.",
    "Congo - Brazzaville" = "Congo",
    "Congo - Kinshasa" = "Dem. Rep. Congo",
    "Dominican Republic" = "Dominican Rep.",
    "Equatorial Guinea" = "Eq. Guinea",
    "Micronesia (Federated States of)" = "Micronesia",
    "Myanmar (Burma)" = "Myanmar",
    "St. Lucia" = "Saint Lucia",
    "St. Vincent & Grenadines" = "St. Vin. and Gren.",
    "São Tomé & Príncipe" = "São Tomé and Principe",
    "Solomon Islands" = "Solomon Is.",
    "South Sudan" = "S. Sudan",
    "Trinidad & Tobago" = "Trinidad and Tobago",
    "United States" = "United States of America"
  )
  
  df$Country_std <- ifelse(df$Country_std %in% names(correction_map),
                           correction_map[df$Country_std],
                           df$Country_std)
  
  unmatched <- setdiff(df$Country_std, world$name)
  
  if (length(unmatched) > 0) {
    warning("Careful: The following countries were not matched:\n",
            paste(unique(unmatched), collapse = ", "))
  }
  
  return(df)
}

scaled_lifeexp_final <- standardize_country_names(life_expectancy_dataset)

# 10. Graphs: do not print them when the module is called
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

# 11. Return only useful objects
result <- list(
  cleaned_data = scaled_lifeexp_final,
  dropped_variables = vars_to_drop,
  correlation_matrix = corr_matrix
)

return(result)