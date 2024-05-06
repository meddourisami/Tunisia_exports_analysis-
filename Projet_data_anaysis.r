library(tidyverse)
library(corrplot)
library(MASS)
library(psych)
library(foreign)
library(dplyr)

# Read the data
df <- read.csv('C:\\Users\\lenovo\\Desktop\\TunisiaExport.csv')

# Check for missing values
print(colSums(is.na(df)))

# Dropping columns with missing values
df <- subset(df, select = -Services..non.marchands)

# Converting string values to numeric values
df <- df %>% mutate(across(where(is.character), ~as.numeric(gsub(',', '', .))))

# Calculating basic statistics indicators
print(describe(df))

# Showing the first 5 lines
head(df)

# Line plot for each sector over time
df %>% 
  pivot_longer(-Total, names_to = "Sector", values_to = "Value") %>% 
  ggplot(aes(x = Total, y = Value, color = Sector)) +
  geom_line() +
  labs(x = "Total", y = "Value", title = "Trends in Different Sectors Over Time") +
  theme_minimal()

# Calculate growth rates for each sector
df_growth <- df
df_growth[-1] <- lapply(df_growth[-1], function(x) diff(x) / lag(x, default = 1) * 100)

# Plot growth rates for each sector
df_growth %>% 
  pivot_longer(-Total, names_to = "Sector", values_to = "Growth Rate") %>% 
  ggplot(aes(x = Total, y = `Growth Rate`, color = Sector)) +
  geom_line() +
  labs(x = "Total", y = "Growth Rate (%)", title = "Comparative Growth Rates of Different Sectors Over Time") +
  theme_minimal()

# Plot distributions of variables
df %>%
  subset(select = -Total) %>%
  lapply(hist)

# Calculate correlation matrix
corr_matrix <- cor(df)

# Plot correlation heatmap
corrplot(corr_matrix, method = "color", type = "lower", addCoef.col = "black")

# Ordinary least squares model
model <- lm(Total ~ ., data = df)
summary(model)

# Plot residuals
plot(model$fitted.values, residuals(model))

# Identify outliers using z-score
z_scores <- scale(df)
outliers <- abs(z_scores) > 3

# Removing outliers
data_no_outliers <- df[!apply(outliers, 1, any), ]

# Transform data (example: log transformation)
data_transformed <- log(df)

# Using Robust linear regression model using Huber loss function
model_robust <- rlm(Total ~ ., data = df)
summary(model_robust)

# Principal Component Analysis
quantitative_variables <- df[-c(1, ncol(df))] # Excluding 'Période' and 'Total' columns
scaled_data <- scale(quantitative_variables)
pca <- princomp(scaled_data)
summary(pca)

# Explained variance ratio
explained_variance_ratio <- pca$sdev^2 / sum(pca$sdev^2)
cat("Explained variance ratio:", explained_variance_ratio, "\n")

# Cumulative explained variance ratio
cumulative_variance_ratio <- cumsum(explained_variance_ratio)
cat("Cumulative explained variance ratio:", cumulative_variance_ratio, "\n")

