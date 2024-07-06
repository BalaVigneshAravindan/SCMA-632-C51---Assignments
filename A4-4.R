# Load necessary libraries
library(readr)
library(dplyr)
library(caret)
library(broom)
library(ggplot2)

# Load data
df <- read_csv("C:\\Users\\Bala Vignesh.A\\Desktop\\SCMA 632\\pizza_data.csv")

# Convert price and ranking to numeric
df$price <- as.numeric(gsub("\\$", "", df$price))
df$ranking <- as.numeric(df$ranking)

# Encode categorical variables
df$brand <- as.numeric(factor(df$brand))
df$weight <- as.numeric(factor(df$weight))
df$crust <- as.numeric(factor(df$crust))
df$cheese <- as.numeric(factor(df$cheese))
df$size <- as.numeric(factor(df$size))
df$toppings <- as.numeric(factor(df$toppings))
df$spicy <- as.numeric(factor(df$spicy))

# Define independent variables (X) and dependent variable (y)
X <- df[, c("brand", "price", "weight", "crust", "cheese", "size", "toppings", "spicy")]
y <- df$ranking

# Add a constant to the independent variables
X <- cbind(Intercept = 1, X)

# Fit the linear regression model
model <- lm(y ~ ., data = X)

# Print model summary
tidy(model)

# Plot coefficients

coefs <- tidy(model)
ggplot(coefs, aes(x = estimate, y = term)) + 
  geom_point() + 
  labs(x = "Coefficient Value", y = "Feature", title = "Conjoint Analysis - Feature Importance")

