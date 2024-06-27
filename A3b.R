# Load the necessary libraries
library(tidyverse)
library(mice)
library(car)
library(ggplot2)
library(lattice)
library(caret)
library(glmnet)
library(Matrix)

# Read in the data
data <- read.csv("C:\\Users\\Bala Vignesh.A\\Desktop\\SCMA 632\\NSSO68.csv")

# Filter the data for non-vegetarians
non_veg_data <- data[data$non_veg == 1, ]

# View the non-veg values
non_veg_values <- non_veg_data$non_veg
print(non_veg_values)

# Get the value counts of non_veg
non_veg_values <- non_veg_data$non_veg
value_counts <- table(non_veg_values)
print(value_counts)

# Define the dependent variable (non_veg) and independent variables
y <- non_veg_data$non_veg
X <- non_veg_data[,!(names(non_veg_data) %in% c("non_veg", "state_1", "Region"))]

# Check for non-numeric columns
non_numeric_cols <- sapply(X, class) == "character"
non_numeric_cols <- names(non_numeric_cols)[non_numeric_cols]

print(paste("Non-numeric columns:", non_numeric_cols))

# One-hot encode categorical columns
dummy_model <- dummyVars(~., data = X)
X_ohe_df <- as.data.frame(predict(dummy_model, newdata = X))

# Combine numeric and one-hot encoded columns
X_numeric <- X[, sapply(X, class)!= "character"]
X_combined <- cbind(X_numeric, X_ohe_df)

# Ensure 'y' is a binary factor
y <- as.factor(y)

# Remove columns with single unique value
X_combined <- X_combined[, sapply(X_combined, function(x) length(unique(x)) > 1)]

# Check dimensions
print(dim(y))
print(dim(X_combined))

# Create the combined data frame
combined_data <- data.frame(y, X_combined)

# Inspect the combined data
str(combined_data)
head(combined_data)

# Check for missing values in X_combined and y
sum(is.na(X_combined))
sum(is.na(y))

# Impute missing values if necessary
X_combined <- na.omit(X_combined)
y <- y[!is.na(X_combined)]

# Check variables with zero standard deviation
zero_sd_vars <- colnames(X_combined)[apply(X_combined, 2, sd) == 0]
print(zero_sd_vars)

# Remove constant variables from X_combined
X_combined <- X_combined[,!colnames(X_combined) %in% zero_sd_vars]

# Compute correlation matrix
cor_matrix <- cor(X_combined, use = "pairwise.complete.obs")

# Find highly correlated predictors
high_cor <- findCorrelation(cor_matrix, cutoff = 0.9)

# Remove highly correlated predictors from X_combined
X_combined <- X_combined[, -high_cor]

dim(X_combined)
length(y)

y <- y[1:nrow(X_combined)]
combined_data <- data.frame(y = y, X_combined)
dim(combined_data)

# Create a matrix of predictor variables
x <- as.matrix(X_combined)

# Create a response variable
y <- as.numeric(as.character(y)) - 1

# Impute missing values using makeX
x_imputed <- makeX(as.data.frame(x), missing = TRUE)
imputed_data <- mice(X_combined)
x_imputed <- complete(imputed_data)

anyNA(x_imputed)
anyNA(y)

# Convert x_imputed to sparse matrix
x_sparse <- as(as.matrix(x_imputed), "sparseMatrix")

# Fit the model using glmnet with sparse matrix
probit_model <- glm(non_veg ~ hhdsz + NIC_2008 + NCO_2004 + HH_type + Religion + Social_Group, 
                    data = non_veg_data, 
                    family = binomial(link = "probit"),
                    control = list(maxit = 1000))
non_veg_data$hhdsz_scaled <- scale(non_veg_data$hhdsz)
non_veg_data$NIC_2008_scaled <- scale(non_veg_data$NIC_2008)


# Print model summary or other relevant outputs
print(probit_model)
