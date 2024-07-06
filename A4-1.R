if(!require(fastDummies)) install.packages("fastDummies", dependencies=TRUE)
library(fastDummies)

# Load the data
data <- read.csv("C:/Users/Bala Vignesh.A/Desktop/SCMA 632/Survey.csv")

# Convert categorical variables to dummy variables
encoded_data <- dummy_cols(data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Remove constant or zero-variance columns
encoded_data <- encoded_data[, apply(encoded_data, 2, function(col) length(unique(col)) > 1)]

# Perform PCA (prcomp function is part of base stats package)
pca <- prcomp(encoded_data, center = TRUE, scale. = TRUE)

# Print the results
print(pca$x)


The error you're encountering indicates that there are constant or zero-variance columns in your dataset, which cannot be scaled to unit variance. To resolve this, you need to remove these columns before performing PCA. Here's how you can modify the code to handle this situation:
  
  PCA (Principal Component Analysis)
r
Copy code
# Load necessary library for dummy variable creation
if(!require(fastDummies)) install.packages("fastDummies", dependencies=TRUE)
library(fastDummies)

# Load the data
data <- read.csv("C:/Users/Bala Vignesh.A/Desktop/SCMA 632/Survey.csv")

# Convert categorical variables to dummy variables
encoded_data <- dummy_cols(data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Remove constant or zero-variance columns
encoded_data <- encoded_data[, apply(encoded_data, 2, function(col) length(unique(col)) > 1)]

# Perform PCA (prcomp function is part of base stats package)
pca <- prcomp(encoded_data, center = TRUE, scale. = TRUE)

# Print the results
print(pca$x)

Factor Analysis

# Load necessary library for Factor Analysis
if(!require(psych)) install.packages("psych", dependencies=TRUE)
library(psych)

# Load the data
data <- read.csv("C:/Users/Bala Vignesh.A/Desktop/SCMA 632/Survey.csv")

# Remove missing values and select numeric columns
data_processed <- na.omit(data)
data_processed <- data_processed[, sapply(data_processed, is.numeric)]

# Perform Factor Analysis
fa <- fa(data_processed, nfactors = 2, rotate = "varimax")

# Print the results
print(fa$loadings)
