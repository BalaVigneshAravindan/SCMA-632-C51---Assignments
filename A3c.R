# Load required libraries
library(readr)
library(censReg)

# Load your dataset 
data <- read_csv("C:/Users/Bala Vignesh.A/Desktop/SCMA 632/NSSO68.csv")

# Define your dependent variable and independent variables
y <- data$foodtotal_v
X <- data[, c("sauce_jam_v", "Othrprocessed_v", "Beveragestotal_v", "fv_tot")]

# Prepare the data for Tobit regression
y_tobit <- pmin(pmax(y, 0), 1)  
X_tobit <- cbind(1, X) 

# Fit the Tobit model
X_tobit_df <- as.data.frame(X_tobit)
model <- censReg(y_tobit ~ ., data = X_tobit_df[, -1])

# Print Tobit model summary
summary(model)

