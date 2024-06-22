# Load necessary libraries
library(tidyr)
library(ggplot2)
library(car)
library(stats)
library(readr)

# Load the dataset
data <- read_csv("C:\\Users\\Bala Vignesh.A\\Desktop\\SCMA 632\\assam data.csv")

# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
cat("Missing values in each column:\n")
print(missing_values)

# Replace missing values with mean
numeric_cols <- sapply(data, is.numeric)
data[numeric_cols] <- lapply(data[numeric_cols], function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))

# Verify that there are no more missing values
missing_values_after <- sapply(data, function(x) sum(is.na(x)))
cat("Missing values after replacement:\n")
print(missing_values_after)

# Identify and remove outliers using the IQR method
numeric_columns <- colnames(data)[numeric_cols]
for (col in numeric_columns) {
  Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  outliers <- (data[[col]] < (Q1 - 1.5 * IQR)) | (data[[col]] > (Q3 + 1.5 * IQR))
  cat(paste(col, "has", sum(outliers), "outliers\n"))
  data <- data[!outliers, ]
}

# Select dependent and independent variables for regression
# Here we assume 'MPCE_URP' as the dependent variable and select a few independent variables for the regression model
dependent_variables <- c("MPCE_URP", "MPCE_MRP", "foodtotal_v", "foodtotal_q")
independent_variables <- c("hhdsz", "NIC_2008", "NCO_2004", "HH_type", "Religion", 
                           "Social_Group", "Whether_owns_any_land", "Type_of_land_owned", 
                           "Land_Owned", "Land_Leased_in", "Otherwise_possessed", 
                           "Land_Leased_out", "Land_Total_possessed", 
                           "During_July_June_Cultivated", "During_July_June_Irrigated", 
                           "NSS", "NSC", "MLT", "land_tt", "Cooking_code", "Lighting_code", 
                           "Dwelling_unit_code", "Regular_salary_earner", "Perform_Ceremony", 
                           "Meals_seved_to_non_hhld_members", "Possess_ration_card", 
                           "Type_of_ration_card")

# Ensure all columns are in the dataset
data <- data %>% 
  select(all_of(c(dependent_variables, independent_variables))) %>% 
  drop_na()

# Prepare the data for regression
y <- data[[dependent_variables[1]]]
y <- data[, dependent_variables]
x <- data[, independent_variables]

# Define dependent variable
y <- data$foodtotal_v

# Define independent variables
x <- data[, -which(names(data) == "foodtotal_v")]

# Fit the regression model
model <- lm(y ~ ., data = data)

# Print the regression results
print(summary(model))

# Visualize the results
ggplot(data = data.frame(y = y, fitted = model$fitted.values), aes(x = y, y = fitted)) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x = 'Actual MPCE_URP', y = 'Fitted MPCE_URP', title = 'Actual vs Fitted MPCE_URP') +
  theme_minimal()
