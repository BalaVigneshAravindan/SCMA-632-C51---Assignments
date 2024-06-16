# Load necessary libraries
library(dplyr)
data <- read.csv("C:/Users/Bala Vignesh.A/Desktop/SCMA 632/assam data.csv")
# Filter data for Assam
assam_data <- filter(data, state_1 == 'ASSM')
# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print("Missing values in each column:")
print(missing_values)
# Replace missing values with mean
assam_data <- data.frame(lapply(assam_data, function(x){ if (is.numeric(x))
{x[is.na(x)] <- mean(x, na.rm = TRUE)}
  return(x)
}))
{x[is.na(x)] <- mean(x, na.rm = TRUE)}
return(x)
# Verify that there are no more missing values
missing_values_after <- sapply(assam_data, function(x) sum(is.na(x)))
# Replace missing values with mean
assam_data <- data.frame(lapply(assam_data, function(x){
  if (is.numeric(x)){
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
}))
# Verify that there are no more missing values
missing_values_after <- sapply(assam_data, function(x) sum(is.na(x)))
print("Missing values after replacement for Assam:")
print(missing_values_after)
# Identify outliers using the IQR method and optionally remove them
numeric_columns <- sapply(assam_data, is.numeric)
for (col in names(assam_data)[numeric_columns]) {
  Q1 <- quantile(assam_data[[col]], 0.25)
  Q3 <- quantile(assam_data[[col]], 0.75)
  IQR <- Q3 - Q1
  outliers <- (assam_data[[col]] < (Q1 - 1.5 * IQR)) | (assam_data[[col]] > (Q3 + 1.5 * IQR))
  print(paste(col, "has", sum(outliers), "outliers in Assam"))
  # Remove outliers (according to IQR method)
  #Interquartile Range (IQR): Calculate the difference between Q3 and Q1, which represents the range of values within which the middle 50% of the data points fall.
  #IQR = Q3 - Q1
  #Outlier detection: Any data point that falls outside the range of 1.5 times the IQR below Q1 or above Q3 is considered an outlier.
  assam_data <- assam_data[!(assam_data[[col]] < (Q1 - 1.5 * IQR) | assam_data[[col]] > (Q3 + 1.5 * IQR)), ]
}
print(paste(col, "has", sum(outliers), "outliers in Assam"))
for (col in names(assam_data)[numeric_columns]) {
  Q1 <- quantile(assam_data[[col]], 0.25)
  Q3 <- quantile(assam_data[[col]], 0.75)
  IQR <- Q3 - Q1
  outliers <- (assam_data[[col]] < (Q1 - 1.5 * IQR)) | (assam_data[[col]] > (Q3 + 1.5 * IQR))
  print(paste(col, "has", sum(outliers), "outliers in Assam"))
  # Remove outliers (according to IQR method)
  #Interquartile Range (IQR): Calculate the difference between Q3 and Q1, which represents the range of values within which the middle 50% of the data points fall.
  #IQR = Q3 - Q1
  #Outlier detection: Any data point that falls outside the range of 1.5 times the IQR below Q1 or above Q3 is considered an outlier.
  assam_data <- assam_data[!(assam_data[[col]] < (Q1 - 1.5 * IQR) | assam_data[[col]] > (Q3 + 1.5 * IQR)), ]
}
# Check if the column exists
if ("old_district_column_name" %in% colnames(assam_data)) {
  # If the column exists, rename it
  assam_data <- rename(assam_data, new_district_column_name = old_district_column_name)
} else {
  # If the column doesn't exist, print a message
  print("Column 'old_district_column_name' doesn't exist in the data.")
}
outlier_iqr <- function(x, k = 1.5) {
  x <- x[!is.na(x) &!is.nan(x)]  # remove NA and NaN values
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_limit <- q1 - k * iqr
  upper_limit <- q3 + k * iqr
  return(x[x >= lower_limit & x <= upper_limit])
}
outlier_iqr <- function(x, k = 1.5) {
  summary_stats <- summary(x)
  q1 <- summary_stats[2]
  q3 <- summary_stats[5]
  iqr <- q3 - q1
  lower_limit <- q1 - k * iqr
  upper_limit <- q3 + k * iqr
  return(x[x >= lower_limit & x <= upper_limit])
}
region_summary <- assam_data %>%
  group_by(Region) %>%
  summarize(
    avg_consumption = if("Consumption" %in% names(assam_data)) {
      mean(Consumption, na.rm = TRUE)
    } else {
      NA
    },
    min_consumption = if("Consumption" %in% names(assam_data)) {
      min(Consumption, na.rm = TRUE)
    } else {
      NA
    },
    max_consumption = if("Consumption" %in% names(assam_data)) {
      max(Consumption, na.rm = TRUE)
    } else {
      NA
    }
  )
# Summarize the critical variables region-wise
region_summary <- assam_data %>%
  group_by(FOD_Sub_Region) %>%
  summarize(avg_No_of_Meals_per_day = mean(No_of_Meals_per_day, na.rm = TRUE),
            min_No_of_Meals_per_day = min(No_of_Meals_per_day, na.rm = TRUE),
            max_No_of_Meals_per_day= max(No_of_Meals_per_day, na.rm = TRUE))
# Summarize the critical variables district-wise
district_summary <- assam_data %>%
  group_by(District) %>%
  summarize(avg_No_of_Meals_per_day = mean(No_of_Meals_per_day, na.rm = TRUE),
            min_No_of_Meals_per_day = min(No_of_Meals_per_day, na.rm = TRUE),
            max_No_of_Meals_per_day = max(No_of_Meals_per_day, na.rm = TRUE))
assam_data <- assam_data %>%
  mutate(District = case_when(
    District == "18" ~ "ASSM Urban",
    TRUE ~ as.character(District)  # or TRUE ~ District, depending on your needs
  ))
# Identify the top and bottom three districts of consumption
top_three_districts <- district_summary %>%
  arrange(desc(avg_No_of_Meals_per_day)) %>%
  head(3)
bottom_three_districts <- district_summary %>%
  arrange(avg_No_of_Meals_per_day) %>%
  head(3)
# Filter data for Assam
assam_data <- filter(data, state_1 == 'ASSM')
summary(anova_result)
# Test whether the differences in the means are significant or not
anova_result <- aov(No_of_Meals_per_day ~ District, data = assam_data)
summary(anova_result)
print(anova_summary)
# Summarize the critical variables district-wise
district_summary <- assam_data %>%
  group_by(District) %>%
  summarize(avg_No_of_Meals_per_day = mean(No_of_Meals_per_day, na.rm = TRUE),
            min_No_of_Meals_per_day = min(No_of_Meals_per_day, na.rm = TRUE),
            max_No_of_Meals_per_day = max(No_of_Meals_per_day, na.rm = TRUE))
# Summarize the critical variables district-wise
district_summary <- assam_data %>%
  group_by(District) %>%
  summarize(avg_No_of_Meals_per_day = mean(No_of_Meals_per_day, na.rm = TRUE),
            min_No_of_Meals_per_day = min(No_of_Meals_per_day, na.rm = TRUE),
            max_No_of_Meals_per_day = max(No_of_Meals_per_day, na.rm = TRUE))
# Summarize the critical variables district-wise
district_summary <- assam_data %>%
  group_by(District) %>%
  summarize(avg_No_of_Meals_per_day = mean(No_of_Meals_per_day, na.rm = TRUE),
            min_No_of_Meals_per_day = min(No_of_Meals_per_day, na.rm = TRUE),
            max_No_of_Meals_per_day = max(No_of_Meals_per_day, na.rm = TRUE))
# Identify the top and bottom three districts of consumption
top_three_districts <- district_summary %>%
  arrange(desc(avg_No_of_Meals_per_day)) %>%
  head(3)
bottom_three_districts <- district_summary %>%
  arrange(avg_No_of_Meals_per_day) %>%
  head(3)
# Test whether the differences in the means are significant or not
anova_result <- aov(No_of_Meals_per_day ~ District, data = assam_data)
anova_summary <- summary(anova_result)
cat("ANOVA results:\n")
print(anova_summary)
View(anova_result)
View(anova_result)
View(outlier_iqr)
save.image("C:/Users/Bala Vignesh.A/Desktop/A1a.RData")
q()
View(anova_result)
install.packages("ggplot2")
# Load the ggplot2 package
library(ggplot2)
# Libraries
library(ggplot2)

# Data from ANOVA summary
data <- data.frame(
  Source = c("District", "Residuals"),
  Mean_Sq = c(0.0580, 0.1634)
)

# Plot
ggplot(data, aes(x = Source, y = Mean_Sq)) +
  geom_bar(stat = "identity", color = "skyblue") +
  labs(title = "Mean Squares (Effect Sizes)", x = "Source", y = "Variance")


