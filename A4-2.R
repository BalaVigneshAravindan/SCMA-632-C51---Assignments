# Load necessary libraries
library(dplyr)
library(readr)

# Read the data
df <- read_csv("C:/Users/Bala Vignesh.A/Desktop/SCMA 632/Survey (1).csv")

# Display column names
print(colnames(df))

# Display column names
existing_cols <- colnames(df)
print(existing_cols)

# Define the correct columns to be encoded (modify this list based on the actual column names)
cols <- c('City', 'Sex', 'Occupation', 'Income', 'Planning to Buy a new house', 
          'Time Frame', 'Reasons for buying a house', 'what type of House', 
          'Number of rooms', 'Size of House', 'Budget', 'Finished/Semi Finished', 
          'Influence Decision', 'EMI', 'X1.Proximity.to.city', 
          'X2.Proximity.to.schools', 'X3.Proximity.to.transport', 
          'X4.Proximity.to.work.place', 'X5.Proximity.to.shopping', 
          'X1.Gym.Pool.Sports.facility', 'X2.Parking.space', 'X3.Power.back.up', 
          'X4.Water.supply', 'X5.Security', 'X1.Exterior.look', 'X2.Unit.size', 
          'X3.Interior.design.and.branded.components', 
          'X4.Layout.plan.Integrated.etc.', 'X5.View.from.apartment', 'X1.Price', 
          'X2.Booking.amount', 'X3.Equated.Monthly.Instalment.EMI', 
          'X4.Maintenance.charges', 'X5.Availability.of.loan', 
          'X1.Builder.reputation', 'X2.Appreciation.potential', 
          'X3.Profile.of.neighbourhood', 'X4.Availability.of.domestic.help', 
          'Time', 'Size', 'Budgets', 'Maintainances', 'EMI.1', 'ages', 'sex', 
          'Finished.Semi.Finished.1', 'Influence.Decision.1')

# Check missing columns
missing_cols <- setdiff(cols, existing_cols)
if (length(missing_cols) > 0) {
  print("The following columns are missing from the DataFrame:")
  print(missing_cols)
} else {
  # Preprocess the data: Encode categorical variables
  df_encoded <- df %>%
    mutate(across(all_of(cols), as.numeric))
  
  # Select background variables
  background_vars <- df_encoded %>% select(tail(cols, 20))
  
  # Scale the data
  background_vars_scaled <- scale(background_vars)
  
  # Determine the optimal number of clusters using the elbow method
  wcss <- numeric(10)
  for (k in 1:10) {
    set.seed(42)
    kmeans_result <- kmeans(background_vars_scaled, centers = k, nstart = 25)
    wcss[k] <- kmeans_result$tot.withinss
  }

# Plot the elbow method
  plot(1:10, wcss, type = "b", pch = 19, frame = FALSE,
       xlab = "Number of clusters",
       ylab = "WCSS",
       main = "Elbow Method for Optimal Number of Clusters")  
  
# Perform K-means clustering with 3 clusters
  set.seed(42)
  kmeans_result <- kmeans(background_vars_scaled, centers = 3, nstart = 25)
  df$Cluster <- kmeans_result$cluster  

# Analyze and interpret clusters
  cluster_summary <- df %>%
    group_by(Cluster) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))
  
  print(cluster_summary)  

# Visualize the clusters
  library(ggplot2)
  ggplot(df, aes(x = factor(Cluster), y = ages)) +
    geom_bar(stat = "summary", fun = "mean") +
    labs(title = "Distribution of Ages by Cluster", x = "Cluster", y = "Mean Age")
}  
