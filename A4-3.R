# Load the ice cream dataset
df <- read.csv("C:\\Users\\Bala Vignesh.A\\Desktop\\SCMA 632\\icecream.csv")

# Calculate the dissimilarity matrix using Euclidean distance
dissimilarity_matrix <- as.matrix(dist(df[, 2:length(df)]))

# Apply MDS
mds <- cmdscale(dissimilarity_matrix, k = 2)

# Create a DataFrame with the MDS coordinates
mds_df <- data.frame(Dim1 = mds[, 1], Dim2 = mds[, 2], row.names = df[, 1])

# Plot the MDS configuration
plot(mds_df$Dim1, mds_df$Dim2, xlab = "Dim1", ylab = "Dim2", main = "MDS Configuration")
text(mds_df$Dim1, mds_df$Dim2, labels = rownames(mds_df), cex = 0.7)

# Interpret the results
print("MDS Configuration:")
print(mds_df)

# Calculate the original distances
original_distances <- as.matrix(dist(df[, 2:length(df)]))

# Calculate the distances in the MDS configuration
mds_distances <- as.matrix(dist(mds))

# Calculate the stress value
stress <- sum((original_distances - mds_distances)^2) / sum(original_distances^2)
print(paste("Stress value:", stress))
