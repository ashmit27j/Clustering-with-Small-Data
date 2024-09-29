# Install necessary packages if you haven't already
install.packages("ggplot2")
install.packages("cluster")
install.packages("factoextra")

# Load the libraries
library(ggplot2)
library(cluster)
library(factoextra)

# Load the iris dataset
data(iris)
head(iris)

# Remove the species column for clustering
iris_data <- iris[, -5]

# Calculate the total within-cluster saum of squares
wss <- (nrow(iris_data)-1)*sum(apply(iris_data, 2, var))

for (i in 2:15) {
  wss[i] <- sum(kmeans(iris_data, centers=i)$withinss)
}

# Plot the Elbow Method
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within-cluster sum of squares")

# Set the seed for reproducibility
set.seed(123)

# Apply K-Means clustering
kmeans_result <- kmeans(iris_data, centers=3, nstart=20)

# View the clustering results
print(kmeans_result)

# Add cluster assignment to the original dataset
iris$Cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters
fviz_cluster(kmeans_result, data = iris_data, geom = "point",
             ellipse.type = "convex", main = "K-Means Clustering of Iris Data",
             palette = "jco")

# Calculate silhouette width
silhouette_values <- silhouette(kmeans_result$cluster, dist(iris_data))

# Plot silhouette
fviz_silhouette(silhouette_values)

# Compare the clusters with actual species
table(iris$Species, iris$Cluster)
