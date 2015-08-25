# Let's start by building a hierarchical clustering model. First, read the
# data set into R. Then, compute the distances (using method="euclidean"), and 
# use hclust to build the model (using method="ward.D"). You should cluster on 
# all of the variables.

# Running the dist function will probably take you a while. Why? Select all that apply.

dailykos <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 6/dailykos.csv")
str(dailykos)

# Compute distances
distances = dist(dailykos, method = "euclidean")

# Hierarchical clustering
dailykosCluster = hclust(distances, method = "ward.D")

# Plot the dendrogram
plot(dailykosCluster)

# Assign points to clusters
clusterGroups = cutree(dailykosCluster, k = 7)
str(clusterGroups)

# Create a new data set with just the movies from cluster 2
cluster1 = subset(dailykos, clusterGroups==1)
cluster2 = subset(dailykos, clusterGroups==2)
cluster3 = subset(dailykos, clusterGroups==3)
cluster4 = subset(dailykos, clusterGroups==4)
cluster5 = subset(dailykos, clusterGroups==5)
cluster6 = subset(dailykos, clusterGroups==6)
cluster7 = subset(dailykos, clusterGroups==7)

nrow(cluster1); nrow(cluster2)
nrow(cluster3); nrow(cluster4)
nrow(cluster5); nrow(cluster6)
nrow(cluster7);

sort(colMeans(cluster1), decreasing = TRUE)[1:6]
sort(colMeans(cluster2), decreasing = TRUE)[1:6]
sort(colMeans(cluster3), decreasing = TRUE)[1:6]
sort(colMeans(cluster4), decreasing = TRUE)[1:6]
sort(colMeans(cluster5), decreasing = TRUE)[1:6]
sort(colMeans(cluster6), decreasing = TRUE)[1:6]
sort(colMeans(cluster7), decreasing = TRUE)[1:6]

# k - means clustering
# Specify number of clusters
k = 7

# Run k-means
set.seed(1000)
KMC = kmeans(dailykos[-1], centers = k)
table(KMC$cluster)

# Extract clusters
KmeansCluster1 = subset(dailykos, KMC$cluster == 1)
KmeansCluster2 = subset(dailykos, KMC$cluster == 2)
KmeansCluster3 = subset(dailykos, KMC$cluster == 3)
KmeansCluster4 = subset(dailykos, KMC$cluster == 4)
KmeansCluster5 = subset(dailykos, KMC$cluster == 5)
KmeansCluster6 = subset(dailykos, KMC$cluster == 6)
KmeansCluster7 = subset(dailykos, KMC$cluster == 7)

# Alternatively
# Then cluster 1 can be accessed by typing KmeansCluster[[1]], cluster 2 can be
# accessed by typing KmeansCluster[[2]], etc. If you have a variable in your 
# current R session called "split", you will need to remove it with rm(split)
# before using the split function.
rm(split)
KmeansCluster = split(dailykos, KMC$cluster)
KmeansCluster[[1]]

sort(colMeans(KmeansCluster1), decreasing = TRUE)[1:6]
sort(colMeans(KmeansCluster2), decreasing = TRUE)[1:6]
sort(colMeans(KmeansCluster3), decreasing = TRUE)[1:6]
sort(colMeans(KmeansCluster4), decreasing = TRUE)[1:6]
sort(colMeans(KmeansCluster5), decreasing = TRUE)[1:6]
sort(colMeans(KmeansCluster6), decreasing = TRUE)[1:6]
sort(colMeans(KmeansCluster7), decreasing = TRUE)[1:6]
