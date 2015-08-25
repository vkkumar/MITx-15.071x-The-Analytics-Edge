Airlines <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 6/AirlinesCluster.csv")
str(Airlines)
summary(Airlines)
sort(colMeans(Airlines))

# Normalize Data:
# If we don't normalize the data, the clustering will be dominated by the 
# variables that are on a larger scale.
library(caret)
preproc = preProcess(Airlines)
airlinesNorm = predict(preproc, Airlines)
summary(airlinesNorm)

# Distances
distances = dist(airlinesNorm, method = "euclidean")

airlinesCluster = hclust(distances, method = "ward.D")
plot(airlinesCluster)

# Cutree, k = 5

airlinesCutCluster5 = cutree(airlinesCluster, k = 5)
table(airlinesCutCluster5)

# Compute average values in each cluster
tapply(Airlines$Balance, airlinesCutCluster5, mean)
tapply(Airlines$QualMiles, airlinesCutCluster5, mean)
tapply(Airlines$BonusMiles, airlinesCutCluster5, mean)
tapply(Airlines$BonusTrans, airlinesCutCluster5, mean)
tapply(Airlines$FlightMiles, airlinesCutCluster5, mean)
tapply(Airlines$FlightTrans, airlinesCutCluster5, mean)
tapply(Airlines$DaysSinceEnroll, airlinesCutCluster5, mean)

lapply(split(Airlines, airlinesCutCluster5), colMeans)

# Clustering with iter.max = 1000
set.seed(88)
KMC1 = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
summary(KMC1)
airlinesClusterKMC = KMC1$cluster
str(airlinesClusterKMC)
table(airlinesClusterKMC)

KMC1$centers
KMC1$size
