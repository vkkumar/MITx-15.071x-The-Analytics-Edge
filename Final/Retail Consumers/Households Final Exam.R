setwd("~/Dropbox/EdX/The Analytics Edge/Final/Retail Consumers")
Households <- read.csv("Households.csv", stringsAsFactors=FALSE)

str(Households)
summary(Households$MorningPct)
hist(Households$MorningPct, breaks = 20)

library(corrplot)
library(rattle)
library(plyr)
library(ggplot2)

# How many households have logged transactions at the retailer only
# in the morning?
nrow(Households[Households$MorningPct == 100, ])

# How many households have logged transactions at the retailer only
# in the afternoon?
nrow(Households[Households$AfternoonPct == 100, ])

# Of the households that spend more than $150 per transaction on average, what
# is the minimum average discount per transaction?
min(Households[Households$AvgSalesValue > 150, ]$AvgDiscount)

# Of the households who have an average discount per transaction greater
# than 25%, what is the minimum average sales value per transaction?
min(Households[Households$AvgDiscount > 25, ]$AvgSalesValue)

# In the dataset, what proportion of households visited the retailer at least
# 300 times?
prop.table(table(Households$NumVisits >= 300))

# Normalizing the data
library(caret)
library(flexclust)

preproc = preProcess(Households)
HouseholdsNorm = predict(preproc, Households)

max(HouseholdsNorm$NumVisits)
min(HouseholdsNorm$AfternoonPct)

# Dendrogram of the data
set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

# K-means Clustering
km = kmeans(HouseholdsNorm, centers = 10)
km$size
km$centers

clusterDF = data.frame(km$centers)
clusterDF$cluster = c(1:10)

arrange(clusterDF, desc(MorningPct - AvgDiscount))
arrange(clusterDF, desc(MorningPct), AvgDiscount)

arrange(clusterDF, desc(AvgProdCount + AvgSalesValue))
arrange(clusterDF, desc(AvgProdCount), AvgSalesValue)

arrange(clusterDF, desc(NumVisits), AvgSalesValue)

# K-means with seed 5000
set.seed(5000)
km1 = kmeans(HouseholdsNorm, centers = 5)
km1$size

clusterDF = data.frame(km1$centers)
clusterDF$cluster = c(1:5)
arrange(clusterDF, desc(clusterDF$NumVisits), clusterDF$AvgDiscount)

boxplot(Households$NumVisits ~ km1$cluster)

