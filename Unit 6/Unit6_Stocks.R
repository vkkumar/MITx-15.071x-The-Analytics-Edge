# Libraries
library(caTools)
library(caret)
library(flexclust)

stocks <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 6/StocksCluster.csv")
str(stocks)
summary(stocks)

# Baseline
table(stocks$PositiveDec)
(6324)/nrow(stocks) # 0.546114
corTable = cor(stocks[, 1:11])
corTable[1,1] = corTable[2, 2] = corTable[3, 3] = corTable[4, 4] = corTable[5, 5] = 0
corTable[6,6] = corTable[7, 7] = corTable[8, 8] = corTable[9, 9] = corTable[10, 10] = 0
corTable[11, 11] = 0

# Maximum correlation
which.max(corTable)/ncol(corTable)
corTable[which.max(corTable)]

# Maximum mean by month
which.max(colSums(stocks[, 1:11])/nrow(stocks))
which.min(colSums(stocks[, 1:11])/nrow(stocks))

set.seed(144)
# stocks$PositiveDec = as.factor(stocks$PositiveDec)

spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data = stocksTrain, family = 'binomial')

predLogStocks = predict(StocksModel, newdata = stocksTrain, type = 'response')
table(stocksTrain$PositiveDec, predLogStocks > 0.5)
(990+3640)/(nrow(stocksTrain))

predLogStocksTest = predict(StocksModel, newdata = stocksTest, type = 'response')
table(stocksTest$PositiveDec, predLogStocksTest > 0.5)
(417+1553)/nrow(stocksTest)
table(stocksTest$PositiveDec)
(1897)/nrow(stocksTest)

# Now run clustering
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
# Needing to know the dependent variable value to assign an observation to a 
# cluster defeats the purpose of the methodology

# Normalize Date with caret package
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTrain)
summary(normTest)

# k-means clustering
set.seed(144)
km = kmeans(normTrain, centers = 3)
km$size

# running flexclust
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTrain)

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

summary(stocksTrain1$PositiveDec)
summary(stocksTrain2$PositiveDec)
summary(stocksTrain3$PositiveDec)

StocksModel1 = glm(PositiveDec ~ ., data = stocksTrain1, family = 'binomial')
StocksModel2 = glm(PositiveDec ~ ., data = stocksTrain2, family = 'binomial')
StocksModel3 = glm(PositiveDec ~ ., data = stocksTrain3, family = 'binomial')

StocksModel1$coefficients
StocksModel2$coefficients
StocksModel3$coefficients

predSModel1 = predict(StocksModel1, newdata = stocksTrain1, type = 'response' )
predSModel2 = predict(StocksModel2, newdata = stocksTrain2, type = 'response' )
predSModel3 = predict(StocksModel3, newdata = stocksTrain3, type = 'response' )

PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type = 'response' )
PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type = 'response' )
PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type = 'response' )

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
(30+774)/nrow(stocksTest1)
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
(388+757)/nrow(stocksTest2)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
(49+13)/nrow(stocksTest3)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
(467+1544)/(467+1110+353+1544)
