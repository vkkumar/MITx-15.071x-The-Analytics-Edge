library(ggplot2)
library(maps)
# The maps package contains other built-in maps, including a US county map,
# a world map, and maps for France and Italy.

library(ggmap)
library(caTools)


statesMap = map_data("state")
str(statesMap)
summary(statesMap)
table(statesMap$group)

ggplot(statesMap, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

polling = read.csv("Pollingimputed.csv")
str(polling)
table(polling$Year)

Train = subset(polling, polling$Year != 2012)
Test = subset(polling, polling$Year == 2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
str(predictionDataFrame)

table(Test$Republican, TestPrediction == 1)
table(TestPredictionBinary)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
# Merge Data
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
str(predictionMap)
predictionMap = predictionMap[order(predictionMap$order),]
str(predictionMap)
str(statesMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = "legend", 
                      breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = "legend", 
                      breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")

TestNew = Test
TestNew$Prediction = TestPrediction
subset(TestNew, TestNew$State == 'Florida')
table(TestNew$State)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
  geom_polygon(color = "black", alpha = 0.3, linetype = 3) +
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = "legend", 
                      breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")
