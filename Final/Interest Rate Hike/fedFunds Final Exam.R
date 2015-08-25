setwd("~/Dropbox/EdX/The Analytics Edge/Final/Interest Rate Hike")
fedFunds <- read.csv("federalFundsRate.csv", stringsAsFactors=FALSE)

str(fedFunds)
prop.table(table(fedFunds$RaisedFedFunds))
which.max(table(fedFunds$Chairman))
table(fedFunds$Chairman, fedFunds$RaisedFedFunds)

fedFunds$Chairman       = as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres = as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds = as.factor(fedFunds$RaisedFedFunds)

set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)

training = subset(fedFunds, spl == TRUE)
testing  = subset(fedFunds, spl == FALSE)

fundsGLM = glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + 
                                HomeownershipRate + DemocraticPres +
                                MonthsUntilElection,
               data = training, family = "binomial")
summary(fundsGLM)

testSet = data.frame(PreviousRate = 1.7, Streak = -3, Unemployment = 5.1,
                     HomeownershipRate = 65.3, DemocraticPres = 0,
                     MonthsUntilElection = 18)
testSet$DemocraticPres = as.factor(testSet$DemocraticPres)

predict(fundsGLM, newdata = testSet, type = "response")

# Predict on testing dataset
testingPred = predict(fundsGLM ,newdata = testing, type = "response")
table(testing$RaisedFedFunds, testingPred > 0.5)

(60+57)/nrow(testing)                     # Accuracy
table(testingPred < 0.5)                  # Number of different predictions

# AUC calculation
library(ROCR)
predROCR = prediction(testingPred, testing$RaisedFedFunds)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE, grid = TRUE)
performance(predROCR, "auc")@y.values

# Cross-validation for CART model using caret package
set.seed(201)
library(caret)
library(e1071)
library(rattle)

tuneGrid = expand.grid(.cp = (1:50)*0.001)
trnCtrl  = trainControl(method = "cv",
                        number = 10,
                        repeats = 10)

formula = RaisedFedFunds ~ PreviousRate + Streak + Unemployment + 
          HomeownershipRate + DemocraticPres + MonthsUntilElection

trainCART = train(formula,
                  data = training,
                  method = "rpart",
                  trControl = trnCtrl,
                  tuneGrid = tuneGrid)
trainCART

fundsCART = rpart(formula,
                  data = training,
                  control = rpart.control(cp = 0.016))
fancyRpartPlot(fundsCART)

predict(fundsCART, newdata = testSet, type = "prob")

fundsPredCART = predict(fundsCART, newdata = testing, type = "class")
table(testing$RaisedFedFunds, fundsPredCART)
(64 + 48)/nrow(testing)
