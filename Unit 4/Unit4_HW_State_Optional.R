data(state)
statedata = data.frame(state.x77)

str(statedata)

stateLinear = lm(Life.Exp ~ ., data = statedata)
summary(stateLinear)

SSE = sum(resid(stateLinear)^2)
SSE

stateLinear2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
summary(stateLinear2)
sum(resid(stateLinear2)^2)

stateCART = rpart(Life.Exp ~ ., data = statedata)
prp(stateCART)

statePredict = predict(stateCART, newdata = statedata)
sum(resid(stateCART)^2)

stateCART2 = rpart(Life.Exp ~ ., data = statedata, minbucket = 5)
prp(stateCART2)
sum(resid(stateCART2)^2)

stateCART3 = rpart(Life.Exp ~ Area, data = statedata, minbucket = 1)
prp(stateCART3)
sum(resid(stateCART3)^2)

library(caret)
library(e1071)

set.seed(111)
cartGrid = expand.grid(.cp = seq(0.01,0.5,0.01))
numFolds = trainControl(method = "cv", number = 10)
train(Life.Exp ~ ., data = statedata, method="rpart", trControl=numFolds, tuneGrid = cartGrid)
stateRF = rpart(Life.Exp ~ ., data=statedata, cp = 0.11)
prp(stateRF)


p = predict(stateRF, newdata=statedata)
c =table(statedata$Life.Exp, p)
c
