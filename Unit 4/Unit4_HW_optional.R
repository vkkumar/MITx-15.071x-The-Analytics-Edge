statedata <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 4/statedataSimple.csv")
str(statedata)

stateLM = glm(Life.Exp ~ ., data = statedata)
summary(stateLM)

stateLM = lm(Life.Exp ~ .,
             data = statedata)
summary(stateLM)

pState = predict(stateLM)

SSE = sum((statedata$Life.Exp - pState)^2)
SSE

stateLM1 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost,
              data = statedata)
summary(stateLM1)
p1 = predict(stateLM1)
SSE1 = sum((statedata$Life.Exp - p1)^2)
SSE1

library(caTools)
library(rpart)
library(rpart.plot)

stateCART1 = glm(Life.Exp ~ ., data = statedata)
summary(stateCART1)
plot(stateCART1)

# CART Model
stateCART2 = rpart(Life.Exp ~ .,
                   data = statedata)
summary(stateCART2)
prp(stateCART2)

p3 = predict(stateCART2)
sum((statedata$Life.Exp - p3)^2)

# minbucket = 5
stateCART3 = rpart(Life.Exp ~ .,
                   data = statedata,
                   minbucket = 5)
summary(stateCART3)
prp(stateCART3)

p3 = predict(stateCART3)
sum((statedata$Life.Exp - p3)^2)

# minbucket = 1
stateCART4 = rpart(Life.Exp ~ Area,
                   data = statedata,
                   minbucket = 1)
summary(stateCART4)
prp(stateCART4)

p4 = predict(stateCART4)
sum((statedata$Life.Exp - p4)^2)

library(caret)
library(e1071)

set.seed(111)

numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

train(Life.Exp ~ ., data = statedata, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

stateCART5 = rpart(Life.Exp ~ .,
                   data = statedata,
                   cp = 0.12)
summary(stateCART5)
prp(stateCART5)

p4 = predict(stateCART5)
sum((statedata$Life.Exp - p4)^2)

set.seed(111)
train(Life.Exp ~ Area, data = statedata, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

stateCART6 = rpart(Life.Exp ~ Area,
                   data = statedata,
                   cp = 0.02)
summary(stateCART6)
prp(stateCART6)

p6 = predict(stateCART6)
sum((statedata$Life.Exp - p6)^2)