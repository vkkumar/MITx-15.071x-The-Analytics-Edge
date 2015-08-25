gerber <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 4/gerber.csv")
setwd("~/Dropbox/EdX/The Analytics Edge/Unit 4")

# Load packages
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(caTools)
library(ROCR)
library(randomForest)


str(gerber)
prop.table(as.matrix(table(gerber$voting)))
hawthorne = subset(gerber, hawthorne == 1)
prop.table(as.matrix(table(hawthorne$voting)))
#        [,1]
# 0 0.6776254
# 1 0.3223746

civicduty = subset(gerber, civicduty == 1)
prop.table(as.matrix(table(civicduty$voting)))
#        [,1]
# 0 0.6854623
# 1 0.3145377

neighbors = subset(gerber, neighbors == 1)
prop.table(as.matrix(table(neighbors$voting)))
#        [,1]
# 0 0.6220518
# 1 0.3779482

self = subset(gerber, self == 1)
prop.table(as.matrix(table(self$voting)))
#        [,1]
# 0 0.6548485
# 1 0.3451515

gerberLR = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial")
summary(gerberLR)


gerber.predict.LR = predict(gerberLR, gerber, type = 'response')
table(gerber$voting, gerber.predict.LR >= 0.3)
(51966+134513)/(nrow(gerber))

table(gerber$voting, gerber.predict.LR >= 0.5)
235388/nrow(gerber)

# as.numeric(performance(pred, "auc")@y.values)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodelControl = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodelControl, digits=6)
abs(0.296638-0.34)

CARTmodelSex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodelSex, digits=6)
abs(0.290456-0.302795)
abs(0.334176-0.345818)

gerberLR2 = glm(voting ~ control + sex, data=gerber, family="binomial")
summary(gerberLR2)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
Possibilities

predict(gerberLR2, newdata=Possibilities, type="response")
predict(CARTmodelSex, newdata=Possibilities)
abs(0.2908065 - 0.2904558)

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")
abs(0.2904558 - 0.2904558)

# HW 2
letters <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 4/letters_ABPR.csv", header=TRUE)
str(letters)

letters$isB = as.factor(letters$letter == "B")
set.seed(1000)

split = sample.split(letters$isB, SplitRatio = 0.5)
lettersTrain = subset(letters, split == TRUE)
lettersTest  = subset(letters, split == FALSE)

baseline = rep(FALSE, nrow(lettersTest))
table(lettersTest$isB, baseline)
1175/nrow(lettersTest)

CARTb = rpart(isB ~ . - letter, data = lettersTrain, method = "class")
summary(CARTb)
prp(CARTb)
p = predict(CARTb, newdata = lettersTest, type="class")
c = table(lettersTest$isB, p)
c
(c[1,1]+c[2,2])/sum(c)

RFb = randomForest(isB ~ . - letter,
                   data=lettersTrain)
p = predict(RFb,
            newdata=lettersTest,
            type="class")

c = table(lettersTest$isB, p)
c
(c[1,1]+c[2,2])/sum(c)

letters$letter = as.factor( letters$letter )
table(letters$letter)

set.seed(2000)
split = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)

table(train$letter)
# Most frequent class is letter P
baseline = rep('P', nrow(train))
table(train$letter, baseline)
402/nrow(train)

CART = rpart(letter ~ . - isB,
             data = train)

p = predict(CART, 
            newdata = test,
            type = 'class')
table(test$letter, p)
(348+318+363+340)/sum(table(test$letter, p))


RF = randomForest(letter ~ . - isB,
                  data = train)

plot(RF, type = 'l')

p = predict(RF, 
            newdata = test,
            type = 'class')
table(test$letter, p)
(391+380+393+365)/sum(table(test$letter, p))


# HW 3
census <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 4/census.csv")
str(census)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio=0.6)
Train = subset(census, spl)
Test = subset(census, !spl)

censusLR = glm(over50k ~ ., data=Train, family="binomial")
summary(censusLR)

p = predict(censusLR, newdata=Test)
c =table(Test$over50k, p>=0.5)
c
(c[1,1]+c[2,2])/sum(c)

table(Train$over50k)
pbase = rep("<=50K", nrow(Test))
c =table(Test$over50k, pbase)
(c[1,1])/sum(c)

pred = prediction(p, Test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

censusRT = rpart(over50k ~ ., data=Train, method="class")
prp(censusRT)
p2 = predict(censusRT, newdata=Test)[,2]
c =table(Test$over50k, p2>=0.5)
(c[1,1]+c[2,2])/sum(c)


pred = prediction(p2, Test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

set.seed(1)
trainSmall = Train[sample(nrow(Train), 2000), ]
censusRF = randomForest(over50k ~ ., data=trainSmall, method="class")
p3 = predict(censusRF, newdata=Test)
p3
c =table(Test$over50k, p3)
(c[1,1]+c[2,2])/sum(c)

vu = varUsed(censusRF, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusRF$forest$xlevels[vusorted$ix]))
varImpPlot(censusRF)

set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
numFolds = trainControl(method="cv", number=10)
train(over50k ~ ., data=Train, method="rpart", trControl=numFolds, tuneGrid = cartGrid)
censusRF2 = rpart(over50k ~ ., data=Train, method="class", cp=0.002)
p3 = predict(censusRF2, newdata=Test, type="class")
c =table(Test$over50k, p3)
(c[1,1]+c[2,2])/sum(c)
prp(censusRF2)
summary(censusRF2)
