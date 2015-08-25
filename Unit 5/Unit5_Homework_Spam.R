library(caTools)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(tm) # Text mining package
library(SnowballC)

emails <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 5/emails.csv", stringsAsFactors=FALSE)
str(emails)
summary(emails)
table(emails$spam)

nchar(emails$text[which.max(nchar(emails$text))])
which.min(nchar(emails$text))

# 1) Build a new corpus variable called corpus.
# 2) Using tm_map, convert the text to lowercase.
# 3) Using tm_map, remove all punctuation from the corpus.
# 4) Using tm_map, remove all English stopwords from the corpus.
# 5) Using tm_map, stem the words in the corpus.
# 6) Build a document term matrix from the corpus, called dtm.

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords,  stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm
spdtm = removeSparseTerms(dtm, 0.95)
spdtm
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
colnames(emailsSparse)
which.max(colSums(emailsSparse))
emailsSparse$spam = emails$spam

# How many word stems appear at least 5000 times in the "ham" emails in the dataset?
# Hint: in this and the next question, remember not to count the dependent variable
# we just added.


sort(colSums(subset(emailsSparse, spam == 0))) # spam == 0 => ham!
colSums(emailsSparse[emailsSparse$spam == 0, names(emailsSparse) != 'spam']) >= 5000
sum(colSums(emailsSparse[emailsSparse$spam == 0,names(emailsSparse) !="spam"]) >= 5000)

sort(colSums(subset(emailsSparse, spam == 1))) # spam == 1 => spam!
sum(colSums(emailsSparse[emailsSparse$spam == 1, names(emailsSparse) !="spam"]) >= 1000)

emailsSparse$spam = as.factor(emailsSparse$spam)

set.seed(123)
spl = sample.split(emailsSparse$spam, SplitRatio = 0.70)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

spamLog = glm(spam ~ ., data = train, family = "binomial")
set.seed(123)
spamCART = rpart(spam ~ ., data = train, method = "class")
set.seed(123)
spamRF = randomForest(spam ~., data = train, method = "class")

summary(spamLog)
predLog = predict(spamLog, type = 'response')
sum(predLog < 0.00001)
sum(predLog > 0.99999)
sum(predLog > 0.00001 & predLog < 0.99999)
table(train$spam, predLog > 0.5)
(3052+954)/nrow(train)

predROCR = prediction(predLog, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

predCART = predict(spamCART)[ ,2]
table(train$spam, predCART >= 0.5)
(2885 + 894)/nrow(train)

predROCR = prediction(predCART, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

predRF = predict(spamRF, type = 'prob')[ ,2]
table(train$spam, predRF >= 0.5)
(3013+914)/nrow(train)

predROCR = prediction(predRF, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

predLog1 = predict(spamLog, newdata = test, type = 'response')
table(test$spam, predLog1 >= 0.5)
(1257+376)/nrow(test)
