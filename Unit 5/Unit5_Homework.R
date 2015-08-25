# For sample splits
library(caTools)
# For Cross Validation
library(caret)
library(e1071)
# For CART Models
library(rpart)
library(rpart.plot)
# For Random Forests
library(randomForest)
# For ROC plots
library(ROCR)
# For Language Processing
library(NLP) # Natural Language Processing
library(tm) # Text mining package
library(SnowballC)

wiki = read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 5/wiki.csv", stringsAsFactors = FALSE)
setwd("~/Dropbox/EdX/The Analytics Edge/Unit 5")

str(wiki)
table(wiki$Vandal)
wiki$Vandal = as.factor(wiki$Vandal)
str(wiki)

# Prepare for column "Added"
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded[[1]]


length(stopwords('english')) # Returns 174
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english")) # Remove stopwords
corpusAdded = tm_map(corpusAdded, stemDocument) # Stem the words
corpusAdded = tm_map(corpusAdded, PlainTextDocument) # Convert to Plain text
dtmAdded = DocumentTermMatrix(corpusAdded) # Document term matrix
dtmAdded

# Sparsity
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# Create data frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

# Prepare for column "Removed"
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved[[1]]
# Remove stopwords
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
# Stem the words
corpusRemoved = tm_map(corpusRemoved, stemDocument)
# Convert to Plain Text
corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)
# Document term matrix
dtmRemoved = DocumentTermMatrix(corpusRemoved)
# Sparsity
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
# Create data frame
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)
wikiWords = cbind(wordsAdded, wordsRemoved)

# Add dependent variable
wikiWords$Vandal = wiki$Vandal
set.seed(123)
library(caTools)

spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

# Baseline accuracy
table(train$Vandal)
1443/(1443+1270)

# Build a CART model
library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~ ., data = train, method="class")
prp(wikiCART)
predictCART = predict(wikiCART, newdata = test, type="class")
table(test$Vandal, predictCART)
(618+12)/(618+533+0+12)

grepl("cat","dogs and cats",fixed=TRUE) # TRUE

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART2 = rpart(Vandal ~ ., data = wikiTrain2, method = "class")
prp(wikiCART2)
predCART2 = predict(wikiCART2, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal, predCART2)
(609+57)/nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

train = subset(wikiWords2, spl == TRUE)
test = subset(wikiWords2, spl == FALSE)

wikiCART3 = rpart(Vandal ~ ., data = train, method = "class")
prp(wikiCART3)
predCART3 = predict(wikiCART3, newdata = test, type = "class")
table(test$Vandal, predCART3)
(514+248)/nrow(test)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
train = subset(wikiWords3, spl == TRUE)
test = subset(wikiWords3, spl == FALSE)
wikiCART4 = rpart(Vandal ~ ., data = train, method = "class")
prp(wikiCART4)
predCART4 = predict(wikiCART4, newdata = test, type = "class")
table(test$Vandal, predCART4)
(595+241)/nrow(test)

# Homework 2
trials <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 5/clinical_trial.csv", stringsAsFactors=FALSE)
summary(trials)
str(trials)


nchar(trials$abstract[which.max(nchar(trials$abstract))])
sum(nchar(trials$abstract) == 0)
trials$title[which.min(nchar(trials$title))]

# 1) Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.

corpusTitle    = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

# 2) Convert corpusTitle and corpusAbstract to lowercase. After performing this 
# step, remember to run the lines:

corpusTitle    = tm_map(corpusTitle,    tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
  
# 3) Remove the punctuation in corpusTitle and corpusAbstract.

corpusTitle    = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

# 4) Remove the English language stop words from corpusTitle and corpusAbstract.

corpusTitle    = tm_map(corpusTitle, removeWords,  stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords,  stopwords("english"))

# 5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).

corpusTitle    = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

# 6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# 7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% 
# (aka terms that appear in at least 5% of documents).

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmTitle
dtmAbstract

# 8) Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and dtmAbstract).

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

which.max(colSums(dtmAbstract))

# dtmTitle and dtmAbstract into a single data frame to make predictions
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
ncol(dtm)
set.seed(144)
spl = sample.split(dtm$trial, SplitRatio = 0.70)
train = subset(dtm, spl)
test = subset(dtm, !spl)

table(train$trial)
730/nrow(train)

str(trials)
str(dtm)

trialCART = rpart(trial ~ ., data = train, method = "class")
prp(trialCART)
ptrialCART = predict(trialCART, train)[,2]
max(ptrialCART)

cT = table(train$trial, ptrialCART >= 0.5)
# Accuracy 
(cT[1,1]+cT[2,2])/nrow(train)
# Sensitivity = True Positive Rate = TP/P = TP/(TP+FN)
cT[2,2]/(cT[2,1]+cT[2,2])
# Specificity = True Negative Rate = TN/N = TN/(TN+FP)
cT[1,1]/(cT[1,1]+cT[1,2])

ptrialCART2 = predict(trialCART, newdata = test)[,2]
ct = table(test$trial, ptrialCART2 >= 0.5)
ct
(ct[1,1]+ct[2,2])/nrow(test)


predCART2 = prediction(ptrialCART2, test$trial)
perfCART2 = performance(predCART2, "tpr", "fpr")
plot(perfCART2, colorize=TRUE)

# Compute AUC
performance(predCART2, "auc")@y.values
perfCART2

# Homework 3
emails <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 5/emails.csv", stringsAsFactors=FALSE)
str(emails )
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
which.max(colSums(emailsSparse))
emailsSparse$spam = emails$spam

sum(colSums(emailsSparse[emailsSparse$spam==0,names(emailsSparse) !="spam"])>=5000)
sort(colSums(subset(emailsSparse, spam == 0)))

sum(colSums(emailsSparse[emailsSparse$spam==1,names(emailsSparse) !="spam"])>=1000)

emailsSparse$spam = as.factor(emailsSparse$spam)

set.seed(123)
spl = sample.split(emailsSparse$spam, SplitRatio = 0.70)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

spamLog = glm(spam ~ ., data = train, family = "binomial")
spamCART = rpart(spam ~ ., data = train, method = "class")
set.seed(123)
spamRF = randomForest(spam ~., data = train, method = "class")


