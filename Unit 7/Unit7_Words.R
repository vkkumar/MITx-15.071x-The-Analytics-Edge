rm(list = ls())
setwd("~/Dropbox/EdX/The Analytics Edge/Unit 7/")
tweets <- read.csv("tweets.csv", stringsAsFactors=FALSE)
View(tweets)

library(wordcloud)
library(tm) # Text mining package

# 1) Create a corpus using the Tweet variable
# 2) Convert the corpus to lowercase (don't forget to type "corpus = 
# tm_map(corpus, PlainTextDocument)" in your R console right after this step)
# 3) Remove punctuation from the corpus
# 4) Remove all English-language stopwords
# 5) Build a document-term matrix out of the corpus
# 6) Convert the document-term matrix to a data frame called allTweets

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords,  stopwords("english"))

dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))

str(allTweets)
dim(allTweets)


?wordcloud
wordcloud(colnames(allTweets),colSums(allTweets),
          scale=c(2, 0.25),
          min.freq=3,
          max.words=Inf,
          random.order=TRUE, random.color=FALSE, rot.per=.1,
          colors="black", ordered.colors=FALSE, use.r.layout=FALSE,
          fixed.asp=TRUE)

wordcloud(colnames(allTweets),
          colSums(allTweets),
          scale=c(2, 0.25))

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))

display.brewer.all() 

wordcloud(colnames(allTweets),
          colSums(allTweets),
          scale=c(2, 0.25))

wordcloud(colnames(allTweets),colSums(allTweets),
          scale=c(2, 0.25),
          min.freq=10)

wordcloud(colnames(allTweets),colSums(allTweets),
          scale=c(2, 0.25),
          random.order = FALSE)

wordcloud(colnames(allTweets),colSums(allTweets),
          scale=c(2, 0.25),
          min.freq=10,
          rot.per = 0)

wordcloud(colnames(allTweets),colSums(allTweets),
          scale=c(2, 0.25),
          min.freq=10,
          rot.per = 0.1,
          colors = 'purple',
          random.color = FALSE)

wordcloud(colnames(allTweets),colSums(allTweets),
          scale=c(2, 0.25),
          min.freq=5,
          rot.per = 0.1,
          colors = brewer.pal(9, "Set1"),
          random.color = FALSE)