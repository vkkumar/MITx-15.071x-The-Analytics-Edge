setwd("~/Dropbox/EdX/The Analytics Edge/Final/Movies")
Movies <- read.csv("Movies.csv", stringsAsFactors=FALSE)

str(Movies)
summary(Movies)

MoviesTrain = Movies[ Movies$Year < 2010, ]
MoviesTest  = Movies[!Movies$Year < 2010, ]

# Build the linear regression model
MoviesLM = lm(Worldwide ~ ., data = MoviesTrain[ , 3:ncol(MoviesTrain)])
summary(MoviesLM)

# Correlation between Worldwide and Production.Budget
cor(MoviesTrain$Worldwide, MoviesTrain$Production.Budget)

# New model with only significant predictors
MoviesLM = lm(Worldwide ~ Runtime + Crime + Horror + Animation + History +
                Nominations + Production.Budget,
              data = MoviesTrain[ , 3:ncol(MoviesTrain)])
summary(MoviesLM)

# Make predictions based on the linear model
MoviesPred = predict(MoviesLM, newdata = MoviesTest)
plot(MoviesTest$Worldwide ~ MoviesPred)

# Sum of Squared Errors (SSE) on the test set
SSE = sum((MoviesTest$Worldwide - MoviesPred)^2)
SSE

# Total Sum of Squares (SST) on the test set (Use mean of the Worldwide on the 
# training set as baseline)
SST = sum((MoviesTest$Worldwide - mean(MoviesTrain$Worldwide))^2)
SST

# R-squared calculation
1 -(SSE/SST)

#_______________________________________________________________________________
# Load libraries for building CART model
library(rpart)
library(rpart.plot)
library(caTools)
library(rattle)

# Add a Performance variable based on Worldwide variable
Movies$Performance = factor(ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .75), "Excellent",
                            ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .25), "Average",
                                   "Poor")))
table(Movies$Performance)
Movies$Performance = as.factor(Movies$Performance)
Movies$Rated = as.factor(Movies$Rated)

# Remove Worldwide variable
Movies$Worldwide = NULL

# Split Training and Testing datasets. Use sample.split function on Performance
set.seed(15071)
spl = sample.split(Movies$Performance, SplitRatio = 0.70)
MoviesTrain = subset(Movies, spl == TRUE)
MoviesTest  = subset(Movies, spl == FALSE)

# Build CART model
MoviesCART = rpart(Performance ~ .,
                   data = MoviesTrain[ , 3:ncol(MoviesTrain)],
                   method = "class")
fancyRpartPlot(MoviesCART, sub = "")

# Predict on the training set
MoviesPred = predict(MoviesCART,
                     newdata = MoviesTrain[ ,3:ncol(MoviesTrain)],
                     type = "class")
table(MoviesTrain$Performance, MoviesPred)
(96+41+46)/nrow(MoviesTrain)

# Accuracy of baseline model on training set
table(MoviesTrain$Performance)
116/nrow(MoviesTrain)

# Predict on the testing set
MoviesPred = predict(MoviesCART,
                     newdata = MoviesTest[ ,3:ncol(MoviesTest)],
                     type = "class")
table(MoviesTest$Performance, MoviesPred)
(36+16+16)/nrow(MoviesTest)

# Accuracy of baseline model on testing set
table(MoviesTest$Performance)
50/nrow(MoviesTest)
