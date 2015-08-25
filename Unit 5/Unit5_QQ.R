tweetlog = glm(Negative ~ .,
               data = trainSparse,
               family = binomial)

predictions = predict(tweetlog, newdata=testSparse, type="response")

table(testSparse$Negative, predictions > 0.5)
(32+253)/(253+47+23+32)
