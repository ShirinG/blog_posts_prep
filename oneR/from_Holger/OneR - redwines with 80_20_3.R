# https://archive.ics.uci.edu/ml/datasets/Wine+Quality
# according to http://scialert.net/fulltext/?doi=jas.2012.598.601&org=11#t2
# ID3: 60, Naive Bayes: 58.8, SVM: 62.4
library(OneR)

data <- read.csv("redwines.csv")
data <- optbin(data)
model <- OneR(data, verbose = TRUE)
plot(model)
summary(model)
prediction <- predict(model, data)
eval_model(prediction, data)

##
set.seed(3)
random <- sample(1:nrow(data), 0.75 * nrow(data))
data.train <- optbin(data[random, ])
data.test <- data[-random, ]

model.train <- OneR(data.train, verbose = T)
plot(model.train)
summary(model.train)
prediction <- predict(model.train, data.test)
eval_model(prediction, data.test)


