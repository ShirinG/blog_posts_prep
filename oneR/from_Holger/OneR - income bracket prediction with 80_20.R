# http://sci2s.ugr.es/keel/dataset.php?cod=163
data <- read.csv("marketing1.dat")
data_names <- names(data)
data <- cbind(data[-ncol(data)], factor(data$Income))
names(data) <- data_names
set.seed(12) # for reproducibility
random <- sample(1:nrow(data), 0.8 * nrow(data))
data_train <- data[random, ]
data_test <- data[-random, ]

library(OneR)
data <- optbin(data_train)
model <- OneR(data, verbose = TRUE)
summary(model)
plot(model)
prediction <- predict(model, data_test)
eval_model(prediction, data_test)

library(rpart)
library(rpart.plot)
model <- rpart(Income ~., data = data_train)
rpart.plot(model, type = 3, extra = 0, box.palette = "Grays")
prediction <- predict(model, data_test, type = "class")
eval_model(prediction, data_test)

library(randomForest)
set.seed(4543)
model <- randomForest(Income ~., data = data_train, importance = TRUE)
varImpPlot(model)
prediction <- predict(model, data_test)
eval_model(prediction, data_test)
