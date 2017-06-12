library(OneR)
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/tic-tac-toe/tic-tac-toe.data", header = FALSE)
names(data) <- c("top-left-square", "top-middle-square", "top-right-square", "middle-left-square", "middle-middle-square", "middle-right-square", "bottom-left-square", "bottom-middle-square", "bottom-right-square", "Class")

model <- OneR(data, verbose = T)
summary(model)
prediction <- predict(model, data)
eval_model(prediction, data)
