install.packages("outbreaks")

library(outbreaks)

# https://mran.microsoft.com/web/packages/outbreaks/outbreaks.pdf


head(ebola.kikwit.1995)


str(ebola.sim)
head(ebola.sim$linelist)
head(ebola.sim$contacts)

str(ebola.sim.clean)

## identify mistakes in data entry (negative incubation period)
mistakes <- which(ebola.sim$linelist$date.of.onset <= ebola.sim$linelist$date.of.infection)
mistakes
ebola.sim$linelist[mistakes, ]
## check that ebola.sim.clean is identical after removing mistakes
identical(ebola.sim.clean$linelist, ebola.sim$linelist[-mistakes, ])


head(fluH7N9.china.2013)


head(influenza.england.1978.school)


head(measles.hagelloch.1861)


head(sars.canada.2003)


head(smallpox.abakaliki.1967)

#----

library(dplyr)

dataset <- fluH7N9.china.2013 %>%
  subset(!is.na(outcome)) %>%
  mutate(hospital = ifelse(is.na(date.of.hospitalisation), 0, 1),
         gender_f = ifelse(gender == "f", 1, 0),
         province_Jiangsu = ifelse(province == "Jiangsu", 1, 0),
         province_Shanghai = ifelse(province == "Shanghai", 1, 0),
         province_Zhejiang = ifelse(province == "Zhejiang", 1, 0),
         province_other = ifelse(province == "Zhejiang" | province == "Jiangsu" | province == "Shanghai", 0, 1),
         days_onset_to_outcome = as.numeric(as.character(gsub(" days", "",
                                      as.Date(as.character(date.of.outcome), format = "%Y-%m-%d") - as.Date(as.character(date.of.onset), format = "%Y-%m-%d")))),
         age = as.numeric(as.character(age))) %>%
  subset(select = -c(2:4, 6, 8))

head(dataset)
summary(dataset$outcome)
str(dataset)

nrow(dataset)

dataset <- dataset[!apply(dataset, 1, function(x) any(is.na(x))), ]

library(caret)


library(tidyr)

dataset_gather <- dataset %>%
  subset(!is.na(days_onset_to_outcome)) %>%
  gather(key = measure, value = value, age:days_onset_to_outcome)

head(dataset_gather)
tail(dataset_gather)

ggplot(data = dataset_gather, aes(x = case.ID, y = value, fill = outcome)) +
  geom_bar(stat = "identity") +
  facet_wrap(measure ~ outcome, ncol = 2, scales = "free_y")


library(rpart)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

set.seed(27)
fit <- rpart(outcome ~ .,
                    data = dataset[, -1],
                    method="class",
                    #control=rpart.control(minsplit=5, cp=0)
                    control = rpart.control(xval = 10, minbucket = 2, cp = 0), parms = list(split = 'information')) # or gini

fancyRpartPlot(fit)


# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=10)
# train the model
set.seed(27)
model <- train(outcome~., data=dataset[, -1], method="rf", preProcess=NULL, trControl=control)
# estimate variable importance
importance <- varImp(model, scale=TRUE)

plot(importance)


set.seed(27)
trainIndex <- createDataPartition(dataset$outcome, p = 0.5, list=FALSE)
trainData <- dataset[trainIndex, -1]
testData  <- dataset[-trainIndex, -1]
trainX <- trainData[,-1]
testX <- testData[,-1]
y = trainData$outcome


library(AppliedPredictiveModeling)

transparentTheme(trans = .9)
featurePlot(x = trainX,
            y = y,
            plot = "density",
            ## Pass in options to xyplot() to
            ## make it prettier
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            adjust = 1.5,
            pch = "|",
            layout = c(2,4),
            auto.key = list(columns = 2))


set.seed(27)
model1 <- caret::train(outcome ~ .,
                             data = trainData,
                             method = "rf",
                             preProcess = c("scale", "center"),
                             trControl = trainControl(method = "boot", number = 1000))

model1

set.seed(27)
model2 <- caret::train(outcome ~ .,
                      data = trainData,
                      method = "rf",
                      preProcess = c("scale", "center"),
                      trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, verboseIter = TRUE))

set.seed(27)
model2b <- caret::train(outcome ~ .,
                       data = trainData,
                       method = "glmnet",
                       preProcess = c("scale", "center"),
                       trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, verboseIter = TRUE))

# Create a list of models
models <- list(rf = model2, glmnet = model2b)

# Resample the models
resampled <- resamples(models)

# Generate a summary
summary(resampled)

# Plot the differences between model fits
dotplot(resampled, metric = "Accuracy")


#1 (kknn) k-Nearest Neighbors
set.seed(12345)
m_kknn <- caret::train(outcome ~ .,
                       data = trainData,
                       method = "kknn",
                       preProcess = c("scale", "center"),
                       trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, verboseIter = TRUE))
print(m_kknn)

#2 (pda) Penalized Discriminant Analysis
set.seed(12345)
m_pda <- caret::train(outcome ~ .,
                      data = trainData,
                      method = "pda",
                      preProcess = c("scale", "center"),
                      trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, verboseIter = TRUE))
print(m_pda)

#3 sda (Shrinkage Discriminant Analysis)
set.seed(12345)
m_sda <- caret::train(outcome ~ .,
                      data = trainData,
                      method = "sda",
                      preProcess = c("scale", "center"),
                      trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, verboseIter = TRUE))
print(m_sda)

#4 (slda) Stabilized Linear Discriminant Analysis
set.seed(12345)
m_slda <- caret::train(outcome ~ .,
                       data = trainData,
                       method = "slda",
                       preProcess = c("scale", "center"),
                       trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, verboseIter = TRUE))
print(m_slda)

#5 (hdda) High Dimensional Discriminant Analysis
set.seed(12345)
m_hdda <- caret::train(outcome ~ .,
                       data = trainData,
                       method = "hdda",
                       preProcess = c("scale", "center"),
                       trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, verboseIter = TRUE))
print(m_hdda)

#6 (pam) Nearest Shrunken Centroids
set.seed(12345)
m_pam <- caret::train(outcome ~ .,
                      data = trainData,
                      method = "pam",
                      preProcess = c("scale", "center"),
                      trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, verboseIter = TRUE))
print(m_pam)

#7 C5.0Tree (Single C5.0 Tree)
set.seed(12345)
m_C5 <- caret::train(outcome ~ .,
                     data = trainData,
                     method = "C5.0Tree",
                     preProcess = c("scale", "center"),
                     trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, verboseIter = TRUE))
print(m_C5)

#8 CSimca (SIMCA) // has no logloss
set.seed(12345)
m_CSimca <- caret::train(outcome ~ .,
                         data = trainData,
                         method = "CSimca",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, verboseIter = TRUE))
print(m_CSimca)

#9 pls (Partial Least Squares)
set.seed(12345)
m_pls <- caret::train(outcome ~ .,
                      data = trainData,
                      method = "pls",
                      preProcess = c("scale", "center"),
                      trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, verboseIter = TRUE))
print(m_pls)

# calculate resamples // exclude SIMCA and PLS
resample_results <- resamples(list(KKNN=m_kknn,PDA=m_pda,SDA=m_sda,
                                   SLDA=m_slda, HDDA=m_hdda, PAM=m_pam, C5TREE=m_C5))
summary(resample_results,metric = c("Kappa","Accuracy"))

# plot Kappa values
densityplot(resample_results , metric = "Kappa" ,auto.key = list(columns = 3))

# plot Accuracy values
densityplot(resample_results , metric = "Accuracy" ,auto.key = list(columns = 3))

# plot all (higher is better)
bwplot(resample_results , metric = c("Kappa","Accuracy"))



confusionMatrix(predict(model, testX), testData$outcome)

results_rf <- data.frame(testData = as.character(testData$outcome),
                         randomForest = as.character(predict(model, newdata = testX, preProcess = c("center", "scale"))),
                         predict(model, newdata = testX, preProcess = c("center", "scale"), type="prob"))
results_rf$prediction <- ifelse(results_rf$Death > 0.7, "Death", ifelse(results_rf$Recover > 0.7, "Recover", "uncertain"))
results_rf$correct <- ifelse(results_rf$prediction == results_rf$testData, "yes", ifelse(results_rf$prediction == "uncertain", "unknown", "no"))
results_rf

###

# https://www.kaggle.com/apryor6/d/uciml/forest-cover-type-dataset/random-forests-predicting-forest-coverage

set.seed(27)
model <- randomForest(outcome ~ .,
                      data = trainData,
                      ntree=200,
                      importance=TRUE)

confusionMatrix(predict(model, testX), testData$outcome)

results_rf <- data.frame(testData = as.character(testData$outcome),
                         randomForest = as.character(predict(model, newdata = testX)),
                         predict(model, newdata = testX, type="prob"))
results_rf$prediction <- ifelse(results_rf$Death > 0.7, "Death", ifelse(results_rf$Recover > 0.7, "Recover", "uncertain"))
results_rf$correct <- ifelse(results_rf$prediction == results_rf$testData, "yes", ifelse(results_rf$prediction == "uncertain", "unknown", "no"))
results_rf

###

