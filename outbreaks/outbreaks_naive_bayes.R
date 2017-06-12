## Naive Bayes

https://eight2late.wordpress.com/2015/11/06/a-gentle-introduction-to-naive-bayes-classification-using-r/


  library(outbreaks)
fluH7N9.china.2013_backup <- fluH7N9.china.2013

fluH7N9.china.2013$age[which(fluH7N9.china.2013$age == "?")] <- NA
fluH7N9.china.2013$case.ID <- paste("case", fluH7N9.china.2013$case.ID, sep = "_")
library(dplyr)

dataset <- fluH7N9.china.2013 %>%
  mutate(hospital = as.factor(ifelse(is.na(date.of.hospitalisation), 0, 1)),
         gender_f = as.factor(ifelse(gender == "f", 1, 0)),
         province_Jiangsu = as.factor(ifelse(province == "Jiangsu", 1, 0)),
         province_Shanghai = as.factor(ifelse(province == "Shanghai", 1, 0)),
         province_Zhejiang = as.factor(ifelse(province == "Zhejiang", 1, 0)),
         province_other = as.factor(ifelse(province == "Zhejiang" | province == "Jiangsu" | province == "Shanghai", 0, 1)),
         days_onset_to_outcome = as.numeric(as.character(gsub(" days", "",
                                                              as.Date(as.character(date.of.outcome), format = "%Y-%m-%d") -
                                                                as.Date(as.character(date.of.onset), format = "%Y-%m-%d")))),
         days_onset_to_hospital = as.numeric(as.character(gsub(" days", "",
                                                               as.Date(as.character(date.of.hospitalisation), format = "%Y-%m-%d") -
                                                                 as.Date(as.character(date.of.onset), format = "%Y-%m-%d")))),
         age = as.numeric(as.character(age)),
         early_onset = as.factor(ifelse(date.of.onset < summary(fluH7N9.china.2013$date.of.onset)[[3]], 1, 0)),
         early_outcome = as.factor(ifelse(date.of.outcome < summary(fluH7N9.china.2013$date.of.outcome)[[3]], 1, 0))) %>%
  subset(select = -c(2:4, 6, 8))
rownames(dataset) <- dataset$case.ID
dataset <- dataset[, -1]
library(mice)

dataset_impute <- mice(dataset[, -1],  print = FALSE)
dataset_complete <- merge(dataset[, 1, drop = FALSE], complete(dataset_impute, 1), by = "row.names", all = TRUE)
rownames(dataset_complete) <- dataset_complete$Row.names
dataset_complete <- dataset_complete[, -1]

train_index <- which(is.na(dataset_complete$outcome))
train_data <- dataset_complete[-train_index, ]
test_data  <- dataset_complete[train_index, -1]

library(caret)
set.seed(27)
val_index <- createDataPartition(train_data$outcome, p = 0.7, list=FALSE)
val_train_data <- train_data[val_index, ]
val_test_data  <- train_data[-val_index, ]
val_train_X <- val_train_data[,-1]
val_test_X <- val_test_data[,-1]

###

library(e1071)
nb_model <- naiveBayes(outcome~.,data = val_train_data)
predict(nb_model, newdata = val_test_data[, -1], type="raw")

#…and the moment of reckoning
nb_test_predict <- predict(nb_model, newdata = val_test_data[, -1])
#confusion matrix
table(pred=nb_test_predict, true=val_test_data$outcome)

#fraction of correct predictions
mean(nb_test_predict==val_test_data$outcome)

###

# http://joshwalters.com/2012/11/27/naive-bayes-classification-in-r.html

library("klaR")

set.seed(27)
model_nb <- caret::train(outcome ~ .,
                         data = train_data,
                         method = "nb",
                         preProcess = NULL,
                         trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE))
