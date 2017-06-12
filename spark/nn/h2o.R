# Introductory H2O Machine Learning Tutorial
# Prepared for H2O Open Chicago 2016: http://open.h2o.ai/chicago.html


# First step is to download & install the h2o R library
# The latest version is always here: http://www.h2o.ai/download/h2o/r

# Load the H2O library and start up the H2O cluster locally on your machine
library(h2o)
h2o.init(nthreads = -1, #Number of threads -1 means use all cores on your machine
         max_mem_size = "8G")  #max mem size is the maximum memory to allocate to H2O



# Next we will import a cleaned up version of the Lending Club "Bad Loans" dataset
# The purpose here is to predict whether a loan will be bad (not repaid to the lender)
# The response column, bad_loan, is 1 if the loan was bad, and 0 otherwise

# Import the data
loan_csv <- "/Volumes/H2OTOUR/loan.csv"  # modify this for your machine
# Alternatively, you can import the data directly from a URL
#loan_csv <- "https://raw.githubusercontent.com/h2oai/app-consumer-loan/master/data/loan.csv"
data <- h2o.importFile(loan_csv)  # 163,987 rows x 15 columns
dim(data)
# [1] 163987     15

# Since we want to train a binary classification model,
# we must ensure that the response is coded as a factor
# If the response is 0/1, H2O will assume it's numeric,
# which means that H2O will train a regression model instead
data$bad_loan <- as.factor(data$bad_loan)  #encode the binary repsonse as a factor
h2o.levels(data$bad_loan)  #optional: after encoding, this shows the two factor levels, '0' and '1'
# [1] "0" "1"

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = data,
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 1)  #setting a seed will guarantee reproducibility
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

# Take a look at the size of each partition
# Notice that h2o.splitFrame uses approximate splitting not exact splitting (for efficiency)
# so these are not exactly 70%, 15% and 15% of the total rows
nrow(train)  # 114908
nrow(valid) # 24498
nrow(test)  # 24581

# Identify response and predictor variables
y <- "bad_loan"
x <- setdiff(names(data), c(y, "int_rate"))  #remove the interest rate column because it's correlated with the outcome
print(x)
# [1] "loan_amnt"             "term"
# [3] "emp_length"            "home_ownership"
# [5] "annual_inc"            "verification_status"
# [7] "purpose"               "addr_state"
# [9] "dti"                   "delinq_2yrs"
# [11] "revol_util"            "total_acc"
# [13] "longest_credit_length"



# Now that we have prepared the data, we can train some models
# We will start by training a single model from each of the H2O supervised algos:
# 1. Generalized Linear Model (GLM)
# 2. Random Forest (RF)
# 3. Gradient Boosting Machine (GBM)
# 4. Deep Learning (DL)
# 5. Naive Bayes (NB)


# 1. Let's start with a basic binomial Generalized Linear Model
# By default, h2o.glm uses a regularized, elastic net model
glm_fit1 <- h2o.glm(x = x,
                    y = y,
                    training_frame = train,
                    model_id = "glm_fit1",
                    family = "binomial")  #similar to R's glm, h2o.glm has the family argument

# Next we will do some automatic tuning by passing in a validation frame and setting
# `lambda_search = True`.  Since we are training a GLM with regularization, we should
# try to find the right amount of regularization (to avoid overfitting).  The model
# parameter, `lambda`, controls the amount of regularization in a GLM model and we can
# find the optimal value for `lambda` automatically by setting `lambda_search = TRUE`
# and passing in a validation frame (which is used to evaluate model performance using a
# particular value of lambda).
glm_fit2 <- h2o.glm(x = x,
                    y = y,
                    training_frame = train,
                    model_id = "glm_fit2",
                    validation_frame = valid,
                    family = "binomial",
                    lambda_search = TRUE)

# Let's compare the performance of the two GLMs
glm_perf1 <- h2o.performance(model = glm_fit1,
                             newdata = test)
glm_perf2 <- h2o.performance(model = glm_fit2,
                             newdata = test)

# Print model performance
glm_perf1
glm_perf2

# Instead of printing the entire model performance metrics object,
# it is probably easier to print just the metric that you are interested in comparing.
# Retreive test set AUC
h2o.auc(glm_perf1)  #0.677449084114
h2o.auc(glm_perf2)  #0.677675858276



# Compare test AUC to the training AUC and validation AUC
h2o.auc(glm_fit2, train = TRUE)  #0.674306164325
h2o.auc(glm_fit2, valid = TRUE)  #0.675512216705
glm_fit2@model$validation_metrics  #0.675512216705





# 2. Random Forest
# H2O's Random Forest (RF) implements a distributed version of the standard
# Random Forest algorithm and variable importance measures.
# First we will train a basic Random Forest model with default parameters.
# The Random Forest model will infer the response distribution from the response encoding.
# A seed is required for reproducibility.
rf_fit1 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "rf_fit1",
                            seed = 1)

# Next we will increase the number of trees used in the forest by setting `ntrees = 100`.
# The default number of trees in an H2O Random Forest is 50, so this RF will be twice as
# big as the default.  Usually increasing the number of trees in a RF will increase
# performance as well.  Unlike Gradient Boosting Machines (GBMs), Random Forests are fairly
# resistant (although not free from) overfitting.
# See the GBM example below for additional guidance on preventing overfitting using H2O's
# early stopping functionality.
rf_fit2 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "rf_fit2",
                            #validation_frame = valid,  #only used if stopping_rounds > 0
                            ntrees = 100,
                            seed = 1)

# Let's compare the performance of the two RFs
rf_perf1 <- h2o.performance(model = rf_fit1,
                            newdata = test)
rf_perf2 <- h2o.performance(model = rf_fit2,
                            newdata = test)

# Print model performance
rf_perf1
rf_perf2

# Retreive test set AUC
h2o.auc(rf_perf1)  # 0.662266990734
h2o.auc(rf_perf2)  # 0.66525468051


# Cross-validate performance
# Rather than using held-out test set to evaluate model performance, a user may wish
# to estimate model performance using cross-validation. Using the RF algorithm
# (with default model parameters) as an example, we demonstrate how to perform k-fold
# cross-validation using H2O. No custom code or loops are required, you simply specify
# the number of desired folds in the nfolds argument.
# Since we are not going to use a test set here, we can use the original (full) dataset,
# which we called data rather than the subsampled `train` dataset. Note that this will
# take approximately k (nfolds) times longer than training a single RF model, since it
# will train k models in the cross-validation process (trained on n(k-1)/k rows), in
# addition to the final model trained on the full training_frame dataset with n rows.

rf_fit3 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "rf_fit3",
                            seed = 1,
                            nfolds = 5)

# To evaluate the cross-validated AUC, do the following:
h2o.auc(rf_fit3, xval = TRUE)  # 0.661201482614




# 3. Gradient Boosting Machine
# H2O's Gradient Boosting Machine (GBM) offers a Stochastic GBM, which can
# increase performance quite a bit compared to the original GBM implementation.

# Now we will train a basic GBM model
# The GBM model will infer the response distribution from the response encoding if not specified
# explicitly through the `distribution` argument. A seed is required for reproducibility.
gbm_fit1 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train,
                    model_id = "gbm_fit1",
                    seed = 1)

# Next we will increase the number of trees used in the GBM by setting `ntrees=500`.
# The default number of trees in an H2O GBM is 50, so this GBM will trained using ten times
# the default.  Increasing the number of trees in a GBM is one way to increase performance
# of the model, however, you have to be careful not to overfit your model to the training data
# by using too many trees.  To automatically find the optimal number of trees, you must use
# H2O's early stopping functionality.  This example will not do that, however, the following
# example will.
gbm_fit2 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train,
                    model_id = "gbm_fit2",
                    #validation_frame = valid,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    seed = 1)

# We will again set `ntrees = 500`, however, this time we will use early stopping in order to
# prevent overfitting (from too many trees).  All of H2O's algorithms have early stopping available,
# however early stopping is not enabled by default (with the exception of Deep Learning).
# There are several parameters that should be used to control early stopping.  The three that are
# common to all the algorithms are: `stopping_rounds`, `stopping_metric` and `stopping_tolerance`.
# The stopping metric is the metric by which you'd like to measure performance, and so we will choose
# AUC here.  The `score_tree_interval` is a parameter specific to the Random Forest model and the GBM.
# Setting `score_tree_interval = 5` will score the model after every five trees.  The parameters we
# have set below specify that the model will stop training after there have been three scoring intervals
# where the AUC has not increased more than 0.0005.  Since we have specified a validation frame,
# the stopping tolerance will be computed on validation AUC rather than training AUC.
gbm_fit3 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train,
                    model_id = "gbm_fit3",
                    validation_frame = valid,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    score_tree_interval = 5,      #used for early stopping
                    stopping_rounds = 3,          #used for early stopping
                    stopping_metric = "AUC",      #used for early stopping
                    stopping_tolerance = 0.0005,  #used for early stopping
                    seed = 1)


# Let's compare the performance of the two GBMs
gbm_perf1 <- h2o.performance(model = gbm_fit1,
                             newdata = test)
gbm_perf2 <- h2o.performance(model = gbm_fit2,
                             newdata = test)
gbm_perf3 <- h2o.performance(model = gbm_fit3,
                             newdata = test)

# Print model performance
gbm_perf1
gbm_perf2
gbm_perf3

# Retreive test set AUC
h2o.auc(gbm_perf1)  # 0.682765594191
h2o.auc(gbm_perf2)  # 0.671854616713
h2o.auc(gbm_perf3)  # 0.68309902855


# To examine the scoring history, use the `scoring_history` method on a trained model.
# If `score_tree_interval` is not specified, it will score at various intervals, as we can
# see for `h2o.scoreHistory()` below.  However, regular 5-tree intervals are used
# for `h2o.scoreHistory()`.
# The `gbm_fit2` was trained only using a training set (no validation set), so the scoring
# history is calculated for training set performance metrics only.

h2o.scoreHistory(gbm_fit2)


# When early stopping is used, we see that training stopped at 105 trees instead of the full 500.
# Since we used a validation set in `gbm_fit3`, both training and validation performance metrics
# are stored in the scoring history object.  Take a look at the validation AUC to observe that the
# correct stopping tolerance was enforced.

h2o.scoreHistory(gbm_fit3)




# Look at scoring history for third GBM model
plot(gbm_fit3,
     timestep = "number_of_trees",
     metric = "AUC")
plot(gbm_fit3,
     timestep = "number_of_trees",
     metric = "logloss")




# 4. Deep Learning
# H2O's Deep Learning algorithm is a multilayer feed-forward artificial neural network.
# It can also be used to train an autoencoder. In this example we will train
# a standard supervised prediction model.

# Train a default DL
# First we will train a basic DL model with default parameters. The DL model will infer the response
# distribution from the response encoding if it is not specified explicitly through the `distribution`
# argument.  H2O's DL will not be reproducible if it is run on more than a single core, so in this example,
# the performance metrics below may vary slightly from what you see on your machine.
# In H2O's DL, early stopping is enabled by default, so below, it will use the training set and
# default stopping parameters to perform early stopping.
dl_fit1 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit1",
                            seed = 1)

# Train a DL with new architecture and more epochs.
# Next we will increase the number of epochs used in the GBM by setting `epochs=20` (the default is 10).
# Increasing the number of epochs in a deep neural net may increase performance of the model, however,
# you have to be careful not to overfit your model to your training data.  To automatically find the optimal number of epochs,
# you must use H2O's early stopping functionality.  Unlike the rest of the H2O algorithms, H2O's DL will
# use early stopping by default, so for comparison we will first turn off early stopping.  We do this in the next example
# by setting `stopping_rounds=0`.
dl_fit2 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit2",
                            #validation_frame = valid,  #only used if stopping_rounds > 0
                            epochs = 20,
                            hidden= c(10,10),
                            stopping_rounds = 0,  # disable early stopping
                            seed = 1)

# Train a DL with early stopping
# This example will use the same model parameters as `dl_fit2`. This time, we will turn on
# early stopping and specify the stopping criterion.  We will also pass a validation set, as is
# recommended for early stopping.
dl_fit3 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit3",
                            validation_frame = valid,  #in DL, early stopping is on by default
                            epochs = 20,
                            hidden = c(10,10),
                            score_interval = 1,           #used for early stopping
                            stopping_rounds = 3,          #used for early stopping
                            stopping_metric = "AUC",      #used for early stopping
                            stopping_tolerance = 0.0005,  #used for early stopping
                            seed = 1)


# Let's compare the performance of the three DL models
dl_perf1 <- h2o.performance(model = dl_fit1,
                            newdata = test)
dl_perf2 <- h2o.performance(model = dl_fit2,
                            newdata = test)
dl_perf3 <- h2o.performance(model = dl_fit3,
                            newdata = test)

# Print model performance
dl_perf1
dl_perf2
dl_perf3

# Retreive test set AUC
h2o.auc(dl_perf1)  # 0.6774335
h2o.auc(dl_perf2)  # 0.678446
h2o.auc(dl_perf3)  # 0.6770498

# Scoring history
h2o.scoreHistory(dl_fit3)
# Scoring History:
#   timestamp   duration  training_speed   epochs
# 1 2016-05-03 10:33:29  0.000 sec                  0.00000
# 2 2016-05-03 10:33:29  0.347 sec 424697 rows/sec  0.86851
# 3 2016-05-03 10:33:30  1.356 sec 601925 rows/sec  6.09185
# 4 2016-05-03 10:33:31  2.348 sec 717617 rows/sec 13.05168
# 5 2016-05-03 10:33:32  3.281 sec 777538 rows/sec 20.00783
# 6 2016-05-03 10:33:32  3.345 sec 777275 rows/sec 20.00783
# iterations        samples training_MSE training_r2
# 1          0       0.000000
# 2          1   99804.000000      0.14402     0.03691
# 3          7  700039.000000      0.14157     0.05333
# 4         15 1499821.000000      0.14033     0.06159
# 5         23 2299180.000000      0.14079     0.05853
# 6         23 2299180.000000      0.14157     0.05333
# training_logloss training_AUC training_lift
# 1
# 2          0.45930      0.66685       2.20727
# 3          0.45220      0.68133       2.59354
# 4          0.44710      0.67993       2.70390
# 5          0.45100      0.68192       2.81426
# 6          0.45220      0.68133       2.59354
# training_classification_error validation_MSE validation_r2
# 1
# 2                       0.36145        0.14682       0.03426
# 3                       0.33647        0.14500       0.04619
# 4                       0.37126        0.14411       0.05204
# 5                       0.32868        0.14474       0.04793
# 6                       0.33647        0.14500       0.04619
# validation_logloss validation_AUC validation_lift
# 1
# 2            0.46692        0.66582         2.53209
# 3            0.46256        0.67354         2.64124
# 4            0.45789        0.66986         2.44478
# 5            0.46292        0.67117         2.70672
# 6            0.46256        0.67354         2.64124
# validation_classification_error
# 1
# 2                         0.37197
# 3                         0.34716
# 4                         0.34385
# 5                         0.36544
# 6                         0.34716


# Look at scoring history for third DL model
plot(dl_fit3,
     timestep = "epochs",
     metric = "AUC")





# 5. Naive Bayes model
# The Naive Bayes (NB) algorithm does not usually beat an algorithm like a Random Forest
# or GBM, however it is still a popular algorithm, especially in the text domain (when your
# input is text encoded as "Bag of Words", for example).  The Naive Bayes algorithm is for
# binary or multiclass classification problems only, not regression.  Therefore, your response
# must be a factor instead of a numeric.

# First we will train a basic NB model with default parameters.
nb_fit1 <- h2o.naiveBayes(x = x,
                          y = y,
                          training_frame = train,
                          model_id = "nb_fit1")

# Train a NB model with Laplace Smoothing
# One of the few tunable model parameters for the Naive Bayes algorithm is the amount of Laplace
# smoothing. The H2O Naive Bayes model will not use any Laplace smoothing by default.
nb_fit2 <- h2o.naiveBayes(x = x,
                          y = y,
                          training_frame = train,
                          model_id = "nb_fit2",
                          laplace = 6)

# Let's compare the performance of the two NB models
nb_perf1 <- h2o.performance(model = nb_fit1,
                            newdata = test)
nb_perf2 <- h2o.performance(model = nb_fit2,
                            newdata = test)

# Print model performance
nb_perf1
nb_perf2

# Retreive test set AUC
h2o.auc(nb_perf1)  # 0.6488014
h2o.auc(nb_perf2)  # 0.6490678

#####################################

###################################################################################
### Goal: demonstrate usage of H2O's GLM algorithm

### Note: If run from plain R, execute R in the directory of this script. If run from RStudio,
### be sure to setwd() to the location of this script. h2o.init() starts H2O in R's current
### working directory. h2o.importFile() looks for files from the perspective of where H2O was
### started.


# convenience function to cut a numeric column into intervals, creating a new categorical variable
# uses h2o.hist to come up with interval boundaries
# uses h2o.cut to do the split
# data is list(Train=training frame,Valid (optional) = validation frame, Test (optional) = test frame)
# the operation is performed on all three dataset (the intervals are computed on training)
cut_column <- function(data, col) {
  # need lower/upper bound due to h2o.cut behavior (points < the first break or > the last break are replaced with missing value)
  min_val = min(data$Train[,col])-1
  max_val = max(data$Train[,col])+1
  x = h2o.hist(data$Train[, col])
  # use only the breaks with enough support
  breaks = x$breaks[which(x$counts > 1000)]
  # assign level names
  lvls = c("min",paste("i_",breaks[2:length(breaks)-1],sep=""),"max")
  col_cut <- paste(col,"_cut",sep="")
  data$Train[,col_cut] <- h2o.setLevels(h2o.cut(x = data$Train[,col],breaks=c(min_val,breaks,max_val)),lvls)
  # now do the same for test and validation, but using the breaks computed on the training!
  if(!is.null(data$Test)) {
    min_val = min(data$Test[,col])-1
    max_val = max(data$Test[,col])+1
    data$Test[,col_cut] <- h2o.setLevels(h2o.cut(x = data$Test[,col],breaks=c(min_val,breaks,max_val)),lvls)
  }
  if(!is.null(data$Valid)) {
    min_val = min(data$Valid[,col])-1
    max_val = max(data$Valid[,col])+1
    data$Valid[,col_cut] <- h2o.setLevels(h2o.cut(x = data$Valid[,col],breaks=c(min_val,breaks,max_val)),lvls)
  }
  data
}

# convenience function to add interaction terms between given categorical variables
# data is list(Train=training frame,Valid (optional) = validation frame, Test (optional) = test frame)
# applies the interactions to all three datasets
interactions <- function(data, cols, pairwise = TRUE) {
  iii = h2o.interaction(data = data$Train, destination_frame = "itrain",factors = cols,pairwise=pairwise,max_factors=1000,min_occurrence=100)
  data$Train <- h2o.cbind(data$Train,iii)
  if(!is.null(data$Test)) {
    iii = h2o.interaction(data = data$Test, destination_frame = "itest",factors = cols,pairwise=pairwise,max_factors=1000,min_occurrence=100)
    data$Test <- h2o.cbind(data$Test,iii)
  }
  if(!is.null(data$Valid)) {
    iii = h2o.interaction(data = data$Valid, destination_frame = "ivalid",factors = cols,pairwise=pairwise,max_factors=1000,min_occurrence=100)
    data$Valid <- h2o.cbind(data$Valid,iii)
  }
  data
}

# add features to our cover type example
# let's cut all the numerical columns into intervals and add interactions between categorical terms
add_features <- function(data) {
  names(data) <- c("Train","Test","Valid")
  data = cut_column(data,'Elevation')
  data = cut_column(data,'Hillshade_Noon')
  data = cut_column(data,'Hillshade_9am')
  data = cut_column(data,'Hillshade_3pm')
  data = cut_column(data,'Horizontal_Distance_To_Hydrology')
  data = cut_column(data,'Slope')
  data = cut_column(data,'Horizontal_Distance_To_Roadways')
  data = cut_column(data,'Aspect')
  # pairwise interactions between all categorical columns
  interaction_cols = c("Elevation_cut","Wilderness_Area","Soil_Type","Hillshade_Noon_cut","Hillshade_9am_cut","Hillshade_3pm_cut","Horizontal_Distance_To_Hydrology_cut","Slope_cut","Horizontal_Distance_To_Roadways_cut","Aspect_cut")
  data = interactions(data, interaction_cols)
  # interactions between Hillshade columns
  interaction_cols2 = c("Hillshade_Noon_cut","Hillshade_9am_cut","Hillshade_3pm_cut")
  data = interactions(data, interaction_cols2,pairwise = FALSE)
  data
}

### DEMO STARTS HERE ###
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "2G")
h2o.removeAll() ## clean slate - just in case the cluster was already running

D = h2o.importFile(path = normalizePath("../data/covtype.full.csv"))
# split into Train/Test/Validation
data = h2o.splitFrame(D,ratios=c(.7,.15),destination_frames = c("train","test","valid"))
names(data) <- c("Train","Test","Valid")
y = "Cover_Type"
x = names(data$Train)
x = x[-which(x==y)]

# Multinomial Model 1
m1 = h2o.glm(training_frame = data$Train, validation_frame = data$Valid, x = x, y = y,family='multinomial',solver='L_BFGS')
# not overfitting even a bit, maybe too much regularization? let's try with lower lambda than the default
m1 = h2o.glm(training_frame = data$Train, validation_frame = data$Valid, x = x, y = y,family='multinomial',solver='L_BFGS',lambda=1e-4)
# m2 = h2o.glm(training_frame = data$Train, validation_frame = data$Valid, x = x, y = y,family='multinomial',solver='IRLSM',alpha=.99) # runs 2.5 mins on laptop

# Binomial Model, let's build a model deciding between class1 and class 2
# take only rows with class_1 or class_2
D_binomial = D[D$Cover_Type %in% c("class_1","class_2"),]
h2o.setLevels(D_binomial$Cover_Type,c("class_1","class_2"))
# split to train/test/validation again
data_binomial = h2o.splitFrame(D_binomial,ratios=c(.7,.15),destination_frames = c("train_b","test_b","valid_b"))
names(data_binomial) <- c("Train","Test","Valid")
y = "Cover_Type"
x = names(data_binomial$Train)
x = x[-which(x==y)]
m_binomial = h2o.glm(training_frame = data_binomial$Train, validation_frame = data_binomial$Valid, x = x, y = y, family='binomial')

# Add Features
data_binomial_ext <- add_features(data_binomial)
data_binomial_ext$Train <- h2o.assign(data_binomial_ext$Train,"train_b_ext")
data_binomial_ext$Valid <- h2o.assign(data_binomial_ext$Valid,"valid_b_ext")
data_binomial_ext$Test <- h2o.assign(data_binomial_ext$Test,"test_b_ext")
y = "Cover_Type"
x = names(data_binomial_ext$Train)
x = x[-which(x==y)]
m_binomial_ext = h2o.glm(training_frame = data_binomial_ext$Train, validation_frame = data_binomial_ext$Valid, x = x, y = y, family='binomial')
# does not run, we got too many columns -> try L-BFGS
m_binomial_2_ext = h2o.glm(training_frame = data_binomial_ext$Train, validation_frame = data_binomial_ext$Valid, x = x, y = y, family='binomial',alpha=0, solver='L_BFGS')
# same as above, training same as test, maybe too much regularization? try again with lower lambda (alpha is 0 by default for L-BFGS)
m_binomial_3_ext = h2o.glm(training_frame = data_binomial_ext$Train, validation_frame = data_binomial_ext$Valid, x = x, y = y, family='binomial',alpha=0, solver='L_BFGS', lambda=1e-4)
# way better, 17% error, down from 24%, but is it optimal? We can run lambda search
# let's run default alpha, IRLSM works better with L1 and we can use it now(lambda search + strong rules)!
m_binomial_4_ext = h2o.glm(training_frame = data_binomial_ext$Train, validation_frame = data_binomial_ext$Valid, x = x, y = y, family='binomial',lambda_search=TRUE)
# similar to m3, slightly worse but at the edge of regularization, look slike we don't need much regulariztaion, might as well run the L2 only L-BFGS (faster)

# Multinomial Model 2
# let's revisit the multinomial case with our new features
data_ext <- add_features(data)
data_ext$Train <- h2o.assign(data_ext$Train,"train_m_ext")
data_ext$Valid <- h2o.assign(data_ext$Valid,"valid_m_ext")
data_ext$Test <- h2o.assign(data_ext$Test,"test_m_ext")
y = "Cover_Type"
x = names(data_ext$Train)
x = x[-which(x==y)]
m2 = h2o.glm(training_frame = data_ext$Train, validation_frame = data_ext$Valid, x = x, y = y,family='multinomial',solver='L_BFGS',lambda=1e-4)
# 21% err down from 28%
summary(m2)

### All done, shutdown H2O
h2o.shutdown(prompt=FALSE)

##########################################

###################################################################################
### Goal: demonstrate usage of H2O's Random Forest and GBM algorithms
### Task: Predicting forest cover type from cartographic variables only
###       The actual forest cover type for a given observation
###       (30 x 30 meter cell) was determined from the US Forest Service (USFS).

### Note: If run from plain R, execute R in the directory of this script. If run from RStudio,
### be sure to setwd() to the location of this script. h2o.init() starts H2O in R's current
### working directory. h2o.importFile() looks for files from the perspective of where H2O was
### started.


## H2O is an R package
library(h2o)
## Create an H2O cloud
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

## Load a file from disk
df <- h2o.importFile(path = normalizePath("../data/covtype.full.csv"))

## First, we will create three splits for train/test/valid independent data sets.
## We will train a data set on one set and use the others to test the validity
##  of model by ensuring that it can predict accurately on data the model has not
##  been shown.
## The second set will be used for validation most of the time. The third set will
##  be withheld until the end, to ensure that our validation accuracy is consistent
##  with data we have never seen during the iterative process.
splits <- h2o.splitFrame(
  df,           ##  splitting the H2O frame we read above
  c(0.6,0.2),   ##  create splits of 60% and 20%;
  ##  H2O will create one more split of 1-(sum of these parameters)
  ##  so we will get 0.6 / 0.2 / 1 - (0.6+0.2) = 0.6/0.2/0.2
  seed=1234)    ##  setting a seed will ensure reproducible results (not R's seed)

train <- h2o.assign(splits[[1]], "train.hex")
## assign the first result the R variable train
## and the H2O name train.hex
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

## take a look at the first few rows of the data set
train[1:5,]   ## rows 1-5, all columns

## run our first predictive model
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=1:12,                        ## the predictor columns, by column index
  y=13,                          ## the target index (what we are predicting)
  model_id = "rf_covType_v1",    ## name the model in H2O
  ##   not required, but helps use Flow
  ntrees = 200,                  ## use a maximum of 200 trees to create the
  ##  random forest model. The default is 50.
  ##  I have increased it because I will let
  ##  the early stopping criteria decide when
  ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  ##  average is within 0.001 (default) of
  ##  the prior two 2-tree averages.
  ##  Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 1000000)                ## Set the random seed so that this can be
##  reproduced.
###############################################################################
summary(rf1)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

rf1@model$validation_metrics     ## A more direct way to access the validation
##  metrics. Performance metrics depend on
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
h2o.hit_ratio_table(rf1,valid = T)[1,2]
## Even more directly, the hit_ratio @ k=1
###############################################################################

## Now we will try GBM.
## First we will use all default settings, and then make some changes,
##  where the parameters and defaults are described.

gbm1 <- h2o.gbm(
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=1:12,                        ## the predictor columns, by column index
  y=13,                          ## the target index (what we are predicting)
  model_id = "gbm_covType1",     ## name the model in H2O
  seed = 2000000)                ## Set the random seed for reproducability

###############################################################################
summary(gbm1)                   ## View information about the model.

h2o.hit_ratio_table(gbm1,valid = T)[1,2]
## Overall accuracy.

## This default GBM is much worse than our original random forest.
## The GBM is far from converging, so there are three primary knobs to adjust
##  to get our performance up if we want to keep a similar run time.
## 1: Adding trees will help. The default is 50.
## 2: Increasing the learning rate will also help. The contribution of each
##  tree will be stronger, so the model will move further away from the
##  overall mean.
## 3: Increasing the depth will help. This is the parameter that is the least
##  straightforward. Tuning trees and learning rate both have direct impact
##  that is easy to understand. Changing the depth means you are adjusting
##  the "weakness" of each learner. Adding depth makes each tree fit the data
##  closer.
##
## The first configuration will attack depth the most, since we've seen the
##  random forest focus on a continuous variable (elevation) and 40-class factor
##  (soil type) the most.
##
## Also we will take a look at how to review a model while it is running.

###############################################################################
gbm2 <- h2o.gbm(
  training_frame = train,     ##
  validation_frame = valid,   ##
  x=1:12,                     ##
  y=13,                       ##
  ntrees = 20,                ## decrease the trees, mostly to allow for run time
  ##  (from 50)
  learn_rate = 0.2,           ## increase the learning rate (from 0.1)
  max_depth = 10,             ## increase the depth (from 5)
  stopping_rounds = 2,        ##
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType2",  ##
  seed = 2000000)             ##

#### While this is running, we can actually look at the model.
#### To do this we simply need a new connection to H2O.
#### This R console will run the model, so we need either another R console
####   or the web browser (or python, etc.).
#### In the demo, we will use Flow in our web browser
####  http://localhost:54321
#### And the focus will be to look at model performance, since we are using R to
####  control H2O. So we can simply type in:
####  getModel "gbm_covType2"
###############################################################################

summary(gbm2)
h2o.hit_ratio_table(gbm1,valid = T)[1,2]    ## review the first model's accuracy
h2o.hit_ratio_table(gbm2,valid = T)[1,2]    ## review the new model's accuracy
###############################################################################

## This has moved us in the right direction, but still lower accuracy
##  than the random forest.
## And it still has not converged, so we can make it more aggressive.
## We can now add the stochastic nature of random forest into the GBM
##  using some of the new H2O settings. This will help generalize
##  and also provide a quicker runtime, so we can add a few more trees.

gbm3 <- h2o.gbm(
  training_frame = train,     ##
  validation_frame = valid,   ##
  x=1:12,                     ##
  y=13,                       ##
  ntrees = 30,                ## add a few trees (from 20, though default is 50)
  learn_rate = 0.3,           ## increase the learning rate even further
  max_depth = 10,             ##
  sample_rate = 0.7,          ## use a random 70% of the rows to fit each tree
  col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
  stopping_rounds = 2,        ##
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType3",  ##
  seed = 2000000)             ##
###############################################################################

summary(gbm3)
h2o.hit_ratio_table(rf1,valid = T)[1,2]     ## review the random forest accuracy
h2o.hit_ratio_table(gbm1,valid = T)[1,2]    ## review the first model's accuracy
h2o.hit_ratio_table(gbm2,valid = T)[1,2]    ## review the second model's accuracy
h2o.hit_ratio_table(gbm3,valid = T)[1,2]    ## review the newest model's accuracy
###############################################################################

## Now the GBM is close to the initial random forest.
## However, we used a default random forest.
## Random forest's primary strength is how well it runs with standard
##  parameters. And while there are only a few parameters to tune, we can
##  experiment with those to see if it will make a difference.
## The main parameters to tune are the tree depth and the mtries, which
##  is the number of predictors to use.
## The default depth of trees is 20. It is common to increase this number,
##  to the point that in some implementations, the depth is unlimited.
##  We will increase ours from 20 to 30.
## Note that the default mtries depends on whether classification or regression
##  is being run. The default for classification is one-third of the columns.
##  The default for regression is the square root of the number of columns.

rf2 <- h2o.randomForest(        ##
  training_frame = train,       ##
  validation_frame = valid,     ##
  x=1:12,                       ##
  y=13,                         ##
  model_id = "rf_covType2",     ##
  ntrees = 200,                 ##
  max_depth = 30,               ## Increase depth, from 20
  stopping_rounds = 2,          ##
  stopping_tolerance = 1e-2,    ##
  score_each_iteration = T,     ##
  seed=3000000)                 ##
###############################################################################
summary(rf2)
h2o.hit_ratio_table(gbm3,valid = T)[1,2]    ## review the newest GBM accuracy
h2o.hit_ratio_table(rf1,valid = T)[1,2]     ## original random forest accuracy
h2o.hit_ratio_table(rf2,valid = T)[1,2]     ## newest random forest accuracy
###############################################################################

## So we now have our accuracy up beyond 95%.
## We have witheld an extra test set to ensure that after all the parameter
##  tuning we have done, repeatedly applied to the validation data, that our
##  model produces similar results against the third data set.

## Create predictions using our latest RF model against the test set.
finalRf_predictions<-h2o.predict(
  object = rf2
  ,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalRf_predictions

## Compare these predictions to the accuracy we got from our experimentation
h2o.hit_ratio_table(rf2,valid = T)[1,2]             ## validation set accuracy
mean(finalRf_predictions$predict==test$Cover_Type)  ## test set accuracy

## We have very similar error rates on both sets, so it would not seem
##  that we have overfit the validation set through our experimentation.
##
## This concludes the demo, but what might we try next, if we were to continue?
##
## We could further experiment with deeper trees or a higher percentage of
##  columns used (mtries).
## Also we could experiment with the nbins and nbins_cats settings to control
##  the H2O splitting.
## The general guidance is to lower the number to increase generalization
##  (avoid overfitting), increase to better fit the distribution.
## A good example of adjusting this value is for nbins_cats to be increased
##  to match the number of values in a category. Though usually unnecessary,
##  if a problem has a very important categorical predictor, this can
##  improve performance.
##
## Also, we can tune our GBM more and surely get better performance.
## The GBM will converge a little slower for optimal accuracy, so if we
##  were to relax our runtime requirements a little bit, we could balance
##  the learn rate and number of trees used.
## In a production setting where fine-grain accuracy is beneficial, it is
##  common to set the learn rate to a very small number, such as 0.01 or less,
##  and add trees to match. Use of early stopping is very powerful to allow
##  the setting of a low learning rate and then building as many trees as
##  needed until the desired convergence is met.
## As with random forest, we can also adjust nbins and nbins_cats.


### All done, shutdown H2O
h2o.shutdown(prompt=FALSE)

##################################

