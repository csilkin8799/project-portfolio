# Charles Silkin
# Decision Trees and Random Forests Project Code
# FSU Interdisciplinary Data Science Master's Program - Applied Machine Learning Course
# 11 January 2023

# Load packages:
library(tidyverse)
library(rpart)
library(randomForest)
library(caret)
library(rpart.plot)

# ----- PART A -----

## Denote file path to access MADELON data sets
madelon_file_path = "Supporting Data Sets" ### Fill in manually

## Load data:
madelon_train = read.table(paste(madelon_file_path, "madelon_train.data",sep="/"), fileEncoding = "UTF-8", sep="")
madelon_labels = read.table(paste(madelon_file_path, "madelon_train.labels",sep="/"), fileEncoding = "UTF-8", sep="")
madelon_valid = read.table(paste(madelon_file_path, "madelon_valid.data",sep="/"), fileEncoding = "UTF-8", sep="")
madelon_valid_labs = read.table(paste(madelon_file_path, "madelon_valid.labels",sep="/"), fileEncoding = "UTF-8", sep="")

## Merge labels for training data with data set:
colnames(madelon_labels) = "fin_labs"
fin_madelon_train = cbind(madelon_train, madelon_labels)
fin_madelon_train$fin_labs = as.factor(fin_madelon_train$fin_labs) ### Turn labels from int to factor

## Merge labels for testing data with data set:
colnames(madelon_valid_labs) = "fin_labs"
fin_madelon_test = cbind(madelon_valid, madelon_valid_labs)
fin_madelon_test$fin_labs = as.factor(fin_madelon_test$fin_labs) ### Turn labels from int to factor

## Obtain Training and Test Misclassification Errors:
tree_depth = c()
train_class_error = c()
test_class_error = c()

### Create 12 decision trees:
for (i in 1:12){
  
  ## Create decision tree
  madelon_tree <- rpart(fin_labs~., data=fin_madelon_train, control = rpart.control(maxdepth = i))
  
  ## Training Data Prediction:
  train_pred = predict(madelon_tree, fin_madelon_train, type = "class") ### predicted values
  
  ### Data frame with actual and predicted values
  train_err_df = data.frame(fin_madelon_train$fin_labs, train_pred)
  colnames(train_err_df) = c("val", "pred")
  train_err_df$acc = 0
  
  #### Set "acc" to 1 if predicted value = actual value
  for (j in 1:nrow(train_err_df)) {
    if (train_err_df$val[j] == train_err_df$pred[j]) {
      train_err_df$acc[j] = 1
    }
  }
  
  train_misclass = nrow(train_err_df[train_err_df$acc==0,]) ### Number of innacurate predictions
  
  ### Misclassification Error
  train_misclass_error = train_misclass / nrow(train_err_df)
  
  ## Test Data Prediction
  test_pred = predict(madelon_tree, fin_madelon_test, type = "class") ### predicted values
  
  ### Data frame with actual and predicted values
  test_err_df = data.frame(fin_madelon_test$fin_labs, test_pred)
  colnames(test_err_df) = c("val", "pred")
  test_err_df$acc = 0
  
  #### Set "acc" to 1 if predicted value = actual value
  for (k in 1:nrow(test_err_df)) {
    if (test_err_df$val[k] == test_err_df$pred[k]) {
      test_err_df$acc[k] = 1
    }
  }
  
  test_misclass = nrow(test_err_df[test_err_df$acc==0,]) ## Number of innacurate predictions
  
  ### Misclassification Error
  test_misclass_error = test_misclass / nrow(test_err_df)
  
  ## Append tree depth and errors into respective vectors
  tree_depth = append(tree_depth, i)
  train_class_error = append(train_class_error, train_misclass_error)
  test_class_error = append(test_class_error, test_misclass_error)
}

## Consolidate errors into data frame:
madelon_final_errors_df = data.frame(tree_depth, train_class_error, test_class_error)

## Plot:
plot(madelon_final_errors_df$tree_depth, madelon_final_errors_df$train_class_error, 
     type = "o", col = "orange", pch=19, lwd=1, cex=1, lty = 1, ylim = c(0.12, 0.4),
     main = "Decision Tree Classification Errors Plot (MADELON)",
     xlab = "Tree Depth", ylab = "Misclassification Error")
lines(madelon_final_errors_df$test_class_error, 
      type = "o", col = "blue", pch = 18, lwd = 2, cex = 1, lty = 2)
legend("topright", c("Training Data", "Test Data"), 
       col = c("orange", "blue"), pch = c(19,18), lwd = c(1,2), lty = c(1,2))

# ----- PART B -----

## Denote file path to access satimage data sets:
satimage_file_path = "Supporting Data Sets"

## Load data:
satimage_train = read.table(paste(satimage_file_path, "sat.trn",sep="/"), fileEncoding = "UTF-8", sep="")
satimage_test = read.table(paste(satimage_file_path, "sat.tst",sep="/"), fileEncoding = "UTF-8", sep="")

## Turn labels from int to factor:
satimage_train$V37 = as.factor(satimage_train$V37)
satimage_test$V37 = as.factor(satimage_test$V37)

## Obtain Training and Test Misclassification Errors:
tree_depth = c()
train_class_error = c()
test_class_error = c()

## Create 12 decision trees (same process as with MADELON data set)
for (i in 1:12){
  satimage_tree <- rpart(V37~., data=satimage_train, control = rpart.control(maxdepth = i))
  
  train_pred = predict(satimage_tree, satimage_train, type = "class")
  err_df = data.frame(satimage_train$V37, train_pred)
  colnames(err_df) = c("val", "pred")
  err_df$acc = 0
  
  for (j in 1:nrow(err_df)) {
    if (err_df$val[j] == err_df$pred[j]) {
      err_df$acc[j] = 1
    }
  }
  
  train_misclass = nrow(err_df[err_df$acc==0,])
  train_misclass_error = train_misclass / nrow(err_df)
  
  test_pred = predict(satimage_tree, satimage_test, type = "class")
  test_error_df = data.frame(satimage_test$V37, test_pred)
  colnames(test_error_df) = c("val", "pred")
  test_error_df$acc = 0
  
  for (k in 1:nrow(test_error_df)) {
    if (test_error_df$val[k] == test_error_df$pred[k]) {
      test_error_df$acc[k] = 1
    }
  }
  
  test_misclass = nrow(test_error_df[test_error_df$acc==0,])
  test_misclass_error = test_misclass / nrow(test_error_df)
  
  tree_depth = append(tree_depth, i)
  train_class_error = append(train_class_error, train_misclass_error)
  test_class_error = append(test_class_error, test_misclass_error)
}

satimage_final_errors_df = data.frame(tree_depth, train_class_error, test_class_error)

## Plot:
plot(satimage_final_errors_df$tree_depth, satimage_final_errors_df$train_class_error, 
     type = "o", col = "#E34534", pch=19, lwd=1, cex=1, lty = 1, ylim = c(0.12, 0.6),
     main = "Decision Tree Classification Errors Plot (satimage)",
     xlab = "Tree Depth", ylab = "Misclassification Error")
lines(satimage_final_errors_df$test_class_error, 
      type = "o", col = "#C56784", pch = 18, lwd = 2, cex = 1, lty = 2)
legend("topright", c("Training Data", "Test Data"), 
       col = c("#E34534", "#C56784"), pch = c(19,18), lwd = c(1,2), lty = c(1,2))

# ----- PART C -----

## Create list of number of trees:
k_list = c(3,10, 30, 100, 300)

## Obtain Training and Test Misclassification Errors:
rf_c_num_trees = c()
rf_c_train_error = c()
rf_c_test_error = c()

### Create random forests for each number of trees specified in k_list:
for (k in k_list){
  
  ## Random forest
  madelon_rf_c = randomForest(fin_labs~., data = fin_madelon_train, mtry = sqrt(500), ntrees = k)
  
  ## Training Data Prediction:
  rf_c_predict = predict(madelon_rf_c, fin_madelon_train) ### predicted values
  
  ### Data frame with actual and predicted values
  rf_err_df = data.frame(fin_madelon_train$fin_labs, rf_c_predict)
  colnames(rf_err_df) = c("val", "pred")
  rf_err_df$acc = 0
  
  #### Set "acc" to 1 if predicted value = actual value
  for (j in 1:nrow(rf_err_df)) {
    if (rf_err_df$val[j] == rf_err_df$pred[j]) {
      rf_err_df$acc[j] = 1
    }
  }
  
  rf_train_misclass = nrow(rf_err_df[rf_err_df$acc==0,]) ## Number of innacurate predictions
  
  ### Misclassification Error
  rf_train_misclass_error = rf_train_misclass / nrow(rf_err_df)
  
  ## Test Data Prediction:
  rf_c_test_predict = predict(madelon_rf_c, fin_madelon_test) ### predicted values
  
  ### Data frame with actual and predicted values
  rf_test_err_df = data.frame(fin_madelon_test$fin_labs, rf_c_test_predict)
  colnames(rf_test_err_df) = c("val", "pred")
  rf_test_err_df$acc = 0
  
  #### Set "acc" to 1 if predicted value = actual value
  for (m in 1:nrow(rf_test_err_df)) {
    if (rf_test_err_df$val[m] == rf_test_err_df$pred[m]) {
      rf_test_err_df$acc[m] = 1
    }
  }
  
  rf_test_misclass = nrow(rf_test_err_df[rf_test_err_df$acc==0,]) ## Number of innacurate predictions
  
  ### Misclassification Error
  rf_test_misclass_error = rf_test_misclass / nrow(rf_test_err_df)
  
  ## Append number of trees and errors into respective vectors
  rf_c_num_trees = append(rf_c_num_trees, k)
  rf_c_train_error = append(rf_c_train_error, rf_train_misclass_error)
  rf_c_test_error = append(rf_c_test_error, rf_test_misclass_error)
}

## Consolidate number of trees and errors into data frame
madelon_rf_c_final_errors_df = data.frame(rf_c_num_trees, rf_c_train_error, rf_c_test_error)

## Plot:
plot(madelon_rf_c_final_errors_df$rf_c_num_trees, madelon_rf_c_final_errors_df$rf_c_train_error, 
     type = "o", col = "lightblue", pch=19, lwd=1, cex=1, lty = 1, ylim = c(-0.05, 0.5),
     main = "Random Forest With Subset of SQRT(500) Features",
     xlab = "Number of Trees", ylab = "Misclassification Error")
lines(madelon_rf_c_final_errors_df$rf_c_num_trees, madelon_rf_c_final_errors_df$rf_c_test_error, 
      type = "o", col = "purple", pch = 18, lwd = 2, cex = 1, lty = 2)
legend("topright", c("Training Data", "Test Data"), 
       col = c("lightblue", "purple"), pch = c(19,18), lwd = c(1,2), lty = c(1,2))

# ----- PART D -----

## Obtain Training and Test Misclassification Errors:
rf_d_num_trees = c()
rf_d_train_error = c()
rf_d_test_error = c()

### Create random forests for each number of trees specified in k_list
### (same process as with subset of SQRT(500) features)
for (k in k_list){
  
  madelon_rf_d = randomForest(fin_labs~., data = fin_madelon_train, mtry = log(500), ntrees = k)
  
  rf_d_predict = predict(madelon_rf_d, fin_madelon_train)
  
  rf_err_df = data.frame(fin_madelon_train$fin_labs, rf_d_predict)
  colnames(rf_err_df) = c("val", "pred")
  rf_err_df$acc = 0
  
  for (j in 1:nrow(rf_err_df)) {
    if (rf_err_df$val[j] == rf_err_df$pred[j]) {
      rf_err_df$acc[j] = 1
    }
  }
  
  rf_train_misclass = nrow(rf_err_df[rf_err_df$acc==0,])
  rf_train_misclass_error = rf_train_misclass / nrow(rf_err_df)
  
  rf_d_test_predict = predict(madelon_rf_d, fin_madelon_test)
  
  rf_test_err_df = data.frame(fin_madelon_test$fin_labs, rf_d_test_predict)
  colnames(rf_test_err_df) = c("val", "pred")
  rf_test_err_df$acc = 0
  
  for (m in 1:nrow(rf_test_err_df)) {
    if (rf_test_err_df$val[m] == rf_test_err_df$pred[m]) {
      rf_test_err_df$acc[m] = 1
    }
  }
  
  rf_test_misclass = nrow(rf_test_err_df[rf_test_err_df$acc==0,])
  rf_test_misclass_error = rf_test_misclass / nrow(rf_test_err_df)
  
  
  rf_d_num_trees = append(rf_d_num_trees, k)
  rf_d_train_error = append(rf_d_train_error, rf_train_misclass_error)
  rf_d_test_error = append(rf_d_test_error, rf_test_misclass_error)
}

## Consolidate number of trees and errors into data frame
madelon_rf_d_final_errors_df = data.frame(rf_d_num_trees, rf_d_train_error, rf_d_test_error)

## Plot:
plot(madelon_rf_d_final_errors_df$rf_d_num_trees, madelon_rf_d_final_errors_df$rf_d_train_error, 
     type = "o", col = "green", pch=19, lwd=1, cex=1, lty = 1, ylim = c(-0.05, 0.5),
     main = "Random Forest With Subset of LN(500) Features",
     xlab = "Number of Trees", ylab = "Misclassification Error")
lines(madelon_rf_d_final_errors_df$rf_d_num_trees, madelon_rf_d_final_errors_df$rf_d_test_error, 
      type = "o", col = "pink", pch = 18, lwd = 2, cex = 1, lty = 2)
legend("topright", c("Training Data", "Test Data"), 
       col = c("green", "pink"), pch = c(19,18), lwd = c(1,2), lty = c(1,2))

# ----- PART E -----

## Obtain Training and Test Misclassification Errors:
rf_e_num_trees = c()
rf_e_train_error = c()
rf_e_test_error = c()

### Create random forests for each number of trees specified in k_list
### (same process as with subset of SQRT(500) and LN(500) features)
for (k in k_list){
  
  madelon_rf_e = randomForest(fin_labs~., data = fin_madelon_train, mtry = 500, ntrees = k)
  
  rf_e_predict = predict(madelon_rf_e, fin_madelon_train)
  
  rf_err_df = data.frame(fin_madelon_train$fin_labs, rf_e_predict)
  colnames(rf_err_df) = c("val", "pred")
  rf_err_df$acc = 0
  
  for (j in 1:nrow(rf_err_df)) {
    if (rf_err_df$val[j] == rf_err_df$pred[j]) {
      rf_err_df$acc[j] = 1
    }
  }
  
  rf_train_misclass = nrow(rf_err_df[rf_err_df$acc==0,])
  rf_train_misclass_error = rf_train_misclass / nrow(rf_err_df)
  
  rf_e_test_predict = predict(madelon_rf_e, fin_madelon_test)
  
  rf_test_err_df = data.frame(fin_madelon_test$fin_labs, rf_e_test_predict)
  colnames(rf_test_err_df) = c("val", "pred")
  rf_test_err_df$acc = 0
  
  for (m in 1:nrow(rf_test_err_df)) {
    if (rf_test_err_df$val[m] == rf_test_err_df$pred[m]) {
      rf_test_err_df$acc[m] = 1
    }
  }
  
  rf_test_misclass = nrow(rf_test_err_df[rf_test_err_df$acc==0,])
  rf_test_misclass_error = rf_test_misclass / nrow(rf_test_err_df)
  
  
  rf_e_num_trees = append(rf_e_num_trees, k)
  rf_e_train_error = append(rf_e_train_error, rf_train_misclass_error)
  rf_e_test_error = append(rf_e_test_error, rf_test_misclass_error)
}

## Consolidate number of trees and errors into data frame
madelon_rf_e_final_errors_df = data.frame(rf_e_num_trees, rf_e_train_error, rf_e_test_error)

## Plot:
plot(madelon_rf_e_final_errors_df$rf_e_num_trees, madelon_rf_e_final_errors_df$rf_e_train_error, 
     type = "o", col = "darkgreen", pch=19, lwd=1, cex=1, lty = 1, ylim = c(-0.05, 0.5),
     main = "Random Forest With All 500 Features",
     xlab = "Number of Trees", ylab = "Misclassification Error")
lines(madelon_rf_e_final_errors_df$rf_e_num_trees, madelon_rf_e_final_errors_df$rf_e_test_error, 
      type = "o", col = "red", pch = 18, lwd = 2, cex = 1, lty = 2)
legend("topright", c("Training Data", "Test Data"), 
       col = c("darkgreen", "red"), pch = c(19,18), lwd = c(1,2), lty = c(1,2))