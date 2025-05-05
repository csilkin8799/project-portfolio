# Charles Silkin
# Decision Trees and Random Forests Project Code
# FSU Interdisciplinary Data Science Master's Program - Applied Machine Learning Course
# 24 January 2023

# Load packages:
library(tidyverse)
library(rpart)
library(randomForest)
library(caret)
library(rpart.plot)

# Load data:
abalone = read.csv("abalone.csv", header = FALSE)

# MSE function:
mse = function(y_true, y_pred) {
  mean((y_true - y_pred)^2)
}

## R^2 function:
r2 = function(y_true, y_pred){
  1 - (sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2))
}

# ----- PART A -----

# Create list of MSE's:
train_mse_a = c()
test_mse_a = c()

for (i in 1:15){ # For loop to repeat process 15 times
  
  ## Split data
  spl = sample(x = 2, size = nrow(abalone), replace = T, prob = c(0.9, 0.1))
  train = abalone[spl == 1,] ### Train data (90%)
  test = abalone[spl == 2,] ### Test data (10%)
  
  ## Obtain training y-bar
  train_y_bar = mean(train$V8)
  
  ## Calculate MSE:
  train_mse = mse(train$V8, train_y_bar)
  test_mse = mse(test$V8, train_y_bar)
  
  ## Append train and test MSE's to lists:
  train_mse_a = append(train_mse_a, train_mse)
  test_mse_a = append(test_mse_a, test_mse)
}

# Compute average train and test MSE's
avg_train_mse_a = mean(train_mse_a)
avg_test_mse_a = mean(test_mse_a)

avg_train_mse_a
avg_test_mse_a

# ----- PART B -----

# Create list of MSE's and R^2's:
train_mse_b = c()
test_mse_b = c()
train_r2_b = c()
test_r2_b = c()

for (i in 1:15) { ## For loop to repeat process 15 times
  
  ## Split data
  spl = sample(x = 2, size = nrow(abalone), replace = T, prob = c(0.9, 0.1))
  train = abalone[spl == 1,] ### Train data (90%)
  test = abalone[spl == 2,] ### Test data (10%)
  
  ## Create matrices/vectors for training data
  train_x = as.matrix(train[-8])
  lambda_mat = 0.001 * diag(7) ### Multiply identity matrix by lambda=0.001
  train_x_tran = t(train_x)
  train_y = as.matrix(train[8]) ### vector Y
  
  ## Solve for beta
  train_beta = solve((train_x_tran %*% train_x) + lambda_mat) %*%
    train_x_tran %*% train_y
  
  ## Create matrices for test data:
  test_x = as.matrix(test[-8])
  test_y = as.matrix(test[8])
  
  ## Obtain predicted y-values:
  train_pred = train_x %*% train_beta
  test_pred = test_x %*% train_beta
  
  ## Calculate MSE:
  train_mse = mse(train_y, train_pred)
  test_mse = mse(test_y, test_pred)
  
  ## Calculate R^2:
  train_r2 = r2(train_y, train_pred)
  test_r2 = r2(test_y, test_pred)
  
  ## Append train and test MSE's and R^2's to lists:
  train_mse_b = append(train_mse_b, train_mse)
  test_mse_b = append(test_mse_b, test_mse)
  train_r2_b = append(train_r2_b, train_r2)
  test_r2_b = append(test_r2_b, test_r2)
}

# Compute average train and test MSE's
avg_train_mse_b = mean(train_mse_b)
avg_test_mse_b = mean(test_mse_b)

# Compute average train and test R^2's
avg_train_r2_b = mean(train_r2_b)
avg_test_r2_b = mean(test_r2_b)

avg_train_mse_b
avg_test_mse_b
avg_train_r2_b
avg_test_r2_b


# ----- PART C -----

# Create lists for avg. MSE's, R^2's, and tree depths:
tree_depth = c()
avg_train_mse_c = c()
avg_test_mse_c = c()
avg_train_r2_c = c()
avg_test_r2_c = c()

for (i in 1:7) { ## 7 decision trees
  
  # Create list of MSE's and R^2's:
  train_mse_c = c()
  test_mse_c = c()
  train_r2_c = c()
  test_r2_c = c()
  
  ## 15 random splits
  for (j in 1:15) {
    
    ### Split data
    spl = sample(x = 2, size = nrow(abalone), replace = T, prob = c(0.9, 0.1))
    train = abalone[spl == 1,] #### Train data (90%)
    test = abalone[spl == 2,] #### Test data (10%)
    
    ### Create decision tree
    dec_tree = rpart(V8~., data=train, control = rpart.control(maxdepth = i))
    ## Training Data Prediction:
    train_pred = predict(dec_tree, train)
    
    ### Test Data Prediction
    test_pred = predict(dec_tree, test)
    
    ### Calculate MSE:
    train_mse = mse(train$V8, train_pred)
    test_mse = mse(test$V8, test_pred)
    
    ### Calculate R^2:
    train_r2 = r2(train$V8, train_pred)
    test_r2 = r2(test$V8, test_pred)
    
    ### Append train and test MSE's and R^2's to lists:
    train_mse_c = append(train_mse_c, train_mse)
    test_mse_c = append(test_mse_c, test_mse)
    train_r2_c = append(train_r2_c, train_r2)
    test_r2_c = append(test_r2_c, test_r2)
  }
  
  ## Compute average train and test MSE's
  avg_train_mse = mean(train_mse_c)
  avg_test_mse = mean(test_mse_c)
  
  ## Compute average train and test R^2's
  avg_train_r2 = mean(train_r2_c)
  avg_test_r2 = mean(test_r2_c)
  
  ## Append average values into lists
  tree_depth = append(tree_depth, i)
  avg_train_mse_c = append(avg_train_mse_c, avg_train_mse)
  avg_test_mse_c = append(avg_test_mse_c, avg_test_mse)
  avg_train_r2_c = append(avg_train_r2_c, avg_train_r2)
  avg_test_r2_c = append(avg_test_r2_c, avg_test_r2)
}

## Plots:
par(mfrow = c(1,2))
plot(tree_depth, avg_train_r2_c, 
     type = "o", col = "orange", pch=19, lwd=1, cex=1, lty = 1, ylim = c(0, 1),
     main = expression(paste("Average ", R^2, " vs. Tree Depth")),
     xlab = "Tree Depth", ylab = expression(paste("Average ", R^2)))
lines(x = tree_depth, y = avg_test_r2_c, 
      type = "o", col = "blue", pch = 18, lwd = 2, cex = 1, lty = 2)
legend("bottomright", c("Training Data", "Test Data"), 
       col = c("orange", "blue"), pch = c(19,18), lwd = c(1,2), lty = c(1,2))
plot(tree_depth, avg_train_mse_c, 
     type = "o", col = "orange", pch=19, lwd=1, cex=1, lty = 1, ylim = c(2,12),
     main = expression(paste("Average MSE vs. Tree Depth")),
     xlab = "Tree Depth", ylab = "Average MSE")
lines(x = tree_depth, y = avg_test_mse_c, 
      type = "o", col = "blue", pch = 18, lwd = 2, cex = 1, lty = 2)
abline(h = avg_test_mse_a, col = "red")
legend("bottomright", c("Training Data", "Test Data"), 
       col = c("orange", "blue"), pch = c(19,18), lwd = c(1,2), lty = c(1,2))



# ----- PART D -----

# Create lists for avg. MSE's, R^2's, and tree depths
k_list = c(10, 30, 100, 300)
avg_train_mse_d = c()
avg_test_mse_d = c()
avg_train_r2_d = c()
avg_test_r2_d = c()

for (k in k_list){ # Random forests with number of trees specified
  
  # Create list of MSE's and R^2's:
  train_mse_d = c()
  test_mse_d = c()
  train_r2_d = c()
  test_r2_d = c()
  
  ## 15 random splits
  for (j in 1:15) {
    
    ### Split data
    spl = sample(x = 2, size = nrow(abalone), replace = T, prob = c(0.9, 0.1))
    train = abalone[spl == 1,] #### Train data (90%)
    test = abalone[spl == 2,] #### Test data (10%)
    
    ### Random forest
    rf_d = randomForest(V8~., data = train, ntrees = k)
    
    ### Training Data Prediction:
    train_pred = predict(rf_d, train)
    
    ### Test Data Prediction
    test_pred = predict(rf_d, test)
    
    ### Calculate MSE:
    train_mse = mse(train$V8, train_pred)
    test_mse = mse(test$V8, test_pred)
    
    ### Calculate R^2:
    train_r2 = r2(train$V8, train_pred)
    test_r2 = r2(test$V8, test_pred)
    
    ### Append train and test MSE's and R^2's to lists:
    train_mse_d = append(train_mse_d, train_mse)
    test_mse_d = append(test_mse_d, test_mse)
    train_r2_d = append(train_r2_d, train_r2)
    test_r2_d = append(test_r2_d, test_r2)
  }
  
  ## Compute average train and test MSE's
  avg_train_mse = mean(train_mse_d)
  avg_test_mse = mean(test_mse_d)
  
  ## Compute average train and test R^2's
  avg_train_r2 = mean(train_r2_d)
  avg_test_r2 = mean(test_r2_d)
  
  ## Append average values into lists
  avg_train_mse_d = append(avg_train_mse_d, avg_train_mse)
  avg_test_mse_d = append(avg_test_mse_d, avg_test_mse)
  avg_train_r2_d = append(avg_train_r2_d, avg_train_r2)
  avg_test_r2_d = append(avg_test_r2_d, avg_test_r2)
}