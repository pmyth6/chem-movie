
library(xgboost)
library(dplyr)
library(caret)
library(DiagrammeR)  # for plotting trees 

setwd("C:\\Users\\pmyin3\\Downloads")

# Load training data; ensure it contains: Impurity.Type, I, II, III, IV, V, Temp
training.data <- read.csv("training_set.csv", header = TRUE)
training.data$Impurity.Type <- as.factor(training.data$Impurity.Type)


# Set the target class to one of your actual classes
target_class <- "I" 

# Create 10-fold cross-validation indices
folds <- createFolds(training.data$Impurity.Type, k = 10, list = TRUE)
cv_accuracies <- c()

for (i in seq_along(folds)) {
  test_idx <- folds[[i]]
  train_idx <- setdiff(seq_len(nrow(training.data)), test_idx)
  
  # Split the data into training and test folds
  train_data <- training.data[train_idx, ]
  test_data  <- training.data[test_idx, ]
  
  # Build the matrices for predictors
  X_train_cv <- as.matrix(train_data[, predictors])
  X_test_cv  <- as.matrix(test_data[, predictors])
  
  # Create a binary target: 1 if the observation belongs to the target class, else 0
  binary_target <- as.numeric(train_data$Impurity.Type == target_class)
  
  # Create DMatrix objects for XGBoost
  dtrain_cv <- xgb.DMatrix(data = X_train_cv, label = binary_target)
  dtest_cv  <- xgb.DMatrix(data = X_test_cv)
  
  
  pos_weight <- sum(binary_target == 0) / sum(binary_target == 1)
  
  # Set XGBoost parameters
  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    scale_pos_weight = pos_weight,
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  set.seed(314)
  model_cv <- xgb.train(params = params, data = dtrain_cv, nrounds = 500, verbose = 0)
  
  # Predict probabilities on the test set and convert to binary predictions
  pred_probs <- predict(model_cv, dtest_cv)
  pred_binary <- ifelse(pred_probs > 0.5, 1, 0)
  
  # Convert actual test labels to binary for comparison
  actual_binary <- ifelse(test_data$Impurity.Type == target_class, 1, 0)
  
  fold_accuracy <- mean(pred_binary == actual_binary)
  cv_accuracies <- c(cv_accuracies, fold_accuracy)
}

cat("Cross-Validation Accuracies:", cv_accuracies, "\n")
cat("Average CV Accuracy:", mean(cv_accuracies), "\n")