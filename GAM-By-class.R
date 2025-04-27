# --------------------------
# Load required libraries
# --------------------------
library(gam)
library(mgcv)
library(dplyr)
library(caret)

# --------------------------
# Set working directory and load training data
# --------------------------
setwd("C:/Users/pmyin3/Downloads")
training.data <- read.csv("training_set.csv", header = TRUE)
training.data$Impurity.Type <- as.factor(training.data$Impurity.Type)

# --------------------------
# 3. GAM Regression Model on Training Data Labelled confident 
# --------------------------
training.data$Impurity.Type <- as.factor(training.data$Impurity.Type)

# Fit a GAM model to predict Impurity.Percent using smoothing splines for the numeric predictors
# Impurity.Type as a parametric factor  you can remove Impurity.Type if you only want to use the numeric predictors.
refined_gam <- gam(Impurity.Percent ~ 
                     s(I, k = 3, bs = "cr") +
                     s(II, k = 5, bs = "cr") +
                     s(III, k = 5, bs = "ps") +
                     s(IV, k = 5, bs = "cr") +
                     s(V, k = 15, bs = "cr") +
                     s(Temp, k = 5, bs = "cr") +
                     Impurity.Type,    #adjust for class differences
                   data = training.data,
                   select = TRUE,    # Enable automatic term selection
                   method = "REML")  #smoothing parameter estimation

# Print the summary to inspect effective degrees of freedom and term significance - this means nothing to me 
summary(refined_gam)

# Predict Impurity.Percent on the training data using the updated model tested only on 72 marked as confident,
#I compared with the box plots from training data most predictions seem in line with them 
predicted_percent <- predict(refined_gam, newdata = training.data)



# Add the adjusted regression predictions to the training data
training.data$Predicted.Percent <- adjusted_percent

cat("GAM model with automatic term selection (select = TRUE) has been fitted and predictions stored.\n")

# --------------------------
# 5. 5-Fold Cross-Validation for the GAM Regression Model
# --------------------------
set.seed(314)
folds <- createFolds(training.data$Impurity.Percent, k = 5, list = TRUE)
cv_mse <- numeric(length(folds))

for (i in seq_along(folds)) {
  test_idx <- folds[[i]]
  train_idx <- setdiff(seq_len(nrow(training.data)), test_idx)
  
  # Fit the GAM on the training folds using the same formula gonna try do some more robustness checking on tuesday 
  cv_model <- gam(Impurity.Percent ~ 
                    s(I, k = 3, bs = "cr") +
                    s(II, bs = "cr") +
                    s(III, bs = "ps") +
                    s(IV, bs = "cr") +
                    s(V, k = 15, bs = "cr") +
                    s(Temp, bs = "cr") +
                    Impurity.Type,
                  data = training.data[train_idx, ],
                  method = "REML")
  
  # Predict on left out 
  cv_pred <- predict(cv_model, newdata = training.data[test_idx, ])
  cv_mse[i] <- mean((training.data$Impurity.Percent[test_idx] - cv_pred)^2)
}

mean_cv_mse <- mean(cv_mse)
cat("5-fold cross-validated MSE for the GAM regression model:", mean_cv_mse, "\n")
