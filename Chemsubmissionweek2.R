library(gam)
library(caret)
library(cv) 

setwd("C:\\Users\\pmyin3\\Downloads")
training.data <- read.csv("training_set.csv", header = TRUE)
test.data <- read.csv("test_set.csv", header = TRUE)

###rebeccas GAM

# Remove the percentage column for training (not used in the model formula for classification)
training <- training.data[,-1]

# Ensure Impurity.Type is a factor - used later for classification
training$Impurity.Type <- as.factor(training$Impurity.Type)

## GAM Model 
# Fit the GAM model to the full training set using smoothing splines for each predictor.
mod_gam2 <- gam(Impurity.Percent ~ s(I) + s(II) + s(III) + s(IV) + s(V) + s(Temp), 
                data = training.data)
summary(mod_gam2)

### Cross-validation using LOO for GAM
set.seed(123)
n <- nrow(training.data)
gam_results <- numeric(n)

for(i in 1:n) {
  # Create training and test sets for this fold
  test_set <- training.data[i, , drop = FALSE]
  train_set <- training.data[-i, ]
  
  # Fit GAM on the training fold
  gam_model <- gam(Impurity.Percent ~ s(I) + s(II) + s(III) + s(IV) + s(V) + s(Temp), 
                   data = train_set)
  
  # Predict on the held-out observation
  gam_predictions <- predict(gam_model, test_set)
  
  # Calculate RMSE for this fold (there is only one observation)
  gam_rmse <- sqrt(mean((gam_predictions - test_set$Impurity.Percent)^2))
  gam_results[i] <- gam_rmse
}

gam.mean <- mean(gam_results)
gam.sd <- sd(gam_results)

print(paste("GAM RMSE mean:", round(gam.mean, 4)))
print(paste("GAM RMSE standard deviation:", round(gam.sd, 4)))

### GAM Test Predictions
# Generate predictions on the test set using the model fitted to all training data
predictions.percent <- predict(mod_gam2, test.data)


### Classification problem 


library(dplyr)
library(caret)
library(xgboost)
library(randomForest)

setwd("C:\\Users\\pmyin3\\Downloads")

#1. Load Data for Classification
training.data <- read.csv("training_set.csv", header = TRUE)


# Keep only  I, II, III, IV, V, Temp -common to both training and test
training.data <- training.data %>% 
  select(Impurity.Type, I, II, III, IV, V, Temp)
training.data$Impurity.Type <- as.factor(training.data$Impurity.Type)

# --- 2. Stage 1: Train Binary Classifier to detect "D" ---
# Create a binary target: 1 if D, 0 otherwise. Exclude Impurity.Percent from predictors. Tbh i think doing this indiually for each could work well
binary_train <- training.data %>%
  mutate(D_flag = ifelse(Impurity.Type == "D", 1, 0)) %>%
  select(-Impurity.Type)  # Now binary_train has columns: I, II, III, IV, V, Temp, D_flag

# Prepare feature matrix and label for XGBoost using only predictors present in test data.
X <- as.matrix(binary_train %>% select(-D_flag))
y <- binary_train$D_flag
dtrain <- xgb.DMatrix(data = X, label = y)

# Calculate positive weight for imbalance
pos_weight <- sum(y == 0) / sum(y == 1)

# Set parameters and train XGBoost binary classifier
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
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  verbose = 1
)

# --- 3. Stage 2: Train Multiclass RF Classifier on Non-D Samples ---
# Filter training data to include only non-D samples. Identified D are removed
multi_train <- training.data %>% filter(Impurity.Type != "D")
multi_train$Impurity.Type <- droplevels(multi_train$Impurity.Type)
# Train RF 
set.seed(314)
rf_model <- randomForest(Impurity.Type ~ I + II + III + IV + V + Temp, 
                         data = multi_train, 
                         ntree = 10000, 
                         importance = TRUE)
print("Stage 2 (RF Multiclass on Non-D) Model:")
print(rf_model)
varImpPlot(rf_model)

# Evaluate Stage 2 on non-D training data (if needed)
rf_preds_train <- predict(rf_model, newdata = multi_train)
conf_mat_rf <- confusionMatrix(rf_preds_train, multi_train$Impurity.Type)
print("Stage 2 Confusion Matrix (on non-D training data):")
print(conf_mat_rf)

# --- 4. Integrated Evaluation on Training Data (Optional) ---
# Use the binary model to filter the training set and combine predictions
pred_prob_train <- predict(xgb_model, dtrain)
binary_pred_full <- ifelse(pred_prob_train > 0.5, 1, 0)
integrated_preds <- rep(NA, nrow(training.data))
d_indices_full <- which(binary_pred_full == 1)
integrated_preds[d_indices_full] <- "D"


# ------------------------------
# Additional Prediction on Test Data 
# ------------------------------

# Predict yhat using the GAM model on the test data (test data has predictors: I, II, III, IV, V, Temp)
yhat_test <- predict(mod_gam2, newdata = test.data)

# For ghat: Use the binary classifier first.
# Since training for XGBoost used only columns: I, II, III, IV, V, Temp, we do the same here.
predictor_names <- c("I", "II", "III", "IV", "V", "Temp")
X_test <- as.matrix(test.data[, predictor_names, drop = FALSE])
dtest <- xgb.DMatrix(data = X_test)

# Predict using the XGBoost model to flag samples as "D"
binary_pred_test <- ifelse(predict(xgb_model, dtest) > 0.5, 1, 0)

# Create integrated group predictions (ghat)
integrated_preds_test <- rep(NA, nrow(test.data))
integrated_preds_test[binary_pred_test == 1] <- "D"

# For test cases predicted as non-D, use the RF model to predict the specific Impurity.Type.
# The RF model was trained with predictors: I, II, III, IV, V, Temp.
nonD_indices_test <- which(binary_pred_test == 0)
if (length(nonD_indices_test) > 0) {
  rf_preds_test <- predict(rf_model, newdata = test.data[nonD_indices_test, predictor_names, drop = FALSE])
  integrated_preds_test[nonD_indices_test] <- as.character(rf_preds_test)
}

# Combine the group (ghat) and regression (yhat) predictions into one data frame
output <- data.frame(Group = integrated_preds_test, Impurity.Percent = yhat_test)

write.csv(output, file = "chemical_predictions_group_X_week_Y.csv", row.names = FALSE)
cat("CSV file 'chemical_predictions_group_X_week_Y.csv' has been created with", nrow(output), "observations.\n")
