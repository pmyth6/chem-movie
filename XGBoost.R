install.package('xgboost')


library(dplyr)
library(xgboost)
library(caret)

setwd("C:\\Users\\pmyin3\\Downloads")


training.data <- read.csv("training_set.csv", header = TRUE)
test.data     <- read.csv("test_set.csv", header = TRUE)

# Cleaning
names(training.data) <- trimws(names(training.data))
names(test.data)     <- trimws(names(test.data))

# Check Impurity.Type 
if(!"Impurity.Type" %in% names(training.data)) {
  stop("Column 'Impurity.Type' not found!")
}

# Create a binary target: 1 if Impurity.Type is "D", else 0. Now we remove the OG 
#so the new column is yes or no
binary_train <- training.data %>%
  mutate(D_flag = ifelse(Impurity.Type == "D", 1, 0)) %>%
  select(-Impurity.Type)

# Check binary_train for D_flag
cat("Number of rows in binary_train:", nrow(binary_train), "\n")

# Prepare for XGBoost X is all feats, y is the binary target.
X <- as.matrix(binary_train %>% select(-D_flag))
y <- binary_train$D_flag

# Create XGBoost DMatrix
dtrain <- xgb.DMatrix(data = X, label = y)

# Calculate positive weight for class imbalance
pos_weight <- sum(y == 0) / sum(y == 1)

# Set XGBoost parameters + problem - binary classification -what we tune to make the model better basically 
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
# Train 
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  verbose = TRUE
)

# Make predictions on train
pred_prob <- predict(xgb_model, dtrain)
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# model evaluation
conf_matrix <- table(Predicted = pred_class, Actual = y)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

importance_matrix <- xgb.importance(feature_names = colnames(X), model = xgb_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix)
