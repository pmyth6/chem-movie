
setwd("C:\\Users\\pmyin3\\Downloads")
training.data <- read.csv("training_set.csv", header = TRUE)
test.data     <- read.csv("test_set.csv", header = TRUE)

# Convert target to factor
training.data$Impurity.Type <- as.factor(training.data$Impurity.Type)

# Remove "Impurity.Percent" as a predictor (it isn't in the test set)
train_data <- training.data[, !names(training.data) %in% c("Impurity.Percent")]



set.seed(123)

# Define the tuning grid for the mtry parameter / how many branches is optimal
n_predictors <- ncol(train_data) - 1  
tuneGrid <- expand.grid(mtry = seq(2, n_predictors, by = 1))

# Define cross-validation settings 
train_control <- trainControl(method = "cv", number = 5)

# Train the random forest model using caret's
rf_caret_model <- train(Impurity.Type ~ ., 
                        data = train_data,
                        method = "rf",
                        metric = "Accuracy",
                        tuneGrid = tuneGrid,
                        trControl = train_control,
                        ntree = 10000)

print(rf_caret_model)

# Extract the underlying random forest model for further evaluation and feature importance
rf_model <- rf_caret_model$finalModel

#  Evaluate Accuracy
# Use the caret model to predict on the training data
train_preds <- predict(rf_caret_model, newdata = train_data)
conf_mat <- confusionMatrix(train_preds, train_data$Impurity.Type)
print(conf_mat)
cat("Accuracy:", conf_mat$overall["Accuracy"], "\n")

#Built-in Feature Importance
imp_builtin <- importance(rf_model)
print(imp_builtin)
varImpPlot(rf_model, main = "Built-in Feature Importance")

# Permutation Feature Importance
library(vip)
set.seed(123)
perm_imp <- vi_permute(rf_model, 
                       train = train_data, 
                       target = "Impurity.Type", 
                       metric = "accuracy", 
                       nsim = 10, 
                       sample_frac = 0.7,
                       pred_wrapper = function(object, newdata) {
                         predict(object, newdata = newdata, type = "class")
                       },
                       feature_names = colnames(train_data)[-which(names(train_data) == "Impurity.Type")])
print(perm_imp)
vip(perm_imp, num_features = 7, geom = "col", 
    aesthetics = list(fill = "steelblue")) +
  ggtitle("Permutation Feature Importance")

# SHAP (SHapley Additive exPlanations) Values 
library(iml)
predictor_rf <- Predictor$new(rf_model, 
                              data = train_data[, -which(names(train_data) == "Impurity.Type")], 
                              y = train_data$Impurity.Type,
                              type = "prob")
shap <- Shapley$new(predictor_rf, x.interest = train_data[1, -which(names(train_data) == "Impurity.Type")])
plot(shap) + ggtitle("SHAP Values for First Instance")

# Overview of SHAP values for a few observations
sample_rows <- train_data[sample(nrow(train_data), 5), -which(names(train_data) == "Impurity.Type")]
shap_all <- lapply(1:nrow(sample_rows), function(i) {
  Shapley$new(predictor_rf, x.interest = sample_rows[i, ])
})
lapply(shap_all, plot)