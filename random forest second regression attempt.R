install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)

training.data = read.csv("training_set.csv", header=TRUE)
test.data = read.csv("test_set.csv", header=TRUE)

#convert type to factor 
training.data$Impurity.Type = as.factor(training.data$Impurity.Type)

#remove impurity percent
train_data <- training.data[, !names(training.data) %in% c("Impurity.Percent")]



rf_cv = randomForest(Impurity.Percent~., data = training.data, mtry = 5, ntree = 500, nodesize= 4)

print(rf_cv)
par(mfrow=c(1,1)
plot(rf_cv)

summary(rf_cv)
mean(rf_cv$mse)

rf_cv$importance


###find the best number of trees


set.seed(123)
k= 10 # 10-fold cv

ntree_vals = seq(150,500, by= 5)

# Create k-folds
set.seed(123)
folds = sample(rep(1:k, length.out = nrow(train)))  


cv_results = data.frame(ntree = integer(), RMSE = numeric())

# Loop through each ntree value
for (ntree in ntree_vals) {
  rmse_vals = c()  
  
  #k-fold cross val
  for (i in 1:k) {
    # train and test 
    train_fold = training.data[folds != i, ]
    test_fold  = training.data[folds == i, ]
    
    # rf model 
    set.seed(123)
    rf_model = randomForest(Impurity.Percent ~ ., data = train_fold, ntree = ntree)
    

    predictions = predict(rf_model, test_fold)

    rmse = sqrt(mean((predictions - test_fold$Impurity.Percent)^2))
    

    rmse_vals = c(rmse_vals, rmse)
  }
  
  #  mean RMSE across k folds
  mean_rmse = mean(rmse_vals)
  

  cv_results = rbind(cv_results, data.frame(ntree = ntree, RMSE = mean_rmse))
}

print(cv_results)

# Find best ntree
best_ntree = cv_results[which.min(cv_results$RMSE), "ntree"]
print(paste("Best ntree:", best_ntree))

##best = 205

###now best mtry and nodesize 

set.seed(123)

##create a training and test set
split = sample(nrow(training.data), 0.7 * nrow(training.data))
train = training.data[split,]
test = training.data[-split,]


##cv for other parameters 

mtry.vals = c(1, 2, 3, 4, 5, 6, 7)
nodesize.vals = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
ntree = 295
k_folds = 10

set.seed(123)
n = nrow(train)
fold_indices = sample(1:n, size = n, replace = FALSE)  

# split into folds
folds = split(fold_indices, ceiling(seq_along(fold_indices) / (n / k_folds)))

# cross-validation
for(i in mtry.vals){
  for(j in nodesize.vals){
    
    rmse_vals = numeric(k_folds)  
    
    for(f in 1:k_folds){
      # training and test 
      test_indices = folds[[f]]
      train_data = train[-test_indices, ]
      test_data = train[test_indices, ]
      
      rf_model = randomForest(Impurity.Percent ~ ., data = train_data, mtry = i, ntree = ntree, nodesize = j)
      predictions = predict(rf_model, test_data)
      
      # rmse for this fold 
      rmse_vals[f] = sqrt(mean((predictions - test_data$Impurity.Percent)^2))
    }
    
    avg_rmse = mean(rmse_vals)
    
    results.cv = rbind(results.cv, c(i, j, round(avg_rmse, 4)))
  }
}

colnames(results.cv) = c("mtry", "nodesize", "RMSE")
print(results.cv)

min = which.min(results.cv$RMSE)


results.cv[min,]

###mtry = 4, nodesize = 3

rf_cv.best = randomForest(Impurity.Percent ~ ., data = training.data, mtry = 4, ntree = 205, nodesize = 3)

print(rf_cv.best)
plot(rf_cv.best)

train = training.data[,-1]
predictions = predict(rf_cv.best, train)

res = abs(predictions-training.data$Impurity.Percent)                      
mean(res)


###now max nodes
mtry.val = 4
nodesize.val = 3
maxnode.val = c(10,20,30,35,40,50,60)
ntree = 205
k_folds = 10

set.seed(123)
n = nrow(train)
fold_indices = sample(1:n, size = n, replace = FALSE)  

# split into folds
folds = split(fold_indices, ceiling(seq_along(fold_indices) / (n / k_folds)))

results.cv = data.frame(maxnodes = integer(), RMSE = numeric())

# cross-validation
for(i in maxnode.val){
    
    rmse_vals = numeric(k_folds)  
    
    for(f in 1:k_folds){
      # training and test 
      test_indices = folds[[f]]
      train_data = training.data[-test_indices, ]
      test_data = training.data[test_indices, ]
      
      rf_model = randomForest(Impurity.Percent ~ ., data = train_data, mtry = 4, ntree = 205, nodesize = 3, maxnodes = i)
      predictions = predict(rf_model, test_data)
      
      # rmse for this fold 
      rmse_vals[f] = sqrt(mean((predictions - test_data$Impurity.Percent)^2))
    }
    
    avg_rmse = mean(rmse_vals)
    
    results.cv = rbind(results.cv, c(i, round(avg_rmse, 4)))
  
}

colnames(results.cv) = c("maxnode", "RMSE")
print(results.cv)

min = which.min(results.cv$RMSE)
results.cv[min,]

##maxnode = 40

##final best model after cross validation 

rf_cv.best = randomForest(Impurity.Percent ~ ., data = training.data, mtry = 4, ntree = 205, nodesize = 3, maxnodes=40)

print(rf_cv.best)
plot(rf_cv.best)

train = training.data[,-1]
predictions = predict(rf_cv.best, train)

res = abs(predictions-training.data$Impurity.Percent)                      
mean(res)
#getting mean of about 0.30


