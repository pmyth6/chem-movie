# Load required libraries
library(reshape2)
library(class)
library(caret)

# Load data
ratings.train <- read.csv("ratings_train.csv", header = TRUE)
ratings.test <- read.csv("ratings_test.csv", header = TRUE)
ratings.test$rating = NA
movies <- read.csv("movies.csv", header = TRUE)

# Organize data into matrix format
# Rows are users
# Columns are movies
X <- acast(ratings.train, userId ~ movieId, value.var = "rating", fill = NA)
X_test = acast(ratings.test, userId ~ movieId, value.var = "rating", fill = NA)

# Prepare user and movie IDs
user.ids <- sort(unique(ratings.train$userId))
movie.ids <- sort(unique(ratings.train$movieId))
user.ids <- sort(unique(ratings.test$userId))
movie.ids <- sort(unique(ratings.test$movieId))

# Fill NA values with column (movie) means
for (j in 1:ncol(X)) {
  col_mean <- mean(X[,j], na.rm = TRUE)
  X[is.na(X[,j]), j] <- rnorm(1,col_mean,0.05)
}

# Normalise the matrix
X = scale(X)

# Create folds 
n_rows <- nrow(X)
folds <- createFolds(1:n_rows, k = 5)

# Set k values for knn
k_values = c(3, 5, 7, 10, 15, 20)

# Prepare results storage
cv_results <- data.frame(k = numeric(), mse = numeric())

# Iterate through different k values
for (k in k_values) {
  mse_fold <- numeric(length(folds))
  
  # Perform cross-validation
  for (i in 1:length(folds)) {
    # Split indices
    test_indices <- folds[[i]]
    train_indices <- setdiff(1:n_rows, test_indices)
    
    # Prepare labels
    train_labels <- rowMeans(X[train_indices,], na.rm = TRUE)
    test_labels <- rowMeans(X[test_indices,], na.rm = TRUE)
    
    # Prepare training and test data
    train_data <- X[train_indices,]
    test_data <- X[test_indices,]
    
    # Predict using KNN
    predictions <- knn(train = train_data, 
                       test = test_data, 
                       cl = train_labels, 
                       k = k)
    
    # Calculate MSE for this fold
    mse_fold[i] <- mean((as.numeric(as.character(predictions)) - test_labels)^2, na.rm = TRUE)
  }
  # Store average MSE for this k
  cv_results <- rbind(cv_results, 
                      data.frame(k = k, 
                                 mse = mean(mse_fold, na.rm = TRUE)))
}

# Print and visualize results
print(cv_results)
plot(cv_results$k, cv_results$mse, 
     type = "b", 
     xlab = "Number of Neighbors (k)", 
     ylab = "Mean Squared Error",
     main = "KNN Cross-Validation on Full Dataset")

# Find the best k
best_k <- cv_results$k[which.min(cv_results$mse)]
print(paste("Best k for full dataset:", best_k))
print(paste("Lowest MSE for full dataset:", min(cv_results$mse)))

# Calculate predictions for the test data set
train_labels <- rowMeans(X, na.rm = TRUE)
predictions <- knn(train = X, 
                   test = X_test, 
                   cl = train_labels, 
                   k = 7)
