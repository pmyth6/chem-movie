# Load required libraries
library(reshape2)
library(class)
library(caret)

# Load data
ratings.train <- read.csv("ratings_train.csv", header = TRUE)
ratings.test <- read.csv("ratings_test.csv", header = TRUE)
movies <- read.csv("movies.csv", header = TRUE)

# Organize data into matrix format
X <- acast(ratings.train, userId ~ movieId, value.var = "rating", fill = NA)

# Prepare user and movie IDs
user.ids <- sort(unique(ratings.train$userId))
movie.ids <- sort(unique(ratings.train$movieId))

# Function to prepare KNN data (normalise)
prepare_knn_data <- function(ratings_matrix) {
  # Replace NA with column means
  for (j in 1:ncol(ratings_matrix)) {
    ratings_matrix[is.na(ratings_matrix[,j]), j] <- mean(ratings_matrix[,j], na.rm = TRUE)
  }
  
  # Scale the data
  scaled_matrix <- scale(ratings_matrix)
  return(scaled_matrix)
}

# Cross-validation function for KNN
knn_cv <- function(data, labels, k_values = c(3, 5, 7, 10, 15, 20)) {
  # Perform k-fold cross-validation
  set.seed(123)
  
  # Prepare results storage
  cv_results <- data.frame(k = numeric(), mse = numeric())
  
  # 5-fold cross-validation
  folds <- createFolds(labels, k = 5)
  
  for (k in k_values) {
    mse_fold <- numeric(length(folds))
    
    # For each fold
    for (i in 1:length(folds)) {
      # Split data
      test_indices <- folds[[i]]
      train_indices <- setdiff(1:length(labels), test_indices)
      
      # Prepare training and test sets
      train_data <- data[train_indices, ]
      test_data <- data[test_indices, ]
      train_labels <- labels[train_indices]
      test_labels <- labels[test_indices]
      
      # Predict using KNN
      predictions <- knn(train = train_data, 
                         test = test_data, 
                         cl = train_labels, 
                         k = k)
      
      # Calculate MSE for this fold
      mse_fold[i] <- mean((as.numeric(as.character(predictions)) - test_labels)^2)
    }
    
    # Store average MSE for this k
    cv_results <- rbind(cv_results, 
                        data.frame(k = k, 
                                   mse = mean(mse_fold)))
  }
  
  return(cv_results)
}

# Prepare data for KNN
# We'll use only movies and users that have enough ratings
movie_rating_counts <- colSums(!is.na(X))
user_rating_counts <- rowSums(!is.na(X))

# Filter for movies and users with at least 10 ratings
good_movies <- which(movie_rating_counts >= 10)
good_users <- which(user_rating_counts >= 10)

X_filtered <- X[good_users, good_movies]

# Prepare KNN input
knn_data <- prepare_knn_data(X_filtered)

# Use mean rating as labels for cross-validation
labels <- rowMeans(X_filtered, na.rm = TRUE)

# Perform cross-validation
cv_results <- knn_cv(knn_data, labels)

# Print and plot CV results
print(cv_results)
plot(cv_results$k, cv_results$mse, 
     type = "b", 
     xlab = "Number of Neighbors (k)", 
     ylab = "Mean Squared Error",
     main = "KNN Cross-Validation Results")

# Find the best k
best_k <- cv_results$k[which.min(cv_results$mse)]
print(paste("Best k:", best_k))
print(paste("Lowest MSE:", min(cv_results$mse)))

# Cross-validation function for KNN using entire dataset
knn_cv_full_dataset <- function(ratings_matrix, k_values = c(3, 5, 7, 10, 15, 20)) {
  # Comprehensive data preparation function
  prepare_data <- function(matrix) {
    # Create a copy of the matrix to avoid modifying original data
    imputed_matrix <- matrix
    
    # First, impute missing values with column (movie) means
    for (j in 1:ncol(imputed_matrix)) {
      col_mean <- mean(imputed_matrix[,j], na.rm = TRUE)
      imputed_matrix[is.na(imputed_matrix[,j]), j] <- col_mean
    }
    
    # Scale the imputed matrix
    scaled_matrix <- scale(imputed_matrix)
    return(scaled_matrix)
  }
  
  # Prepare results storage
  cv_results <- data.frame(k = numeric(), mse = numeric())
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Create folds manually to handle matrix structure
  n_rows <- nrow(ratings_matrix)
  folds <- createFolds(1:n_rows, k = 5)
  
  # Iterate through different k values
  for (k in k_values) {
    mse_fold <- numeric(length(folds))
    
    # Perform cross-validation
    for (i in 1:length(folds)) {
      # Split indices
      test_indices <- folds[[i]]
      train_indices <- setdiff(1:n_rows, test_indices)
      
      # Prepare labels (row means of original data)
      # Use original matrix to calculate labels to avoid data leakage
      train_labels <- rowMeans(ratings_matrix[train_indices,], na.rm = TRUE)
      test_labels <- rowMeans(ratings_matrix[test_indices,], na.rm = TRUE)
      
      # Prepare full matrix (impute and scale)
      full_prepared <- prepare_data(ratings_matrix)
      
      # Prepare training and test data
      train_data <- full_prepared[train_indices,]
      test_data <- full_prepared[test_indices,]
      
      print(anyNA(train_data))
      
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
  
  return(cv_results)
}

# Perform cross-validation on full dataset
full_cv_results <- knn_cv_full_dataset(X)

# Print and visualize results
print(full_cv_results)
plot(full_cv_results$k, full_cv_results$mse, 
     type = "b", 
     xlab = "Number of Neighbors (k)", 
     ylab = "Mean Squared Error",
     main = "KNN Cross-Validation on Full Dataset")

# Find the best k
best_k_full <- full_cv_results$k[which.min(full_cv_results$mse)]
print(paste("Best k for full dataset:", best_k_full))
print(paste("Lowest MSE for full dataset:", min(full_cv_results$mse)))

# Prepare final predictions (this is a placeholder and would need to be adapted)
# You'll need to ensure the test set matches the training set structure
num.test <- dim(ratings.test)[1]
predictions <- matrix(NA, num.test, 1)

# Write predictions to file
write.csv(predictions, file = "film_rating_predictions_group_X_week_Y.csv", row.names = FALSE)