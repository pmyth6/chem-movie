# Load packages
library(reshape2)
library(ggplot2)
library(stringr)
library(corrplot)
library(tidyr)
library(mvtnorm)
library(caret)
library(ModelMetrics)
library(dplyr)
library(FNN)

# Load Data
train = read.csv("ratings_train.csv", header=TRUE)
test = read.csv("ratings_test.csv", header=TRUE)

# Model function
# Takes in a training set and a test set
Modelw3 = function(train, test){
  # WEIGHTED AVERAGE FIRST
  test$rating = NA
  for (i in 1:length(test$userId)){
    # User average rating
    ind_u = which(train$userId == test$userId[i])
    mean_u = mean(train$rating[ind_u])
    # Movie average rating
    ind_m = which(train$movieId == test$movieId[i])
    if (length(ind_m) > 0){
      mean_m = mean(train$rating[ind_m])
    }
    else {
      mean_m = 0
    }
    # Number of user ratings
    n_u = length(ind_u)
    # Number of movie ratings
    n_m = length(ind_m)
    # Number of total ratings
    n = n_u + n_m
    # Calculate weighted average
    test$rating[i] = (n_u/n)*mean_u + (n_m/n)*mean_m
  }
  
  # KNN
  # Create train set matrix
  Mtrain = acast(train, userId ~ movieId, value.var = "rating", fill = NA)
  # Count the number of ratings for movies and users
  movie_rating_counts <- colSums(!is.na(Mtrain))
  user_rating_counts <- rowSums(!is.na(Mtrain))
  # Store the indices of movies and users with at least 10 ratings
  good_movies <- which(movie_rating_counts >= 10)
  good_users <- which(user_rating_counts >= 10)
  # Create a filtered train matrix
  Mtrain_f <- Mtrain[good_users, good_movies]
  # Replace NA values with column means (adding small noise to avoid bias)
  for (j in 1:ncol(Mtrain_f)) {
    Mtrain_f[is.na(Mtrain_f[, j]), j] <- rnorm(1, mean(Mtrain_f[, j], na.rm = TRUE), 0.05)
  }
  # Normalize train matrix
  Mtrainf_s = scale(Mtrain_f)
  # Use mean user rating as labels for knn
  labels <- rowMeans(Mtrain_f, na.rm = TRUE)
  # Create test set matrix (with missing ratings)
  Mtest = acast(test, userId ~ movieId, value.var = "rating", fill = NA)
  
  # Get names of good users and movies
  good_movie_names <- colnames(Mtrain)[good_movies]
  good_user_names <- rownames(Mtrain)[good_users]
  # Ensure we only keep movies and users present in both Mtrain and Mtest
  common_movies <- intersect(good_movie_names, colnames(Mtest))
  common_users <- intersect(good_user_names, rownames(Mtest))
  # Filter Mtest to include only common users/movies
  Mtest_f <- Mtest[common_users, common_movies, drop = FALSE]
  # Initialize results matrix
  Results <- matrix(NA, nrow = nrow(Mtest_f), ncol = ncol(Mtest_f))
  rownames(Results) <- rownames(Mtest_f)
  colnames(Results) <- colnames(Mtest_f)
  # Loop through each movie (column) in Mtest_f to predict missing values
  for (movie in colnames(Mtest_f)) {
    # Identify users who rated this movie in the training set
    train_indices <- !is.na(Mtrain_f[, movie])
    # If no training users for this movie, skip it
    if (sum(train_indices) == 0) next
    # Training feature set: all rated movies except the target movie
    train_features <- Mtrain_f[train_indices, , drop = FALSE]
    # Labels: actual ratings for the target movie
    train_labels <- Mtrain_f[train_indices, movie]
    # Identify users in the test set who need predictions
    test_indices <- is.na(Mtest_f[, movie])
    # If no test users need predictions, skip it
    if (sum(test_indices) == 0) next
    # Test feature set: users' ratings for other movies
    test_features <- Mtest_f[test_indices, , drop = FALSE]
    # Ensure each test user has at least one known rating for k-NN
    valid_test_indices <- rowSums(!is.na(test_features)) > 0
    if (sum(valid_test_indices) == 0) next
    # Select only valid test users
    test_features <- test_features[valid_test_indices, , drop = FALSE]
    # Run k-NN to predict ratings
    pred <- knn(train_features, test_features, train_labels, k = 7)
    # Store predictions in the results matrix
    Results[rownames(test_features), movie] <- as.numeric(as.character(pred))
  }
  
  # Accumulate results
  predictions = matrix(NA, nrow=length(test$userId), ncol=1)
  for (i in 1:length(test$userId)){
    user <- test$userId[i]
    movie <- test$movieId[i]
    # Check if both user and movie exist in Results
    if (user %in% rownames(Results) && movie %in% colnames(Results)) {
      # Use KNN prediction
      predictions[i] <- Results[user, movie]
    }
    else {
      predictions[i] = test$rating[i]
    }
  }
  return(predictions)
}










