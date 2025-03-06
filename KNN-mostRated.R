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
  # Create a subset of the users and movies with more than 10 ratings
  # Use this for knn predictions in the test set
  # Use k = 7 as found in cross validation previously
  # Create train set matrix
  Mtrain = acast(train, userId ~ movieId, value.var = "rating", fill = NA)
  # Count the number of ratings for movies and users
  movie_rating_counts <- colSums(!is.na(Mtrain))
  user_rating_counts <- rowSums(!is.na(Mtrain))
  # Store the indices of the movies and users with more than 10 ratings
  good_movies <- which(movie_rating_counts >= 10)
  good_users <- which(user_rating_counts >= 10)
  # Create a new matrix with only those movies and users
  Mtrain_f <- Mtrain[good_users, good_movies]
  # Replace NA with column means with a sd of 0.05
  for (j in 1:ncol(Mtrain_f)) {
    Mtrain_f[is.na(Mtrain_f[,j]), j] <- rnorm(1,mean(Mtrain_f[,j], na.rm = TRUE),0.05)
  }
  # Normalise to prepare for knn
  Mtrainf_s = scale(Mtrain_f)
  # Use mean rating as labels for cross-validation
  labels <- rowMeans(Mtrain_f, na.rm = TRUE)
  # Create test set matrix with blank ratings
  test$rating = NA
  Mtest = acast(test, userId ~ movieId, value.var = "rating", fill = NA)
  # Get the names of good users and movies
  good_movie_names <- colnames(Mtrain)[good_movies]
  good_user_names <- rownames(Mtrain)[good_users]
  # Create a filtered test matrix with only the "good" movies and users
  # Filter the test matrix using names (instead of indices)
  # Ensure that we only keep movies and users that are present in Mtest
  common_movies <- intersect(good_movie_names, colnames(Mtest))
  common_users <- intersect(good_user_names, rownames(Mtest))
  Mtest_f <- Mtest[common_users, common_movies, drop = FALSE]
  # Create a results matrix with the same column and row names as the test matrix
  Results = matrix(NA, nrow=nrow(Mtest_f), ncol=ncol(Mtest_f))
  Results = knn(Mtrainf_s, Mtest_f, labels, 7)
  
  # For the rest - use a weighted average (bimodal sampler when n large)
  
  # Return predictions as a list
}

# Cross validate

# Create predicitions