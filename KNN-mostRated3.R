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

# Model function that combines weighted average and KNN approaches
# Takes in a training set and a test set
Modelw3 = function(train, test){
  # Create a copy of test to populate with predictions
  test_predictions <- test
  test_predictions$rating <- NA
  
  # STEP 1: WEIGHTED AVERAGE BASELINE
  for (i in 1:nrow(test)){
    # User average rating
    ind_u = which(train$userId == test$userId[i])
    mean_u = mean(train$rating[ind_u])
    
    # Movie average rating
    ind_m = which(train$movieId == test$movieId[i])
    if (length(ind_m) > 0){
      mean_m = mean(train$rating[ind_m])
    } else {
      mean_m = mean(train$rating) # Default to global mean if movie not found
    }
    
    # Number of user ratings
    n_u = length(ind_u)
    # Number of movie ratings
    n_m = length(ind_m)
    
    # Avoid division by zero
    if (n_u + n_m == 0) {
      test_predictions$rating[i] = mean(train$rating) # Global mean
    } else {
      # Calculate weighted average
      test_predictions$rating[i] = (n_u/(n_u + n_m))*mean_u + (n_m/(n_u + n_m))*mean_m
    }
  }
  
  # STEP 2: USER-BASED KNN IMPROVEMENT
  # Create ratings matrix (users as rows, movies as columns)
  ratings_matrix <- acast(train, userId ~ movieId, value.var = "rating", fill = NA)
  
  # Identify users with at least 10 ratings
  active_users <- names(which(rowSums(!is.na(ratings_matrix)) >= 10))
  
  # Identify movies with at least 10 ratings
  active_movies <- names(which(colSums(!is.na(ratings_matrix)) >= 10))
  
  # Create a user similarity matrix (using correlation)
  user_correlations <- matrix(NA, nrow = length(active_users), ncol = length(active_users))
  rownames(user_correlations) <- active_users
  colnames(user_correlations) <- active_users
  
  for (i in 1:length(active_users)) {
    for (j in i:length(active_users)) {
      user_i <- active_users[i]
      user_j <- active_users[j]
      
      # Find movies rated by both users
      common_movies <- intersect(
        colnames(ratings_matrix)[!is.na(ratings_matrix[user_i, ])],
        colnames(ratings_matrix)[!is.na(ratings_matrix[user_j, ])]
      )
      
      if (length(common_movies) >= 5) {  # At least 5 movies in common
        sim <- cor(ratings_matrix[user_i, common_movies], ratings_matrix[user_j, common_movies])
        user_correlations[user_i, user_j] <- sim
        user_correlations[user_j, user_i] <- sim
      }
    }
  }
  
  # Loop through test set to make KNN predictions where possible
  for (i in 1:nrow(test)) {
    user_id <- as.character(test$userId[i])
    movie_id <- as.character(test$movieId[i])
    
    # Check if user and movie meet our criteria
    if (user_id %in% active_users && movie_id %in% active_movies) {
      # Find users who rated this movie
      movie_raters <- rownames(ratings_matrix)[!is.na(ratings_matrix[, movie_id])]
      
      # Find k nearest neighbors among those who rated the movie
      if (length(movie_raters) > 0 && user_id %in% rownames(user_correlations)) {
        similarities <- user_correlations[user_id, intersect(movie_raters, active_users)]
        
        # Filter out NA similarities and sort
        valid_similarities <- similarities[!is.na(similarities)]
        if (length(valid_similarities) >= 3) { # Need at least 3 neighbors
          sorted_similarities <- sort(valid_similarities, decreasing = TRUE)
          
          # Take top k neighbors (or all if fewer than k)
          k <- min(7, length(sorted_similarities))
          top_neighbors <- names(sorted_similarities[1:k])
          
          # Weight by similarity
          neighbor_ratings <- ratings_matrix[top_neighbors, movie_id]
          neighbor_similarities <- sorted_similarities[1:k]
          
          # Ensure only positive correlations influence the prediction
          valid_indices <- neighbor_similarities > 0
          if (sum(valid_indices) > 0) {
            weighted_sum <- sum(neighbor_ratings[valid_indices] * 
                                  neighbor_similarities[valid_indices])
            sum_weights <- sum(neighbor_similarities[valid_indices])
            
            # Replace weighted average with KNN prediction
            test_predictions$rating[i] <- weighted_sum / sum_weights
          }
        }
      }
    }
  }
  
  # Ensure predictions are within valid rating range (1-5)
  test_predictions$rating <- pmin(5, pmax(1, test_predictions$rating))
  
  return(test_predictions$rating)
}

# Load Data
train = read.csv("ratings_train.csv", header=TRUE)
test = read.csv("ratings_test.csv", header=TRUE)

# Cross validate
folds = createFolds(train$userId, 5)

# Fold 1
train1 = train[c(folds$Fold2,folds$Fold3,folds$Fold4,folds$Fold5),]
test1 = train[folds$Fold1,]
true_ratings = test1$rating
test1$rating = NA
predicted_ratings = Modelw3(train1, test1)

MSE1 = mse(true_ratings, predicted_ratings)

# Fold 2
train2 = train[c(folds$Fold1,folds$Fold3,folds$Fold4,folds$Fold5),]
test2 = train[folds$Fold2,]
true_ratings2 = test2$rating
test2$rating = NA
predicted_ratings2 = Modelw3(train2, test2)

MSE2 = mse(true_ratings2, predicted_ratings2)

# Fold 3
train3 = train[c(folds$Fold1,folds$Fold2,folds$Fold4,folds$Fold5),]
test3 = train[folds$Fold3,]
true_ratings3 = test3$rating
test3$rating = NA
predicted_ratings3 = Modelw3(train3, test3)

MSE3 = mse(true_ratings3, predicted_ratings3)

# Fold 4
train4 = train[c(folds$Fold1,folds$Fold2,folds$Fold3,folds$Fold5),]
test4 = train[folds$Fold4,]
true_ratings4 = test4$rating
test4$rating = NA
predicted_ratings4 = Modelw3(train4, test4)

MSE4 = mse(true_ratings4, predicted_ratings4)

# Fold 5
train5 = train[c(folds$Fold1,folds$Fold2,folds$Fold3,folds$Fold4),]
test5 = train[folds$Fold5,]
true_ratings5 = test5$rating
test5$rating = NA
predicted_ratings5 = Modelw3(train5, test5)

MSE5 = mse(true_ratings5, predicted_ratings5)

mean(c(MSE1, MSE2, MSE3, MSE4, MSE5))

# Test predictions
train = read.csv("ratings_train.csv", header=TRUE)
test = read.csv("ratings_test.csv", header=TRUE)
predicted_ratings = Modelw3(train, test)
write.csv(predicted_ratings, file = "film_rating_predictions_group_F_week_3.csv", row.names=FALSE)

