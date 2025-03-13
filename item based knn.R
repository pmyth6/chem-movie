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
  
  # STEP 2: movie-BASED KNN IMPROVEMENT
  # Create ratings matrix (movie as rows, users as columns)
  ratings_matrix <- acast(train, movieId ~ userId, value.var = "rating", fill = NA)
  
  # Identify movies with at least 10 ratings
  active_movies <- names(which(rowSums(!is.na(ratings_matrix)) >= 10))
  
  # Identify users with at least 10 ratings
  active_users <- names(which(colSums(!is.na(ratings_matrix)) >= 10))
  
  # Create a movie similarity matrix (using correlation)
  movie_correlations <- matrix(NA, nrow = length(active_movies), ncol = length(active_movies))
  rownames(movie_correlations) <- active_movies
  colnames(movie_correlations) <- active_movies
  
  for (i in 1:length(active_movies)) {
    for (j in i:length(active_movies)) {
      movie_i <- active_movies[i]
      movie_j <- active_movies[j]
      
      # Find users taht watch both movies 
      common_users <- intersect(
        colnames(ratings_matrix)[!is.na(ratings_matrix[movie_i, ])],
        colnames(ratings_matrix)[!is.na(ratings_matrix[movie_j, ])]
      )
      
      if (length(common_users) >= 5) {  # At least 5 users in common
        ratings_i <- ratings_matrix[movie_i, common_users]
        ratings_j <- ratings_matrix[movie_j, common_users]
        
        # Check if standard deviation is zero before computing correlation
        if (sd(ratings_i, na.rm = TRUE) == 0 || sd(ratings_j, na.rm = TRUE) == 0) {
          sim <- NA  # No valid correlation if no variation
        } else {
          sim <- cor(ratings_i, ratings_j, use = "pairwise.complete.obs")  #correlation with pairwise obs to avoid errors 
        }
        movie_correlations[movie_i, movie_j] <- sim
        movie_correlations[movie_j, movie_i] <- sim
      }
    }
  }
  
  # Loop through test set to make KNN predictions where possible
  for (i in 1:nrow(test)) {
    movie_id <- as.character(test$movieId[i])
    user_id <- as.character(test$userId[i])
    
    # Check if user and movie meet our criteria
    if (movie_id %in% active_movies && user_id %in% active_users) {
      # Find users that have rated the movies 
      user_rated_movies <- rownames(ratings_matrix)[!is.na(ratings_matrix[, user_id])]
      
      
      valid_movies = intersect(user_rated_movies, active_movies)
      # Find k nearest neighbors between movies 
      if (length(user_rated_movies) > 0 && movie_id %in% rownames(movie_correlations)) {
        valid_movies = intersect(user_rated_movies, active_movies) #define valid movies before similarities to avoid NAs
        if(length(valid_movies)>0){
          similarities <- movie_correlations[movie_id, valid_movies]
        }else{
          similarities = c()
        }
        
        
        # Filter out NA similarities and sort
        valid_similarities <- similarities[!is.na(similarities)]
        if (length(valid_similarities) >= 3) { # Need at least 3 neighbors
          sorted_similarities <- sort(valid_similarities, decreasing = TRUE)
          
          # Take top k neighbors (or all if fewer than k)
          k <- min(7, length(sorted_similarities))
          top_neighbors <- names(sorted_similarities[1:k])
          
          # Weight by similarity
          neighbor_ratings <- ratings_matrix[top_neighbors, user_id]
          neighbor_similarities <- sorted_similarities[1:k]
          
          # Ensure only positive correlations influence the prediction and non na
          valid_indices <- neighbor_similarities > 0 &!is.na(neighbor_ratings)
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

Modelw3(train,test)


train = read.csv("ratings_train.csv", header=TRUE)
test = read.csv("ratings_test.csv", header=TRUE)

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



