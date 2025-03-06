# KNN function for movie rating prediction
predict_ratings <- function(train_matrix, test_matrix, k = 7, mu = 3.5) {
  library(FNN)  # For knn implementation
  
  # Get unique movie IDs from both matrices
  train_movies <- colnames(train_matrix)
  test_movies <- colnames(test_matrix)
  
  # Create a results matrix with the same dimensions as test_matrix
  results <- matrix(NA, nrow = nrow(test_matrix), ncol = ncol(test_matrix))
  rownames(results) <- rownames(test_matrix)
  colnames(results) <- colnames(test_matrix)
  
  # Process each user in the test matrix
  for (i in 1:nrow(test_matrix)) {
    test_user <- rownames(test_matrix)[i]
    
    # Find common movies between training and test
    common_movies <- intersect(train_movies, test_movies)
    unknown_movies <- setdiff(test_movies, train_movies)
    
    if (length(common_movies) > 0) {
      # For movies that exist in the training set, use KNN
      
      # Extract subsets with common movies
      train_subset <- train_matrix[, common_movies, drop = FALSE]
      
      # Calculate distances between test user and all training users
      # We need to handle the case where the test user might not be in the training set
      if (test_user %in% rownames(train_matrix)) {
        # If test user exists in training, use their ratings as query
        test_user_ratings <- train_matrix[test_user, common_movies, drop = FALSE]
        
        # Remove the test user from training for unbiased prediction
        train_subset_without_test <- train_subset[rownames(train_subset) != test_user, , drop = FALSE]
        
        # Find k nearest neighbors excluding the test user
        user_distances <- apply(train_subset_without_test, 1, function(x) {
          sqrt(sum((x - as.numeric(test_user_ratings))^2, na.rm = TRUE))
        })
        
        neighbors <- names(sort(user_distances)[1:min(k, length(user_distances))])
      } else {
        # If test user doesn't exist in training, use NA pattern to find similar users
        test_user_pattern <- !is.na(test_matrix[test_user, common_movies])
        
        # Count matches in NA pattern for each training user
        user_similarities <- apply(train_subset, 1, function(x) {
          sum(!is.na(x) & test_user_pattern, na.rm = TRUE)
        })
        
        # Select users with most similar available rating patterns
        neighbors <- names(sort(user_similarities, decreasing = TRUE)[1:min(k, length(user_similarities))])
      }
      
      # Predict ratings for each movie in test set
      for (movie in common_movies) {
        if (is.na(test_matrix[test_user, movie])) {
          # Get ratings from neighbors for this movie
          neighbor_ratings <- train_matrix[neighbors, movie]
          neighbor_ratings <- neighbor_ratings[!is.na(neighbor_ratings)]
          
          if (length(neighbor_ratings) > 0) {
            # Calculate average rating from available neighbors
            results[test_user, movie] <- mean(neighbor_ratings)
          } else {
            # If no neighbors rated this movie, use global mean
            results[test_user, movie] <- mu
          }
        }
      }
    }
    
    # For movies not in training set, assign global mean
    if (length(unknown_movies) > 0) {
      for (movie in unknown_movies) {
        if (is.na(test_matrix[test_user, movie])) {
          results[test_user, movie] <- mu
        }
      }
    }
  }
  
  return(results)
}

# Example usage:
train_ratings <- read.csv("ratings_train.csv", header=TRUE)
test_ratings <- read.csv("ratings_test.csv", header=TRUE)
test_ratings$rating = NA
train_ratings <- acast(train_ratings, userId ~ movieId, value.var = "rating", fill = NA)
test_ratings = acast(test_ratings, userId ~ movieId, value.var = "rating", fill = NA)
predicted_ratings <- predict_ratings(train_ratings, test_ratings, k = 7, mu = 3.5)

test_ratings <- read.csv("ratings_test.csv", header=TRUE)
ratings = matrix(NA, length(test_ratings$userId))
for (i in 1:length(test_ratings$userId)){
  ratings[i] = predicted_ratings[as.character(test_ratings$userId[i]), 
                                 as.character(test_ratings$movieId[i])]
}

# DON'T FORGET TO FACTOR IN DATE AND HALF RATINGS!!!

plot(seq(1,length(test_ratings$userId), length.out=length(test_ratings$userId)), ratings)

ratings.train = read.csv("ratings_train.csv", header=TRUE)
ratings.test = read.csv("ratings_test.csv", header=TRUE)
X <- acast(ratings.train, userId ~ movieId, value.var="rating")
print(X)

user.ids <- sort(unique(ratings.train$userId))
movie.ids <- sort(unique(ratings.train$movieId))

num.test <- dim(ratings.test)[1]
predictions <- matrix(NA, num.test, 1)
for (i in 1:num.test){
  if (!any(movie.ids==ratings.test[i,]$movieId)){ # if film does not arise in training set
    predictions[i] <- 3
    }
  else
    { # if film does arise in training set
      movie.i.col.ind <- which(movie.ids==ratings.test[i,]$movieId)
      predictions[i] <- mean(X[,movie.i.col.ind],na.rm=TRUE)
      }
}

# Check using cross validation
# Fold 5
train_ratings <- read.csv("ratings_train.csv", header=TRUE)
folds = createFolds(train_ratings$userId, k=5)

train_set = train_ratings[c(folds$Fold1,folds$Fold2,folds$Fold3,folds$Fold4),]
test_set = train_ratings[folds$Fold5,]
true_ratings = test_set$rating
test_set$rating = NA
train_set = acast(train_set, userId ~ movieId, value.var = "rating", fill = NA)
test_set = acast(test_set, userId ~ movieId, value.var = "rating", fill = NA)
predicted_ratings <- predict_ratings(train_set, test_set, k = 7, mu = 3.5)

test_set = train_ratings[folds$Fold5,]
p_ratings = matrix(NA, length(test_set$userId))
for (i in 1:length(test_set$userId)){
  p_ratings[i] = predicted_ratings[as.character(test_set$userId[i]), 
                                 as.character(test_set$movieId[i])]
}

MSE_fold5 = mse(true_ratings, p_ratings)

# Fold 4
train_ratings <- read.csv("ratings_train.csv", header=TRUE)
folds = createFolds(train_ratings$userId, k=5)

train_set = train_ratings[c(folds$Fold1,folds$Fold2,folds$Fold3,folds$Fold5),]
test_set = train_ratings[folds$Fold4,]
true_ratings = test_set$rating
test_set$rating = NA
train_set = acast(train_set, userId ~ movieId, value.var = "rating", fill = NA)
test_set = acast(test_set, userId ~ movieId, value.var = "rating", fill = NA)
predicted_ratings <- predict_ratings(train_set, test_set, k = 7, mu = 3.5)

test_set = train_ratings[folds$Fold4,]
p_ratings = matrix(NA, length(test_set$userId))
for (i in 1:length(test_set$userId)){
  p_ratings[i] = predicted_ratings[as.character(test_set$userId[i]), 
                                   as.character(test_set$movieId[i])]
}

ind = which(as.Date(as.POSIXct(test_set$timestamp, origin="1970-01-01")) < "2003-05-16")
p_ratings[ind] = round(p_ratings[ind])
MSE_fold4.2 = mse(true_ratings, p_ratings)
sum(p_ratings != 3.5 & p_ratings != 4)
ind2 = which(p_ratings != 3.5 & p_ratings != 4)
p_ratings[ind2]
hist(p_ratings)
hist(true_ratings)
