# PACKAGES
library(reshape2)
library(ggplot2)
library(stringr)
library(corrplot)
library(tidyr)
library(mvtnorm)
library(caret)
library(ModelMetrics)
library(dplyr)

# LOAD DATA
movies.train = read.csv("ratings_train.csv", header=TRUE)
movies.test = read.csv("ratings_test.csv", header=TRUE)
movies.info = read.csv("movies.csv", header=TRUE)

# TEXT EXTRACTION
movies.train = arrange(movies.train, movieId)
movies = left_join(movies.train, movies.info)
movies.test = left_join(movies.test, movies.info)

# Extract the year of the movie
year = str_extract(movies$title, "\\(\\d{4}\\)")  # Extracts (YYYY)
year = str_replace_all(year, "[()]", "")   # Removes parentheses
year = as.integer(year) # Convert to integer
movies$title = year
movies = rename(movies, year = title)

year = str_extract(movies.test$title, "\\(\\d{4}\\)")  # Extracts (YYYY)
year = str_replace_all(year, "[()]", "")   # Removes parentheses
year = as.integer(year) # Convert to integer
movies.test$title = year
movies.test = rename(movies.test, year = title)

# Extract the genre of the movie
# Assign a unique number to each unique genre combination
movies <- movies %>%
  mutate(uniqueGenres = as.integer(factor(genres, levels = unique(genres))))

movies.test <- movies.test %>%
  mutate(uniqueGenres = as.integer(factor(genres, levels = unique(genres))))

# Put the data into a matrix
# Rows are users
# Columns are movies
RatingsMatrix <- acast(movies, userId ~ movieId, value.var="rating")

# Neighbourhood model
# Step 1: Baseline estimates
# Find b_u and b_i
optimize_biases <- function(RatingsMatrix, lambda = 10, max_iter = 20) {
  
  # Compute global average rating (mu)
  mu <- mean(RatingsMatrix, na.rm = TRUE)
  
  # Get user IDs and item IDs
  user_ids <- rownames(RatingsMatrix)
  item_ids <- colnames(RatingsMatrix)
  
  # Get number of users and items
  num_users <- nrow(RatingsMatrix)
  num_items <- ncol(RatingsMatrix)
  
  # Initialize biases
  b_u <- rep(0, num_users)  # User biases
  b_i <- rep(0, num_items)  # Item biases
  
  # Assign names for tracking
  names(b_u) <- user_ids
  names(b_i) <- item_ids
  
  for (iter in 1:max_iter) {
    # Update user biases
    for (u in 1:num_users) {
      rated_items <- !is.na(RatingsMatrix[u, ])
      if (any(rated_items)) {
        b_u[u] <- sum(RatingsMatrix[u, rated_items] - mu - b_i[rated_items]) / 
          (sum(rated_items) + lambda)
      }
    }
    
    # Update item biases
    for (i in 1:num_items) {
      rated_by_users <- !is.na(RatingsMatrix[, i])
      if (any(rated_by_users)) {
        b_i[i] <- sum(RatingsMatrix[rated_by_users, i] - mu - b_u[rated_by_users]) / 
          (sum(rated_by_users) + lambda)
      }
    }
  }
  
  return(list(b_u = b_u, b_i = b_i, mu = mu))
}

result <- optimize_biases(RatingsMatrix)
mu = result$mu
b_u = result$b_u
b_i = result$b_i

# Then to get a baseline estimate do mu +b_u +b_i
# Cross validate the baseline estimates with 10 folds
folds = createFolds(movies$userId, k=10, list=TRUE, returnTrain=FALSE)
MSE = matrix(NA, 10, 1)
for (i in 1:length(folds)){
  # Get test indices for this fold
  test_indices <- folds[[i]]
  
  # Create training and test sets
  train_data <- movies[-test_indices, ]
  test_data <- movies[test_indices, ]
  
  # Convert the training data to a matrix
  X = acast(train_data, userId ~ movieId, value.var="rating")
  
  # Calculate mu, b_u and b_i
  result = optimize_biases(X, lambda=100)
  mu = result$mu
  b_u = result$b_u
  b_i = result$b_i
  
  # Calculate predictions
  predictions = matrix(NA, length(test_data$userId), 1)
  for (j in 1:length(test_data$userId)){
    predictions[j] = mu + b_u[as.character(test_data$userId[1])] + 
      b_i[as.character(test_data$movieId[1])]
  }
  
  # Calculate MSE
  MSE[i] = mse(test_data$rating, predictions)
}
print(MSE)
mean(MSE)

# Baseline estimates are worse than weighted average - try with both and CV

# Step 2: Similarity measure
compute_item_similarity <- function(RatingsMatrix, lambda = 10) {
  num_items <- ncol(RatingsMatrix)
  item_names <- colnames(RatingsMatrix)
  
  # Initialize similarity matrix
  S <- matrix(0, nrow = num_items, ncol = num_items, dimnames = list(item_names, item_names))
  
  # Compute pairwise similarity
  for (i in 1:(num_items - 1)) {
    for (j in (i + 1):num_items) {
      
      # Get ratings for both items
      item_i <- RatingsMatrix[, i]
      item_j <- RatingsMatrix[, j]
      
      # Find users who rated both items
      common_users <- !is.na(item_i) & !is.na(item_j)
      n_ij <- sum(common_users)  # Number of users that rated both items
      
      if (n_ij > 1) {  # Pearson correlation requires at least 2 points
        rho_ij <- cor(item_i[common_users], item_j[common_users], use = "pairwise.complete.obs")
        
        # Compute similarity measure
        S_ij <- (n_ij / (n_ij + lambda)) * rho_ij
        
        # Store the similarity in the matrix
        S[i, j] <- S_ij
        S[j, i] <- S_ij  # Symmetric matrix
      }
    }
  }
  
  return(S)
}

S_matrix <- compute_item_similarity(RatingsMatrix, lambda = 10)

# View similarity for specific items
S_matrix["1", "5"]








