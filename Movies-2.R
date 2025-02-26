#Genres with the most variance?
#Movies with the most variance?
#Decision trees with majority vote
#Count how many movies in the test set that arent in the training set
#Bivariate normal with mean and var of users ratings and mean and var of movies ratings


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

# Use a vector to set each genre to 0 or 1
# Step 1: Extract all unique singular genres
uniqueGenres <- movies$genres %>%
  str_split("\\|") %>%
  unlist() %>%
  unique()

# Step 2: Create binary indicator columns for each genre
movies <- movies %>%
  mutate(id = row_number()) %>%  # Create an ID column for spreading
  separate_rows(genres, sep = "\\|") %>%  # Split genres into separate rows
  mutate(value = 1) %>%  # Assign a value of 1 to indicate presence
  pivot_wider(names_from = genres, values_from = value, values_fill = 0)  # Spread into wide format

movies = movies[,-7]

# Convert UNIX timestamp to date 
movies$date = as.Date(as.POSIXct(movies$timestamp, origin="1970-01-01"))
movies.test$date = as.Date(as.POSIXct(movies.test$timestamp, origin="1970-01-01"))

# Look at the correlation between user, rating, timestamp and year
# Plot histograms of the distribution of data in the training set and the
# test set
ggplot(movies, aes(x = year)) +
  geom_histogram(binwidth = 2, fill = "darkgrey", color = "black", alpha = 0.8) +
  labs(title = "Histogram of Year Movie Distributed", x = "Year", y = "Frequency") +
  theme_minimal()
ggplot(movies.test, aes(x = year)) +
  geom_histogram(binwidth = 2, fill = "darkgrey", color = "black", alpha = 0.8) +
  labs(title = "Histogram of Year Movie Distributed (Test Data)", x = "Year", 
       y = "Frequency") +
  theme_minimal()

ggplot(movies, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "darkgrey", color = "black", alpha = 0.8) +
  labs(title = "Histogram of Movie Ratings", x = "Rating", y = "Frequency") +
  theme_minimal()

# Plot ratings against date rated and year of movie
plot(movies$date, movies$rating, xlab="Date Rated", ylab="Rating",
     col = alpha("black", 0.2))
ind1 = which(movies$rating == 0.5)
ind2 = which(movies$rating == 1.5)
ind3 = which(movies$rating == 2.5)
ind4 = which(movies$rating == 3.5)
ind5 = which(movies$rating == 4.5)
time1 = min(movies$date[ind1])
time2 = min(movies$date[ind2])
time3 = min(movies$date[ind3])
time4 = min(movies$date[ind4])
time5 = min(movies$date[ind5])
rate.time = min(c(time1,time2,time3,time4,time5))
points(rep(rate.time, 100), seq(0, 6, length.out = 100), type="l", col="red")
text(rate.time - 950, 3.25, labels = paste(rate.time), col = "red", pos = 4)


plot(movies$year, movies$rating, xlab="Year Produced", ylab="Rating", 
     col = alpha("black", 0.2))

# Plot distribution of date in the training set and test set
ggplot(movies, aes(x = date)) +
  geom_histogram(fill = "darkgrey", color = "black", alpha = 0.8) +
  labs(title = "Histogram of Date Movie Rated", x = "Date Rated", y = "Frequency") +
  theme_minimal()
ggplot(movies.test, aes(x = date)) +
  geom_histogram(fill = "darkgrey", color = "black", alpha = 0.8) +
  labs(title = "Histogram of Date Movie Rated (Test Data)", x = "Date Rated", y = "Frequency") +
  theme_minimal()

# Calculate and visualise the linear correlation between rating, date,
# year and unique genre
M = cor(movies[c(3,4,5,7)], use = "complete.obs")
cor.mtest <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j])
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(movies[c(3,4,5,7)])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

# Create a new dataset with mean rating and variance per movieId
movie.stats <- movies %>%
  group_by(movieId) %>%
  summarise(
    mean_rating = mean(rating, na.rm = TRUE),  # Calculate mean rating
    var_rating = var(rating, na.rm = TRUE)     # Calculate variance of ratings
  ) %>%
  ungroup()

# Plot the mean and variance of the ratings for each movie
plot(movie.stats$movieId, movie.stats$mean_rating)
ind = which(movie.stats$movieId < 20000)
plot(movie.stats$movieId[ind], movie.stats$mean_rating[ind])
ind = which(movie.stats$movieId > 20000)
plot(movie.stats$movieId[ind], movie.stats$mean_rating[ind])

plot(movie.stats$movieId, sqrt(movie.stats$var_rating))
ind = which(movie.stats$movieId < 20000)
plot(movie.stats$movieId[ind], sqrt(movie.stats$var_rating[ind]))
ind = which(movie.stats$movieId > 20000)
plot(movie.stats$movieId[ind], sqrt(movie.stats$var_rating[ind]))

# Bivariate normal model
# Covariance function
covar = function(x,y){
  n.x = length(x)
  n.y = length(y)
  N = n.x+n.y
  sum = 0
  for (i in 1:n.x){
    for (j in 1:n.y){
      sum = sum + (x[i]-mean(x))*(y[j]-mean(y))
    }
  }
  return(sum/(N-1))
}
# Try on user 1 with movieID 31 (rated 2.5)
user_id <- 1
user_data <- movies %>% filter(userId == user_id)
# variable u is the user's ratings
u = user_data$rating[2:15]
# variable m is the movie's ratings
indm = which(movies$movieId == 31)
m = movies$rating[indm]
# calculate the covariance between u and m
cov.um = covar(u,m)
cov.mu = covar(m,u)
# get sigma
Sigma = matrix(NA, 2, 2)
Sigma[1,1] = var(u)
Sigma[1,2] = max(c(0,cov.um))
Sigma[2,1] = max(c(0,cov.mu))
Sigma[2,2] = var(m)
# get mu
Mu = c(mean(u), mean(m))
# generate random samples
rand.samp = rmvnorm(1000, Mu, Sigma)
# plot the samples
plot(rand.samp[,1], rand.samp[,2])
# get our prediction
pred = mean((rand.samp[,1]+rand.samp[,2])/2)

# Bimodal normal model
bimodal.sampler = function(n,x,y){
  w_x = length(x)/(length(x)+length(y))
  w_y = length(y)/(length(x)+length(y))
  if (w_y == 0){
    return(w_x*rnorm(n, mean(x), sqrt(var(x))))
  }
  if (length(y) == 1){
    return(w_x*rnorm(n, mean(x), sqrt(var(x))) + w_y*rnorm(n, mean(y), 0))
  }
  if (w_y != 0){
    return(w_x*rnorm(n, mean(x), sqrt(var(x))) + w_y*rnorm(n, mean(y), sqrt(var(y))))
  }
}

# K folds cross validation
folds = createFolds(movies$userId, k=10, list=TRUE, returnTrain=FALSE)

MSE = 0
for (i in 1:length(folds)) {
  # Get test indices for this fold
  test_indices <- folds[[i]]
  
  # Create training and test sets
  train_data <- movies[-test_indices, ]
  test_data <- movies[test_indices, ]
  
  # Create a predictions vector
  predicted_ratings = 0
  
  # Fit the model
  for (j in 1:length(test_data$userId)){
    user_id <- test_data$userId[j]
    user_data <- train_data %>% filter(userId == user_id)
    x = user_data$rating
    indy = which(train_data$movieId == test_data$movieId[j])
    y = movies$rating[indy]
    predicted_ratings[j] = mean(bimodal.sampler(500, x, y))
  }
  
  # Evaluate Performance
  MSE[i] = mse(test_data$rating, predicted_ratings)
}
print(MSE)
mean(MSE)


# Select a specific user (change user_id as needed)
user_id <- 4
user_data <- movies %>% filter(userId == user_id)

# Plot ratings over time with color-coded genres
ggplot(user_data, aes(x = movieId, y = rating, color = factor(uniqueGenres))) +
  geom_point(size = 3) +  # Adjust point size
  labs(
    title = paste("Ratings of User", user_id),
    x = "Movie Id",
    y = "Rating",
    color = "Genre"
  ) +
  scale_color_manual(values = rainbow(length(user_data$uniqueGenres))[length(user_data$uniqueGenres):1])

# Plot ratings against genre
ggplot(user_data, aes(x = uniqueGenres, y = rating)) +
  geom_point(size = 3) +  # Adjust point size
  labs(
    title = paste("Ratings of User", user_id),
    x = "Genre",
    y = "Rating",
    color = "Genre"
  ) +
  theme(legend.position = "none")





# PREDICTION FORMAT WEEK 1
# ratings.train = read.csv("ratings_train.csv", header=TRUE)
# ratings.test = read.csv("ratings_test.csv", header=TRUE)
# X <- acast(ratings.train, userId ~ movieId, value.var="rating")
# print(X)
# 
# user.ids <- sort(unique(ratings.train$userId))
# movie.ids <- sort(unique(ratings.train$movieId))
# 
# num.test <- dim(ratings.test)[1]
# predictions <- matrix(NA, num.test, 1)
# for (i in 1:num.test){
#   if (!any(movie.ids==ratings.test[i,]$movieId)){ # if film does not arise in training set
#     predictions[i] <- 3
#     }
#   else
#     { # if film does arise in training set
#       movie.i.col.ind <- which(movie.ids==ratings.test[i,]$movieId)
#       predictions[i] <- mean(X[,movie.i.col.ind],na.rm=TRUE)
#       }
#   }
# write.csv(predictions, file = "film_rating_predictions_group_F_week_1.csv", row.names=FALSE)

# PREDICTIONS WEEK 2
# test_data = movies.test
# for (j in 1:length(test_data$userId)){
#   user_id <- test_data$userId[j]
#   user_data <- train_data %>% filter(userId == user_id)
#   x = user_data$rating
#   indy = which(train_data$movieId == test_data$movieId[j])
#   y = movies$rating[indy]
#   predicted_ratings[j] = mean(bimodal.sampler(500, x, y))
# }
# write.csv(predicted_ratings, file = "film_rating_predictions_group_F_week_2.csv", row.names=FALSE)
