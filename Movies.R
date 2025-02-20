# PACKAGES
library(reshape2)
library(dplyr)
library(ggplot2)
library(stringr)

# LOAD DATA
movies.train = read.csv("ratings_train.csv", header=TRUE)
movies.test = read.csv("ratings_test.csv", header=TRUE)
movies.info = read.csv("movies.csv", header=TRUE)

# TEXT EXTRACTION
movies.train = arrange(movies.train, movieId)
movies = left_join(movies.train, movies.info)

# Extract the year of the movie
year = str_extract(movies$title, "\\(\\d{4}\\)")  # Extracts (YYYY)
year = str_replace_all(year, "[()]", "")   # Removes parentheses
year = as.integer(year) # Convert to integer
movies$title = year
movies = rename(movies, year = title)

# Extract the genre of the movie
# Assign a unique number to each unique genre combination
movies <- movies %>%
  mutate(genres = as.integer(factor(genres, levels = unique(genres))))

# Select a specific user (change user_id as needed)
user_id <- 4
user_data <- movies %>% filter(userId == user_id)

# Plot ratings over time with color-coded genres
ggplot(user_data, aes(x = movieId, y = rating, color = factor(genres))) +
  geom_point(size = 3) +  # Adjust point size
  labs(
    title = paste("Ratings of User", user_id),
    x = "Movie Id",
    y = "Rating",
    color = "Genre"
  ) +
  scale_color_manual(values = rainbow(length(user_data$genres))[length(user_data$genres):1])

# Plot ratings against genre
ggplot(user_data, aes(x = genres, y = rating)) +
  geom_point(size = 3) +  # Adjust point size
  labs(
    title = paste("Ratings of User", user_id),
    x = "Genre",
    y = "Rating",
    color = "Genre"
  ) +
  theme(legend.position = "none")



# PREDICTION FORMAT
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
