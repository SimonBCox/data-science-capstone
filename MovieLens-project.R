<<<<<<< HEAD
##################################
# Create edx set, validation set #
##################################
# The code until line 42 is created by the course instructors
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(dplyr)
library(matrixStats)

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Clean up memory by removing data sets which will not be used anymore
rm(dl, ratings, movies, test_index, temp, movielens, removed)

###############################
# Movie Recommendation System #
###############################
# Function to calculate the RMSE
calculate_RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# calculate average rating of training set, mu will be used in all four models
mu <- mean(edx$rating)

## ------------------
## Movie effect model
## ------------------
# calculate the movie specific bias b_i and store in movie_avgs
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# predict ratings for the validation set
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%                  # add column b_i
  mutate(pred = mu + b_i) %>%
  pull(pred)

# create table and add first RMSE
summary <- tibble(Method = "Movie effect model", RMSE = calculate_RMSE(predicted_ratings, validation$rating))
summary

## ---------------------------
## Movie and user effect model
## ---------------------------
# calculate the user specific bias (b_u)
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%                  # add column b_i
  group_by(userId) %>%                     
  summarize(b_u = mean(rating - mu - b_i))

# predict ratings for the validation set
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%                  # add column b_i
  left_join(user_avgs, by='userId') %>%                    # add column b_u
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# add RMSE to summary
summary <- add_row(summary, Method = "Movie and user effect model", RMSE = calculate_RMSE(predicted_ratings, validation$rating))
summary

## -----------------------------------
## Movie, user and genres effect model
## -----------------------------------
# calculate the genres specific bias b_g
genre_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%                  # add column b_i
  left_join(user_avgs, by='userId') %>%                    # add column b_u
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

# predict ratings for the validation set
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%                  # add column b_i
  left_join(user_avgs, by='userId') %>%                    # add column b_u
  left_join(genre_avgs, by='genres') %>%                   # add column b_g
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

# add RMSE to summary
summary <- add_row(summary, Method = "Movie, user and genre effect model", RMSE = calculate_RMSE(predicted_ratings, validation$rating))
summary

## -----------------------------------------------
## Regularized movie, user and genres effect model
## -----------------------------------------------
# function which takes lambda as input and calculates the RMSE for the regularized model
calculate_RMUGEM <- function(l){
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  b_g <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(calculate_RMSE(predicted_ratings, validation$rating))
}

# create sequence of 50 lamdba values
lambdas <- seq(2.5, 7.5, 0.1)

# calcualte RMSE for all lambda values
RMSEs <- sapply(lambdas, calculate_RMUGEM)

# make a plot of the lambdas against the RMSE
qplot(lambdas, RMSEs) 

# add minimum RMSE to summary
summary <- add_row(summary, Method = "Reg. movie, user and genre effect model", RMSE = min(rmses))
options(digits=5) # set the number of significant digits to 5 for nice output
summary %>% knitr::kable()

=======
##################################
# Create edx set, validation set #
##################################
# The code until line 42 is created by the course instructors
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(dplyr)
library(matrixStats)

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Clean up memory by removing data sets which will not be used anymore
rm(dl, ratings, movies, test_index, temp, movielens, removed)

###############################
# Movie Recommendation System #
###############################
# Function to calculate the RMSE
calculate_RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# calculate average rating of training set, mu will be used in all four models
mu <- mean(edx$rating)

## ------------------
## Movie effect model
## ------------------
# calculate the movie specific bias b_i and store in movie_avgs
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# predict ratings for the validation set
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%                  # add column b_i
  mutate(pred = mu + b_i) %>%
  pull(pred)

# create table and add first RMSE
summary <- tibble(Method = "Movie effect model", RMSE = calculate_RMSE(predicted_ratings, validation$rating))
summary

## ---------------------------
## Movie and user effect model
## ---------------------------
# calculate the user specific bias (b_u)
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%                  # add column b_i
  group_by(userId) %>%                     
  summarize(b_u = mean(rating - mu - b_i))

# predict ratings for the validation set
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%                  # add column b_i
  left_join(user_avgs, by='userId') %>%                    # add column b_u
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# add RMSE to summary
summary <- add_row(summary, Method = "Movie and user effect model", RMSE = calculate_RMSE(predicted_ratings, validation$rating))
summary

## -----------------------------------
## Movie, user and genres effect model
## -----------------------------------
# calculate the genres specific bias b_g
genre_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%                  # add column b_i
  left_join(user_avgs, by='userId') %>%                    # add column b_u
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

# predict ratings for the validation set
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%                  # add column b_i
  left_join(user_avgs, by='userId') %>%                    # add column b_u
  left_join(genre_avgs, by='genres') %>%                   # add column b_g
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

# add RMSE to summary
summary <- add_row(summary, Method = "Movie, user and genre effect model", RMSE = calculate_RMSE(predicted_ratings, validation$rating))
summary

## -----------------------------------------------
## Regularized movie, user and genres effect model
## -----------------------------------------------
# function which takes lambda as input and calculates the RMSE for the regularized model
calculate_RMUGEM <- function(l){
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  b_g <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(calculate_RMSE(predicted_ratings, validation$rating))
}

# create sequence of 50 lamdba values
lambdas <- seq(2.5, 7.5, 0.1)

# calcualte RMSE for all lambda values
RMSEs <- sapply(lambdas, calculate_RMUGEM)

# make a plot of the lambdas against the RMSE
qplot(lambdas, RMSEs) 

# add minimum RMSE to summary
summary <- add_row(summary, Method = "Reg. movie, user and genre effect model", RMSE = min(rmses))
options(digits=5) # set the number of significant digits to 5 for nice output
summary %>% knitr::kable()

>>>>>>> a7355144d4c4dcd3d533155ab528c590c10080c1
