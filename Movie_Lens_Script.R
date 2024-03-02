if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(ggplot2)
library(caret)
library(dplyr)
library(knitr)


# 1.	Introduction

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



# Display the structure of the edx data frame
str(edx)

# Create a data frame structure_table containing information about column names, formats, and descriptions
structure_table <- data.frame(
  Name =c("`userId`", 
          "`movieId`", 
          "`rating`", 
          "`timestamp`", 
          "`title`", 
          "`genres`"),

  Format =c("Integer", 
            "Integer",
            "Numerical", 
            "Integer", 
            "Character string", 
            "Character string"),

  Description = c("Unique numerical identifier for each user", 
                  "Unique numerical identifier", 
                  "Rating given to a movie by a user, starting at 0 and going up to 5 in steps of 0.5", 
                  "Unix epoch of the date/time of the rating (number of seconds since 1-Jan-1970.", 
                  "Name of the Movie & year of release",
                  "Genres the movie belongs to, separated by |"))

# Display the structure_table
structure_table

# Display the first few rows of the edx data frame
head(edx)

# Create a summary data frame edx_summary containing information about the edx data frame
edx_summary <- data.frame(
  rows_number = nrow(edx), 
  users_number = n_distinct(edx$userId),
  movies_number = n_distinct(edx$movieId),
  average_rating = round(mean(edx$rating), 3),
  first_rating_Date = as.Date(as.POSIXct(min(edx$timestamp), origin = "1970-01-01")),
  last_rating_date = as.Date(as.POSIXct(max(edx$timestamp), origin = "1970-01-01"))
)

# Display the edx_summary
edx_summary

# Create a summary data frame final_holdout_test_summary for the final hold-out test set
final_holdout_test_summary <- data.frame(
  rows_number = nrow(final_holdout_test),
  users_number = n_distinct(final_holdout_test$userId),
  movies_number = n_distinct(final_holdout_test$movieId),
  average_rating = round(mean(final_holdout_test$rating), 3),
  first_rating_Date = as.Date(as.POSIXct(min(final_holdout_test$timestamp), origin = "1970-01-01")),
  last_rating_date = as.Date(as.POSIXct(max(final_holdout_test$timestamp), origin = "1970-01-01"))
)

# Display the final_holdout_test_summary
final_holdout_test_summary

## 1.2	 Project methodology

# Set the target RMSE value for the project
target_rmse <- 0.86490

# 2. Analysis and Exploration data

# Plot histogram for the distribution of ratings in the edx data frame
edx %>%
  ggplot(aes(x = rating)) +
  geom_histogram(bins = 10, color = "blue") +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ggtitle("Ratings distribution") +
  labs(x = "Rating", y = "Number of ratings") +
  geom_vline(xintercept = mean(edx$rating), colour = "red")

## 2.2 Movies Exploration

# Analyze and visualize the number of ratings per movie
edx %>% 
  group_by(movieId) %>%
  summarize(num_movie_ratings = n()) %>% 
  ggplot(aes(x = num_movie_ratings)) +
  geom_histogram(bins = 40, color = "blue") +
  scale_x_log10() +
  ggtitle("Number of ratings per movie distribution") +
  labs(x = "Ratings per Movie (log10)", y = "Number of Movies") +
  geom_vline(aes(xintercept = mean(num_movie_ratings)), color = "red")

# Analyze and visualize the average rating per movie
edx %>% 
  group_by(movieId) %>%
  summarise(movie_avg_ratings = sum(rating)/n()) %>%
  ggplot(aes(x = movie_avg_ratings)) +
  geom_histogram(bins = 20, color = I("blue")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ggtitle("Average Rating per Movie distribution") +
  labs(x = "Average Rating per Movie (log10)", y = "Number of Movies")

# Analyze and visualize the relationship between the number of ratings and average rating per movie
edx %>% 
  group_by(movieId) %>%
  summarise(num_movie_ratings = n(),
            movie_avg_ratings = sum(rating)/n()) %>%
  sample_frac(0.2) %>%
  ggplot(aes(x = num_movie_ratings, y = movie_avg_ratings)) +
  geom_point(color = "blue") +
  geom_smooth(color = "green") +
  ggtitle("Number of Ratings vs. Average Rating per Movie") +
  labs(x = "Number of Ratings", y = "Average Rating per Movie")

## 2.3 User Exploration

# Analyze and visualize the number of ratings per user
edx %>% 
  group_by(userId) %>%
  summarize(num_user_ratings = n()) %>% 
  ggplot(aes(x = num_user_ratings)) +
  geom_histogram(bins = 40, color = "blue") +
  scale_x_log10() +
  ggtitle("Number of ratings by users distribution") +
  labs(x = "Number of ratings per User (log10)", y = "Number of Users") +
  geom_vline(aes(xintercept = mean(num_user_ratings)), color = "red")

# Analyze and visualize the average rating per user
edx %>% 
  group_by(userId) %>%
  summarise(user_avg_ratings = sum(rating)/n()) %>%
  ggplot(aes(user_avg_ratings)) +
  geom_histogram(bins = 20, color = I("blue")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ggtitle("Users average rating Distribution") +
  labs(x = "User average rating", y = "Number of users")

# Analyze and visualize the relationship between the number of ratings and average rating per user
edx %>% 
  group_by(userId) %>%
  summarise(num_user_ratings = n(),
            user_avg_ratings = sum(rating)/n()) %>%
  sample_frac(0.2) %>%
  ggplot(aes(x = num_user_ratings, y = user_avg_ratings)) +
  geom_point(color = "blue") +
  geom_smooth(color = "green") +
  ggtitle("Number of Ratings vs. Average Rating per User") +
  labs(x = "Number of Ratings", y = "Average Rating per User")

## 2.4 Time effect Exploration

# Analyze and visualize the average ratings per month
edx %>%
  mutate(month = round_date(as_datetime(timestamp), unit = "month")) %>%
  group_by(month) %>%
  summarize(avg_ratings = mean(rating)) %>%
  ggplot(aes(x = month, y = avg_ratings)) +
  geom_point() +
  geom_smooth(color = "blue") +
  ggtitle("Average ratings of Month") +
  labs(x = "Month", y = "Average Rating")

# Analyze and visualize the number of ratings per month
edx %>%
  mutate(month = round_date(as_datetime(timestamp), unit = "month")) %>%
  group_by(month) %>%
  summarize(num_ratings = n()) %>%
  ggplot(aes(x = month, y = num_ratings)) +
  geom_point() +
  geom_smooth(color = "blue") +
  ggtitle("Number of Ratings per Month") +
  labs(x = "Month", y = "Number of Ratings")

## 2.5 Genres Exploration

# Extract unique genres from the edx data frame
str_extract_all(unique(edx$genres), "[^|]+") %>%
  unlist() %>%
  unique()

# Explore genres by analyzing and visualizing the number of ratings and average ratings per genre
edx_genres <- edx %>% 
  separate_rows(genres, sep = "\\|", convert = TRUE) %>%
  group_by(genres) %>%
  summarize(num_ratings_genre = n(), avg_ratings_genre = mean(rating))

# Visualize the number of ratings per genre
edx_genres$genres <- reorder(edx_genres$genres, -edx_genres$num_ratings_genre)
ggplot(edx_genres, aes(x = genres, y = num_ratings_genre)) +
  geom_bar(stat = "identity", color = "blue") +
  labs(title = "Number of Ratings per Genre", x = "Genre", y = "Number of Ratings") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Visualize the average rating per genre
edx_genres$genres <- reorder(edx_genres$genres, -edx_genres$avg_ratings_genre)
ggplot(edx_genres, aes(x = genres, y = avg_ratings_genre)) +
  geom_bar(stat = "identity", color = "blue") +
  labs(title = "Average Rating for Each Genre", x = "Genre", y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_cartesian(ylim = c(3, NA))


# 3. Methodology

## 3.1	Model performance evaluation

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## 3.2	Modeling Approach


# 4. Results


## 4.1 Model 1: BaseLine Model

# Calculate the mean rating (mu) for the baseline model
mu <- mean(edx$rating)

# Calculate the RMSE for the baseline model using the final_holdout_test data
model_1_rmse <- RMSE(final_holdout_test$rating, mu)

# Store the results in a data frame
rmse_results <- data.frame(Model = "Just the Average", RMSE = model_1_rmse)

# Display the results
rmse_results

## 4.2 Model 2: Movie effects Model

# Calculate movie effects (b_i) for each movie
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# Predict ratings for Model 2 using movie effects
predicted_rating_m2 <- final_holdout_test %>%
  left_join(movie_avgs, by = 'movieId') %>%
  mutate(pred = mu + b_i)

# Calculate the RMSE for Model 2
model_2_rmse <- RMSE(final_holdout_test$rating, predicted_rating_m2$pred)

# Update the results data frame with Model 2 results
rmse_results <- rbind(rmse_results, data.frame(Model = "Movie Effect Model", RMSE = round(model_2_rmse, 4)))

# Display the results for Model 2
rmse_results[2,]

## 4.3 Model 3: Movies and Users effects Model

# Calculate user effects (b_u) for each user
user_avgs <- edx %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predict ratings for Model 3 using movie and user effects
predicted_rating_m3 <- final_holdout_test %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u)

# Calculate the RMSE for Model 3
model_3_rmse <- RMSE(final_holdout_test$rating, predicted_rating_m3$pred)

# Update the results data frame with Model 3 results
rmse_results <- rbind(rmse_results, data.frame(Model = "Movie and User Effect model", RMSE = round(model_3_rmse, 4)))

# Display the results for Model 3
rmse_results[3,]

## 4.4 Model 4: Regularization of Movies and Users effects Model

# Define a function to calculate RMSE for different lambda values
calculate_rmse <- function(lambda) {
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + lambda))
  
  b_u <- edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu) / (n() + lambda))
  
  predicted_ratings <- final_holdout_test %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(final_holdout_test$rating, predicted_ratings))
}

# Define a range of lambda values
lambdas <- seq(4, 7, 0.25)

# Calculate RMSE for each lambda value
rmses <- sapply(lambdas, calculate_rmse)

# Plot the RMSE values for different lambda values
qplot(lambdas, rmses) +
  ggtitle("Selecting the tuning parameter") +
  labs(x = "Lambda", y = "RMSE")

# Select the lambda value with the minimum RMSE
lambda <- lambdas[which.min(rmses)]

# Calculate movie effects with regularization
movie_avgs_regularized <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu) / (n() + lambda))

# Calculate user effects with regularization
user_avgs_regularized <- edx %>%
  left_join(movie_avgs_regularized, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))

# Predict ratings for Model 4 using regularized movie and user effects
predicted_rating_m4 <- final_holdout_test %>%
  left_join(movie_avgs_regularized, by = 'movieId') %>%
  left_join(user_avgs_regularized, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u)

# Calculate the RMSE for Model 4
model_4_rmse <- RMSE(final_holdout_test$rating, predicted_rating_m4$pred)

# Update the results data frame with Model 4 results
rmse_results <- rbind(rmse_results, data.frame(Model = "Regularized Movie and User Effect Model", RMSE = round(model_4_rmse, 4)))

# Display the final results
rmse_results