## ----Setup, include=FALSE------------------------------------------------------------------------------------------
#Specifying the knitr global options for chunks of code
knitr::opts_chunk$set(echo = FALSE, 
                      eval=TRUE,
                      cache=FALSE, 
                      cache.lazy = FALSE,
                      message=FALSE,
                      warning=FALSE,
                      fig.align="center",
                      out.width = "75%",
                      out.height = "50%",
                      purl=TRUE
                      )


## ----Installing required packages----------------------------------------------------------------------------------
#removing cache/__packages
if (file.exists("cache/__packages")) unlink("cache/__packages")
#Installing required packages if there are not already installed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")


## ----Loading packages----------------------------------------------------------------------------------------------
#Loading required packages
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(kableExtra)
library(ggplot2)
library(ggthemes)


## ----Creating edx and validation set-------------------------------------------------------------------------------
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

#omitting the following part since I'm using R 4.0 or later
#please use the following part if you are using R 3.6 or earlier
# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = #as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))

#If you are using R 3.6 or earlier:
# - please omit the following part
# - instead, use the code above that has been commented out if
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                            title = as.character(title),
                                            genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)



## ----Gathering Info About Data-------------------------------------------------------------------------------------
#Finding column names in the data
edx_columns <- colnames(edx)
#Finding total number of movie ratings (i.e. rows) in MovieLens Dataset
n_ratings <- nrow(edx) + nrow(validation)
#Finding number of unique users
n_users <- length(unique(edx$userId))
#Finding number of unique movies
n_movies <- length(unique(edx$movieId))


## ----Table 1: Columns names with explanation-----------------------------------------------------------------------
#Write column names & explanations
columns <- c("userId", "movieId", "title", "genres", "rating", "timestamp")
column_explanations <- c("Unique identifier for each user",
                  "Unique identifier for each movie",
                  "Title of a movie",
                  "Genre of a movie - multiple genres are concatenated with a | sign such as Comedy|Romance",
                  "Rating given by a specific user to a specific movie",
                  "Date and time associated with each rating")

#Show columns & their explanations in a table
data.frame(columns, column_explanations) %>%
  kable(caption = "Columns", align = "cl") %>%
  column_spec(column = 1:2, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = "HOLD_position",
                position = "center")

#Remove stored data & values
rm(columns, column_explanations)


## ----Creating edxTrain and edxTest sets----------------------------------------------------------------------------
##########################################################
# Create edxTrain and edxTest sets from the edx set
##########################################################

# edxTest set will be 10% of MovieLens data like Validation set
set.seed(3, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(3)`
#p = 1/9 is chosen to make edxTest set similar in size to Validation set
edxTest_index <- createDataPartition(y = edx$rating, times = 1, p = 1/9, list = FALSE)
edxTrain <- edx[-edxTest_index,]
temp_test <- edx[edxTest_index,]

# Make sure userId and movieId in edxTest set are also in edxTrain set
edxTest <- temp_test %>% 
      semi_join(edxTrain, by = "movieId") %>%
      semi_join(edxTrain, by = "userId") 

# Add rows removed from edxTest set back into edxTrain set
removed <- anti_join(temp_test, edxTest)
edxTrain <- rbind(edxTrain, removed)

rm(edxTest_index, temp_test, removed)


## ----Creating release and rating year columns----------------------------------------------------------------------
#Create a column in edxTrain and edxTest sets for year of releases for movies
edxTrain$release_year <- as.numeric(str_sub(edxTrain$title, start = -5, end = -2))
edxTest$release_year <- as.numeric(str_sub(edxTest$title, start = -5, end = -2))

#Create a column in edxTrain and edxTest sets for year of ratings
edxTrain$rating_year <- year(as_datetime(edxTrain$timestamp))
edxTest$rating_year <- year(as_datetime(edxTest$timestamp))


## ----RMSE function-------------------------------------------------------------------------------------------------
#Define the RMSE function
rmse <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings)^2, na.rm=TRUE))
}



## ----Table 2: first 3 rows of edxTrain set-------------------------------------------------------------------------
#Put the first 3 rows of edxTrain set in a table
edxTrain %>% head(3) %>%
   kable(caption = "First 3 rows of edxTrain set", align="c") %>%
   column_spec(column = 1: ncol(edxTrain), border_left = TRUE, border_right = TRUE) %>%
   kable_styling(full_width=FALSE,
                 font_size = 6.7,
                 latex_options = c("HOLD_position","striped"),
                 position = "center")


## ----Table 3: first 3 rows of edxTest set--------------------------------------------------------------------------
#Put the first 3 rows of edxTest set in a table
edxTest %>% head(3) %>%
   kable(caption = "First 3 rows of edxTest set", align="c") %>%
   column_spec(column = 1: ncol(edxTest), border_left = TRUE, border_right = TRUE) %>%
   kable_styling(full_width=FALSE,
                 font_size = 8,
                 latex_options = c("HOLD_position","striped"),
                 position = "center")


## ----Figure 1 - Average Rating by Movie ID-------------------------------------------------------------------------
#Plot Average Rating given to each Movie
fig1 <- edx %>% 
  group_by(movieId) %>%
  summarize(avg_rating = mean(rating)) %>%
  select(movieId, avg_rating) %>%
  ggplot(aes(x=as.factor(movieId), y=avg_rating)) +
  geom_bar(stat = "identity")+
  labs(title = "Figure 1: Average Rating by Movie ID\n", x = "\nMovie ID", y = "Average Rating\n")+
  scale_x_discrete(breaks=NULL,
                   labels=NULL) +
  theme_economist()

fig1



## ----Figure 2 - Average Rating by User ID--------------------------------------------------------------------------
#Plot Average Rating given by each User
fig2 <- edx %>% 
  group_by(userId) %>%
  summarize(avg_rating = mean(rating)) %>%
  select(userId, avg_rating) %>%
  ggplot(aes(x=as.factor(userId), y=avg_rating)) +
  geom_bar(stat = "identity")+
  labs(title = "Figure 2: Average Rating by User ID\n", x = "\nUser ID", y = "Average Rating\n")+
  scale_x_discrete(breaks=NULL,
                   labels=NULL) +
  theme_economist()

fig2



## ----Figure 3 - Average Rating by Year of Release------------------------------------------------------------------
#Plot Average Rating for Year of Release of Each Movie
fig3 <- edx %>% 
  mutate(release_year = as.numeric(str_sub(edx$title, start = -5, end = -2))) %>% 
  group_by(release_year) %>%
  summarize(avg_rating = mean(rating)) %>%
  select(release_year, avg_rating) %>%
  ggplot(aes(x=release_year, y=avg_rating)) +
  geom_line(color="#FA8072")+
  geom_vline(xintercept = 1985)+
  geom_text(aes(1980, 3.93, label = "Year of\n1985"), data.frame())+
  labs(title = "Figure 3: Average Rating by Year of Release\n", x = "\nYears", y = "Average Rating\n")+
  theme_economist()

fig3


## ----Figure 4 - Average Rating by Year of Rating-------------------------------------------------------------------
#Plot Average Rating for Year of Each Rating in Dataset
fig4 <- edx %>% 
  mutate(rating_year = year(as_datetime(edx$timestamp))) %>% 
  group_by(rating_year) %>%
  summarize(avg_rating = mean(rating)) %>%
  select(rating_year, avg_rating) %>%
  ggplot(aes(x=rating_year, y=avg_rating)) +
  geom_line(color="#FA8072")+
  geom_vline(xintercept = 2000)+
  geom_text(aes(1999.1, 3.88, label = "Year of\n2000"), data.frame())+
  labs(title = "Figure 4: Average Rating by Year of Rating\n", x = "\nYears", y = "Average Rating\n")+
  theme_economist()

fig4



## ----Table 4: Top 5 Movies by Rating Frequency---------------------------------------------------------------------
#Show Top 5 Movies by Rating Frequency in a table
edx %>% 
  group_by(title) %>% 
  summarize(count = n()) %>%
  select(title, count) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  kable(caption = "Top 5 Movies by Rating Frequency", align="c") %>%
  column_spec(column = 1:2, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = c("HOLD_position","striped"),
                position = "center")



## ----Table 5: Bottom 5 Movies by Rating Frequency------------------------------------------------------------------
#Show Bottom 5 Movies by Rating Frequency in a table
edx %>% 
  group_by(title) %>% 
  summarize(count = n()) %>%
  select(title, count) %>%
  arrange(desc(-count)) %>%
  head(5) %>%
  kable(caption = "Bottom 5 Movies by Rating Frequency", align="c") %>%
  column_spec(column = 1:2, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = c("HOLD_position","striped"),
                position = "center")


## ----Table 6: Top 5 Users by Rating Frequency----------------------------------------------------------------------
#Show Top 5 Users by Rating Frequency in a table
edx %>% 
  group_by(userId) %>% 
  summarize(count = n()) %>%
  select(userId, count) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  kable(caption = "Top 5 Users by Rating Frequency", align="c") %>%
  column_spec(column = 1:2, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = c("HOLD_position","striped"),
                position = "center")


## ----Table 7: Bottom 5 Users by Rating Frequency-------------------------------------------------------------------
#Show Bottom 5 Users by Rating Frequency in a table
edx %>% 
  group_by(userId) %>% 
  summarize(count = n()) %>%
  select(userId, count) %>%
  arrange(desc(-count)) %>%
  head(5) %>%
  kable(caption = "Bottom 5 Users by Rating Frequency", align="c") %>%
  column_spec(column = 1:2, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = c("HOLD_position","striped"),
                position = "center")



## ----Results for edxTest & validation------------------------------------------------------------------------------
#Create Results table for the edxTest set to store RMSE of each model
results_edxTest <- data.frame(matrix(ncol = 2, nrow=0))
#Give column names
colnames(results_edxTest) <- c("Model", "RMSE")
#Change type of column Model to character
results_edxTest$Model <- as.character(results_edxTest$Model)
#Change type of column RMSE to double
results_edxTest$RMSE <- as.double(results_edxTest$RMSE)

#Create Results table for the validation set to store RMSE of each model
results_validation <- data.frame(matrix(ncol = 2, nrow=0))
#Give column names
colnames(results_validation) <- c("Model", "RMSE")
#Change type of column Model to character
results_validation$Model <- as.character(results_validation$Model)
#Change type of column RMSE to double
results_validation$RMSE <- as.double(results_validation$RMSE)



## ----Baseline Mean Model-------------------------------------------------------------------------------------------
#Find overall average rating
avg_rating <- mean(edxTrain$rating)

#Remove average rating to free up space
rm(avg_rating)


## ----Baseline Mean Model RMSE--------------------------------------------------------------------------------------
#This model uses Average Rating as the predicted rating for each Movie
#Find overall average rating
avg_rating <- mean(edxTrain$rating)
#Find RMSE for Baseline Mean Model using edxTest set
baseline_rmse <- RMSE(avg_rating, edxTest$rating)

#Store RMSE for edxTest set in edxTest Results table
results_edxTest <- bind_rows(results_edxTest, 
                              data.frame(
                                Model = "Baseline Mean Model",
                                RMSE = baseline_rmse)
                              )
#Remove duplicates from edxTest Results table in case code is run more than once
results_edxTest <- results_edxTest[!duplicated(results_edxTest[, c("Model","RMSE")]),]

#Find RMSE for Baseline Mean Model using Validation set
val_baseline_rmse <- RMSE(avg_rating, validation$rating)
#Store RMSE for Validation set in Validation Results table
results_validation <- bind_rows(results_validation , 
                              data.frame(
                                Model = "Baseline Mean Model",
                                RMSE = val_baseline_rmse)
                              )
#Remove duplicates from Validation Results table in case code is run more than once
results_validation <- results_validation[!duplicated(results_validation[, c("Model","RMSE")]),]

#Remove stored data or values to free up space
rm(avg_rating, baseline_rmse, val_baseline_rmse)


## ----Movie Model---------------------------------------------------------------------------------------------------
#This model considers impact of movies on ratings
#Find overall average rating
avg_rating <- mean(edxTrain$rating)

#Find the Average Effect (or bias) movies have on Ratings
movie_effects <- edxTrain %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - avg_rating))

#Predict ratings for Movie Model using edxTest set
movie_model_ratings <- edxTest %>% 
  left_join(movie_effects, by = "movieId") %>%
  mutate(pred_rating = avg_rating + b_m) %>%
  .$pred_rating

#Find RMSE for Movie Model using edxTest set
rmse_movie_model <- rmse(movie_model_ratings, edxTest$rating)
#Store RMSE for edxTest set in edxTest Results table
results_edxTest <- bind_rows(results_edxTest, 
                              data.frame(
                                Model = "Movie Model",
                                RMSE = rmse_movie_model)
                              )
#Remove duplicates from edxTest Results table in case code is run more than once
results_edxTest <- results_edxTest[!duplicated(results_edxTest[, c("Model","RMSE")]),]

#Predict ratings for Movie Model using Validation set
val_movie_model_ratings <- validation %>% 
  left_join(movie_effects, by = "movieId") %>%
  mutate(pred_rating = avg_rating + b_m) %>%
  .$pred_rating

#Find RMSE for Movie Model using edxTest set
val_rmse_movie_model <- rmse(val_movie_model_ratings, validation$rating)

#Store RMSE for Validation set in Validation Results table
results_validation <- bind_rows(results_validation , 
                              data.frame(
                                Model = "Movie Model",
                                RMSE = val_rmse_movie_model)
                              )
#Remove duplicates from Validation Results table in case code is run more than once
results_validation <- results_validation[!duplicated(results_validation[, c("Model","RMSE")]),]

#Remove stored data or values to free up space
rm(avg_rating, movie_effects, movie_model_ratings, rmse_movie_model, val_movie_model_ratings, val_rmse_movie_model)
  


## ----Movie & User Model--------------------------------------------------------------------------------------------
#This model considers impact of movies and users on ratings
#Find overall average rating
avg_rating <- mean(edxTrain$rating)

#Find the Average Effect (or bias) movies have on Ratings
movie_effects <- edxTrain %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - avg_rating))

#Find the Average Effect (or bias) users have on Ratings
user_effects <- edxTrain %>%
  left_join(movie_effects, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - avg_rating - b_m))

#Predict ratings for Movie & User Model using edxTest set
m_u_model_ratings <- edxTest %>% 
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId") %>%
  mutate(pred_rating = avg_rating + b_m + b_u) %>%
  .$pred_rating

#Find RMSE for Movie & User Model using edxTest set
rmse_m_u_model <- rmse(m_u_model_ratings, edxTest$rating)

#Store RMSE for edxTest set in edxTest Results table
results_edxTest <- bind_rows(results_edxTest, 
                              data.frame(
                                Model = "Movie & User Model",
                                RMSE = rmse_m_u_model)
                              )
#Remove duplicates from edxTest Results table in case code is run more than once
results_edxTest <- results_edxTest[!duplicated(results_edxTest[, c("Model","RMSE")]),]

#Predict ratings for Movie & User Model using Validation set
val_m_u_model_ratings <- validation %>% 
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId") %>%
  mutate(pred_rating = avg_rating + b_m + b_u) %>%
  .$pred_rating

#Find RMSE for Movie & User Model using Validation set
val_rmse_m_u_model <- rmse(val_m_u_model_ratings, validation$rating)

#Store RMSE for Validation set in Validation Results table
results_validation <- bind_rows(results_validation , 
                              data.frame(
                                Model = "Movie & User Model",
                                RMSE = val_rmse_m_u_model)
                              )
#Remove duplicates from Validation Results table in case code is run more than once
results_validation <- results_validation[!duplicated(results_validation[, c("Model","RMSE")]),]

#Remove stored data or values to free up space
rm(avg_rating, movie_effects, user_effects, m_u_model_ratings, rmse_m_u_model, val_m_u_model_ratings, val_rmse_m_u_model)
  


## ----Movie & User & Year Model-------------------------------------------------------------------------------------
#This model considers impact of movies, users & years of release on ratings
#Find overall average rating
avg_rating <- mean(edxTrain$rating)

#Find the Average Effect (or bias) movies have on Ratings
movie_effects <- edxTrain %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - avg_rating))

#Find the Average Effect (or bias) users have on Ratings
user_effects <- edxTrain %>%
  left_join(movie_effects, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - avg_rating - b_m))

#Find the Average Effect (or bias) years of release have on Ratings
year_effects <- edxTrain %>%
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId") %>%
  group_by(release_year) %>%
  summarize(b_y = mean(rating - avg_rating - b_m - b_u))

#Predict ratings for Movie & User & Year Model using edxTest set
m_u_y_model_ratings <- edxTest %>% 
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId") %>%
  left_join(year_effects, by = "release_year") %>%
  mutate(pred_rating = avg_rating + b_m + b_u + b_y) %>%
  .$pred_rating

#Find RMSE for Movie & User & Year Model using edxTest set
rmse_m_u_y_model <- rmse(m_u_y_model_ratings, edxTest$rating)

#Store RMSE for edxTest set in edxTest Results table
results_edxTest <- bind_rows(results_edxTest, 
                              data.frame(
                                Model = "Movie & User & Year Model",
                                RMSE = rmse_m_u_y_model)
                              )
#Remove duplicates from edxTest Results table in case code is run more than once
results_edxTest <- results_edxTest[!duplicated(results_edxTest[, c("Model","RMSE")]),]

#Predict ratings for Movie & User & Year Model using Validation set
val_m_u_y_model_ratings <- validation %>% 
  mutate(release_year = as.numeric(str_sub(validation$title, start = -5, end = -2))) %>%
  mutate(rating_year = year(as_datetime(validation$timestamp))) %>%
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId") %>%
  left_join(year_effects, by = "release_year") %>%
  mutate(pred_rating = avg_rating + b_m + b_u + b_y) %>%
  .$pred_rating

#Find RMSE for Movie & User & Year Model using Validation set
val_rmse_m_u_y_model <- rmse(val_m_u_y_model_ratings, validation$rating)

#Store RMSE for Validation set in Validation Results table
results_validation <- bind_rows(results_validation , 
                              data.frame(
                                Model = "Movie & User & Year Model",
                                RMSE = val_rmse_m_u_y_model)
                              )
#Remove duplicates from Validation Results table in case code is run more than once
results_validation <- results_validation[!duplicated(results_validation[, c("Model","RMSE")]),]

#Remove stored data or values to free up space
rm(avg_rating, movie_effects, user_effects, year_effects, m_u_y_model_ratings, rmse_m_u_y_model, val_m_u_y_model_ratings, val_rmse_m_u_y_model)

  


## ----Movie & User & Year & Timestamp Model-------------------------------------------------------------------------
#This model considers impact of movies, users, years of release & years of ratings on ratings
#Find overall average rating
avg_rating <- mean(edxTrain$rating)

#Find the Average Effect (or bias) movies have on Ratings
movie_effects <- edxTrain %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - avg_rating))

#Find the Average Effect (or bias) users have on Ratings
user_effects <- edxTrain %>%
  left_join(movie_effects, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - avg_rating - b_m))

#Find the Average Effect (or bias) years of release have on Ratings
year_effects <- edxTrain %>%
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId") %>%
  group_by(release_year) %>%
  summarize(b_y = mean(rating - avg_rating - b_m - b_u))

#Find the Average Effect (or bias) years of rating have on Ratings
timestamp_effects <- edxTrain %>%
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId") %>%
  left_join(year_effects, by = "release_year") %>%
  group_by(rating_year) %>%
  summarize(b_t = mean(rating - avg_rating - b_m - b_u - b_y))

#Predict ratings for Movie & User & Year & Timestamp Model using edxTest set
m_u_y_t_model_ratings <- edxTest %>% 
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId") %>%
  left_join(year_effects, by = "release_year") %>%
  left_join(timestamp_effects, by = "rating_year") %>%
  mutate(pred_rating = avg_rating + b_m + b_u + b_y + b_t) %>%
  .$pred_rating

#Find RMSE for Movie & User & Year & Timestamp Model using edxTest set
rmse_m_u_y_t_model <- rmse(m_u_y_t_model_ratings, edxTest$rating)

#Store RMSE for edxTest set in edxTest Results table
results_edxTest <- bind_rows(results_edxTest, 
                              data.frame(
                                Model = "Movie & User & Year & Timestamp Model",
                                RMSE = rmse_m_u_y_t_model)
                              )
#Remove duplicates from edxTest Results table in case code is run more than once
results_edxTest <- results_edxTest[!duplicated(results_edxTest[, c("Model","RMSE")]),]

#Predict ratings for Movie & User & Year & Timestamp Model using Validation set
val_m_u_y_t_model_ratings <- validation %>% 
  mutate(release_year = as.numeric(str_sub(validation$title, start = -5, end = -2))) %>%
  mutate(rating_year = year(as_datetime(validation$timestamp))) %>%
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId") %>%
  left_join(year_effects, by = "release_year") %>%
  left_join(timestamp_effects, by = "rating_year") %>%
  mutate(pred_rating = avg_rating + b_m + b_u + b_y + b_t) %>%
  .$pred_rating

#Find RMSE for Movie & User & Year & Timestamp Model using Validation set
val_rmse_m_u_y_t_model <- rmse(val_m_u_y_t_model_ratings, validation$rating)

#Store RMSE for Validation set in Validation Results table
results_validation <- bind_rows(results_validation , 
                              data.frame(
                                Model = "Movie & User & Year & Timestamp Model",
                                RMSE = val_rmse_m_u_y_t_model)
                              )
#Remove duplicates from Validation Results table in case code is run more than once
results_validation <- results_validation[!duplicated(results_validation[, c("Model","RMSE")]),]

#Remove stored data or values to free up space
rm(avg_rating, movie_effects, user_effects, year_effects, timestamp_effects, m_u_y_t_model_ratings, rmse_m_u_y_t_model, val_m_u_y_t_model_ratings, val_rmse_m_u_y_t_model)
  


## ----Regularized Movie & User & Release Year & Timestamp Model-----------------------------------------------------
#This model considers impact of movies, users, years of release & years of ratings on ratings
#This also introduces regularization to penalize bigger estimates from smaller samples 

#Define lambda values to tune the algorithm
#This sequence of lambdas is computationally expensive
#So, this might not work on all computers
#Pick this sequence as it balances computationally expensiveness & variety of tuning parameter
lambdas <- seq(0, 20, 1)

#Define function to find RMSE for given lambda and given test sets
regularized_rmse <- function(lambda, test_set = edxTest){
  
  #Find overall average rating
  avg_rating <- mean(edxTrain$rating)
  
  #Find the Average Effect (or bias) movies have on Ratings
  #Regularize this effect
  movie_effects <- edxTrain %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating -
                    avg_rating)/(lambda+n()))
  
  #Find the Average Effect (or bias) users have on Ratings
  #Regularize this effect
  user_effects <- edxTrain %>%
    left_join(movie_effects, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - avg_rating - b_m)/(lambda + n()))
  
  #Find the Average Effect (or bias) years of release have on Ratings
  #Regularize this effect
  year_effects <- edxTrain %>%
    left_join(movie_effects, by = "movieId") %>%
    left_join(user_effects, by = "userId") %>%
    group_by(release_year) %>%
    summarize(b_y = sum(rating - avg_rating - b_m - b_u)/(lambda + n()))
  
  #Find the Average Effect (or bias) years of ratings have on Ratings
  #Regularize this effect
  timestamp_effects <- edxTrain %>%
    left_join(movie_effects, by = "movieId") %>%
    left_join(user_effects, by = "userId") %>%
    left_join(year_effects, by = "release_year") %>%
    group_by(rating_year) %>%
    summarize(b_t = sum(rating - avg_rating - b_m - b_u - b_y)/(lambda + n()))
  
  #Predict ratings for this Model using a given test set
  m_u_y_t_model_ratings <- test_set %>% 
    mutate(release_year = as.numeric(str_sub(test_set$title, start = -5, end = -2))) %>%
    mutate(rating_year = year(as_datetime(test_set$timestamp))) %>%
    left_join(movie_effects, by = "movieId") %>%
    left_join(user_effects, by = "userId") %>%
    left_join(year_effects, by = "release_year") %>%
    left_join(timestamp_effects, by = "rating_year") %>%
    mutate(pred_rating = avg_rating + b_m + b_u + b_y + b_t) %>%
    pull(pred_rating)
  
  #Find RMSE for this Model using a given test set
  rmse_m_u_y_t_model <- rmse(m_u_y_t_model_ratings, test_set$rating)
  rmse_m_u_y_t_model
}
#Calculate RMSEs using edxTest for each lambda
rmses <- sapply(lambdas, regularized_rmse, test_set = edxTest)
#Choose the lambda that minimizes RMSE
best_lambda <- lambdas[which.min(rmses)]
#Pick the minimum RMSE
rmse_min <- min(rmses)

#Store RMSE for edxTest set in edxTest Results table
results_edxTest <- bind_rows(results_edxTest, 
                              data.frame(
                                Model = "Regularized Movie & User & Year & Timestamp Model",
                                RMSE = rmse_min)
                              )
#Remove duplicates from edxTest Results table in case code is run more than once
results_edxTest <- results_edxTest[!duplicated(results_edxTest[, c("Model","RMSE")]),]

  


## ----Plot - RMSEs by Lambdas, message=FALSE------------------------------------------------------------------------
#Create RMSEs by Lambdas plot using ggplot2
#Check if there are lambdas and RMSEs
if(exists("lambdas") && exists("rmses")){
  data.frame(lambdas=lambdas, rmses=rmses) %>%
    ggplot(aes(lambdas, rmses)) +
    geom_point(color="#FA8072") +
    labs(title = "Figure 5: RMSEs by Lambdas \n", 
         subtitle="Regularized Movie & User & Year & Timestamp Model",
         x = "\nLambdas",
         y = "RMSEs\n") +
    theme_economist()
}
  


## ----RMSE for Regularized Model using Validation set---------------------------------------------------------------
#Find RMSE for Regularized Model using Validation set & previously tuned lambda
#Check if the lambda exist
if(exists("best_lambda")){
  #Find RMSE for this Final Model using Validation set
  val_rmse_min <- regularized_rmse(best_lambda, validation)
  
  #Store RMSE for Validation set in Validation Results table
  results_validation <- bind_rows(results_validation,
                                  data.frame(
                                    Model = "Regularized Movie & User & Year & Timestamp Model",
                                    RMSE = val_rmse_min)
                                  )
  #Remove duplicates from Validation Results table in case code is run more than once
  results_validation <- results_validation[!duplicated(results_validation[, c("Model","RMSE")]),]
  
}


## ----Table 8: Validation Results Table for All models--------------------------------------------------------------
#Show Validation Results Table for All models
results_validation %>% 
  kable(caption = "RMSEs of All Models Using Validation Set", align="c") %>% 
  column_spec(column = 1: ncol(results_validation), border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 13,
                latex_options = c("HOLD_position","striped"),
                position = "center")


## ----Remove stored data or values----------------------------------------------------------------------------------
##Remove stored data or values if they exist
if(exists("lambdas")) rm(lambdas)
if(exists("rmses")) rm(rmses)
if(exists("best_lambda")) rm(best_lambda)
if(exists("rmse_min")) rm(rmse_min)
if(exists("val_rmse_min")) rm(val_rmse_min)
if(exists("edx_columns")) rm(edx_columns)
if(exists("n_ratings")) rm(n_ratings)
if(exists("n_users")) rm(n_users)
if(exists("n_movies")) rm(n_movies)


