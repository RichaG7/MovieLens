################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggfortify)) install.packages("ggfortify", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(BBmisc)) install.packages("BBmisc", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggfortify)
library(Metrics)
library(scales)
library(BBmisc)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

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
set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1, sample.kind="Rounding")` instead
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

####################Quiz#####################

edx %>% group_by(rating) %>% count()

edx %>% group_by(movieId) %>% count() %>% nrow()

edx %>% group_by(userId) %>% count() %>% nrow()

length(edx$genres[grep("Drama",edx$genres)])

length(edx$genres[grep("Comedy",edx$genres)])

length(edx$genres[grep("Thriller",edx$genres)])

length(edx$genres[grep("Romance",edx$genres)])

test = edx %>% group_by(title) %>% count()

test = edx %>% group_by(rating) %>% count()

remove(test)

#########################################DATA PREPROCESSING#########################################

###Separate combined genres###

edx = edx %>% mutate(Action = ifelse(grepl("Action", genres), 1, 0),
                     Adventure = ifelse(grepl("Adventure", genres), 1, 0),
                     Animation = ifelse(grepl("Animation", genres), 1, 0),
                     Children = ifelse(grepl("Children", genres), 1, 0),
                     Comedy = ifelse(grepl("Comedy", genres), 1, 0),
                     Crime = ifelse(grepl("Crime", genres), 1, 0),
                     Documentary = ifelse(grepl("Documentary", genres), 1, 0),
                     Drama = ifelse(grepl("Drama", genres), 1, 0),
                     Fantasy = ifelse(grepl("Fantasy", genres), 1, 0),
                     Film_noir = ifelse(grepl("Film-Noir", genres), 1, 0),
                     Horror = ifelse(grepl("Horror", genres), 1, 0),
                     Musical = ifelse(grepl("Musical", genres), 1, 0),
                     Mystery = ifelse(grepl("Mystery", genres), 1, 0),
                     Romance = ifelse(grepl("Romance", genres), 1, 0),
                     Sci_Fi = ifelse(grepl("Sci-Fi", genres), 1, 0),
                     Thriller = ifelse(grepl("Thriller", genres), 1, 0),
                     War = ifelse(grepl("War", genres), 1, 0),
                     Western = ifelse(grepl("Western", genres), 1, 0))

#Create function to pull out characters from the right end of a string
substrR <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

edx = edx %>% mutate(year = substr(substrR(substrR(edx$title, 6),5),1,4))

edx$year = as.numeric(edx$year)

edx <- edx %>%
  mutate(rating_datetime = year(as_datetime(timestamp)))

edx$genres = NULL

#########################################INVESTIGATING COUNTS#####################################################

movie_counts = edx %>% group_by(movieId) %>% summarise(Users_Rated=n())
movie_counts[1:1000,] %>% 
  ggplot(aes(x=movieId, y=Users_Rated)) + 
  geom_bar(stat="identity") + 
  labs(title = "Number of Users Rating Each Movie")+
  ylab("Number of Users Rating")

user_counts = edx %>% group_by(userId) %>% summarise(Movies_Rated=n())
user_counts[1:1000,] %>% 
  ggplot(aes(x=userId, y=Movies_Rated)) + 
  geom_bar(stat="identity") + 
  labs(title = "Number of Movies Rated by Each User")+
  ylab("Number of Movies Rated")

#########################################MOVIE EFFECTS#########################################

mu = mean(edx$rating)

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))

predicted_ratings_movie <- validation %>% 
  left_join(movie_avgs,by='movieId') %>% 
  mutate(pred = mu + b_i) %>%
  .$pred

model_movie_rmse <- RMSE(predicted_ratings_movie,validation$rating)
model_movie_rmse #bad

#########################################USER EFFECTS#########################################

user_avgs <- edx %>% 
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu))

predicted_ratings_user = validation %>% 
  left_join(user_avgs,by = 'userId') %>% 
  mutate(pred = mu+b_u) %>% 
  .$pred

model_user_rmse <- RMSE(predicted_ratings_user,validation$rating)
model_user_rmse #bad

#########################################TIME EFFECTS#########################################

rating_datetime_avgs <- edx %>%
  group_by(rating_datetime) %>%
  summarize(b_t = mean(rating - mu))

predicted_ratings_time = validation %>% 
  mutate(rating_datetime = year(as_datetime(timestamp))) %>%
  left_join(rating_datetime_avgs,by = 'rating_datetime') %>% 
  mutate(pred = mu+b_t) %>% 
  .$pred

model_time_rmse <- RMSE(predicted_ratings_time,validation$rating)
model_time_rmse #VERY bad

#########################################MOVIES THAT GO TOGETHER#########################################
genre_matrix = edx[,c(1,2,3,7:24)]

genre_matrix1 = genre_matrix %>% 
  gather('genre', 'present', -c(userId, movieId, rating)) %>%
  filter(present == 1) %>%
  select(-c(rating, present)) %>%
  group_by_at(vars(userId:movieId)) %>%
  summarize_all(paste, collapse="|")

genre_matrix2 = genre_matrix[,c(1,2,3)]

genre_matrix3 = genre_matrix1 %>% 
  left_join(genre_matrix2, by=c('userId', 'movieId'))

cor_t = cor(genre_matrix[,-c(1,2)])

#Plot 1
as.data.frame(cor_t[2:19,1]) %>% ggplot(aes(x=row.names(.), y=`cor_t[2:19, 1]`)) + 
  geom_bar(stat='identity')+
  xlab("Genre") +
  ylab("Correlation Coefficient with Ratings")+
  labs(title="Correlation of Genres and Rating")

#Plot 2
corrplot(cor_t[-1, -1], method = "color", type = "upper")

#For correlation lower than -0.1975513 (mean-sd), we have comedy-drama, horror-drama, and comedy-thriller
#For correlation greater than 0.2929194 (mean + sd), we have animation-children
#We can see that genre is important, so let's run a linear model that takes everything into account

#########################################GENRE BIAS#########################################

edx_genre = edx %>% 
  gather('genre', 'present', -c(userId, movieId, rating, timestamp, title, year, rating_datetime)) %>%
  filter(present == 1)

genre_avgs <- edx_genre %>% 
  group_by(genre) %>%
  summarise(b_g = mean(rating - mu, na.rm=TRUE))

edx_genre = edx_genre %>%
  left_join(genre_avgs, by='genre') %>%
  spread('genre', 'present')

genre_avgs <- edx_genre %>% 
  group_by(movieId) %>%
  summarise(b_g = mean(b_g))

predicted_ratings_genre = validation %>%
  left_join(genre_avgs,by = 'movieId') %>% 
  mutate(pred = mu+b_g) %>% 
  .$pred

model_genre_rmse <- RMSE(predicted_ratings_genre,validation$rating, na.rm=TRUE)
model_genre_rmse #REAL bad

#########################################USER AND MOVIE BIAS#########################################

predicted_ratings_movie_user = validation %>%
  left_join(movie_avgs,by='movieId') %>% 
  left_join(user_avgs,by = 'userId') %>% 
  mutate(pred = mu+b_i+b_u) %>% 
  .$pred

model_movie_user_rmse <- RMSE(predicted_ratings_user,validation$rating)
model_movie_user_rmse #bad

#########################################MOVIE + USER BIAS#########################################

movieuser_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

predicted_ratings_movieuser = validation %>%
  left_join(movie_avgs,by='movieId') %>% 
  left_join(movieuser_avgs,by = 'userId') %>% 
  mutate(pred = mu+b_i+b_u) %>% 
  .$pred

model_movieuser_rmse <- RMSE(predicted_ratings_movieuser,validation$rating)
model_movieuser_rmse #better!

#########################################GENRE + MOVIE + USER + TIME BIAS#########################################

rating_datetime_movieuser_avgs <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(rating_datetime) %>%
  summarize(b_t = mean(rating - mu - b_i))

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(genre_avgs, by='movieId') %>%
  left_join(rating_datetime_avgs, by="rating_datetime") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i - b_g))

predicted_ratings_timegenremovieuser = validation %>%
  mutate(rating_datetime = year(as_datetime(timestamp))) %>% 
  left_join(genre_avgs,by = 'movieId') %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(rating_datetime_avgs, by="rating_datetime") %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu+b_i++b_t+b_u+b_g) %>% 
  .$pred

model_timegenremovieuser_rmse <- RMSE(predicted_ratings_timegenremovieuser,validation$rating, na.rm=TRUE)
model_timegenremovieuser_rmse #Worse than Movie-User, but p good

rm(predicted_ratings_genremovieuser)

RMSE_table = data.frame(Method = c('Movie Bias', 'Movie Bias with filtered mu', 
                                   'User Bias', 'User Bias with filtered mu', 
                                   'User and Movie Bias calculated separately', 'User and Movie Bias calculated separately with filtered mu',
                                   'User and Movie Bias calculated together', 'User and Movie Bias calculated together with filtered mu',
                                   'Movie and User Bias calculated together', 'Movie and User Bias calculated together with filtered mu',
                                   'Genre Bias', 'Genre Bias with filtered mu',
                                   'User, Movie and Genre Bias'),
                        RMSE = c(model_movie_rmse, model_movie_rmse_clean,
                                 model_user_rmse, model_user_rmse_clean,
                                 model_movie_user_rmse, model_movie_user_rmse_clean,
                                 model_movieuser_rmse, model_movieuser_rmse_clean,
                                 model_usermovie_rmse, model_usermovie_rmse_clean,
                                 model_genre_rmse, model_genre_rmse_clean,
                                 model_genremovieuser_rmse))

#########################################REGULARISATION OF MOVIE AND USER RATING#########################################
#########################################REGULARISATION OF MOVIE AND USER RATING#########################################
#########################################REGULARISATION OF MOVIE AND USER RATING#########################################
#########################################REGULARISATION OF MOVIE AND USER RATING#########################################
#########################################REGULARISATION OF MOVIE AND USER RATING#########################################

lambda_reg <- seq(1,10,0.25)

rmses1 <- sapply(lambda_reg,function(l){
  mu <- mean(edx$rating)
  b_i<-edx %>% 
    group_by(movieId) %>% 
    summarise(b_i = sum(rating-mu, na.rm = TRUE)/(n()+l))
  edx_genre = edx %>% 
    gather('genre', 'present', -c(userId, movieId, rating, timestamp, title, year, rating_datetime)) %>%
    filter(present == 1)
  b_g <- edx_genre %>% 
    group_by(genre) %>%
    summarise(b_g = sum(rating - mu - b_i, na.rm=TRUE)/(n()+l))
  edx_genre = edx_genre %>%
    left_join(b_g, by='genre') %>%
    spread('genre', 'present')
  b_g <- edx_genre %>% 
    group_by(movieId) %>%
    summarise(b_g = mean(b_g, na.rm=TRUE))
  b_t = edx %>% 
    left_join(b_g, by='movieId') %>%
    left_join(b_i, by='movieId') %>%
    group_by(rating_datetime) %>% 
    summarize(b_t = sum(rating - mu - b_i - b_g, na.rm=TRUE)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i,by='movieId') %>% 
    left_join(b_g, by='movieId') %>%
    left_join(b_t, by='rating_datetime') %>%
    group_by(userId) %>% 
    summarise(b_u = sum(rating-mu-b_i-b_g-b_t)/(n()+l))
  pred1 <- validation %>% 
    mutate(rating_datetime = year(as_datetime(timestamp))) %>% 
    left_join(b_i,by='movieId') %>% 
    left_join(b_g,by='movieId') %>% 
    left_join(b_t,by='rating_datetime') %>% 
    left_join(b_u,by='userId') %>% 
    mutate(pred = mu+b_i+b_u+b_g+b_t) %>% .$pred
  return(RMSE(pred1,validation$rating))
})

lambda_regularized <- lambda_reg[which.min(rmses1)]

model1_rmse <- rmses1[which.min(rmses1)]
model1_rmse #BEST

#########################################AFTER SCALING RATINGS BY USER#########################################
a = unique(edx$userId)

edx = edx %>% mutate(scaled_score_user = unlist(lapply(a, function(a){
  normalize(edx$rating[which(edx$userId == a)], method = "range", range = c(0, 5))
})))

edx = edx[order(edx$movieId),]
b = unique(edx$movieId)

edx = edx %>% mutate(scaled_score_movie = unlist(lapply(b, function(b){
  normalize(edx$rating[which(edx$movieId == b)], method = "range", range = c(0, 5))
})))

mu_user = mean(edx$scaled_score_user, na.rm = TRUE)
mu_movie = mean(edx$scaled_score_movie, na.rm = TRUE)

rmses_standardized <- sapply(lambda_reg,function(l){
  b_i<-edx %>% 
    group_by(movieId) %>% 
    summarise(b_i = sum(scaled_score_movie - mu_movie, na.rm=TRUE)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i,by='movieId') %>% 
    group_by(userId) %>% 
    summarise(b_u = sum(scaled_score_user - mu_user - b_i, na.rm=TRUE)/(n()+l))
  predicted_ratings_movie_user_regn <- validation %>% 
    left_join(b_i,by='movieId') %>% 
    left_join(b_u,by='userId') %>% 
    mutate(pred = mu+b_i+b_u) %>% .$pred
  return(RMSE(predicted_ratings_movie_user_regn,validation$rating))
})

lambda_standardized <- lambda_reg[which.min(rmses_standardized)]

model_movieuserstandardized_rmse <- rmses_standardized[which.min(rmses_standardized)]
model_movieuserstandardized_rmse #TERRIBLE

RMSE_table2 = data.frame(Method = c('User Bias calculated with Movie Bias', 'User Bias calculated with Movie Bias with filtered mu',
                                   'Movie Bias calculated with User Bias calculated together', 'Movie Bias calculated with User Bias with filtered mu',
                                   'User, Movie and Genre Bias',
                                   'Regularized User+Movie Bias',
                                   'Standardized + Regularized User+Movie Bias'),
                        RMSE = c(model_movieuser_rmse, model_movieuser_rmse_clean,
                                 model_usermovie_rmse, model_usermovie_rmse_clean,
                                 model_genremovieuser_rmse,
                                 model_movieuserregularization_rmse,
                                 model_movieuserstandardized_rmse))

#Scaling fails because we are predicting ratings, and people who consistently rate high or low are likely to continue doing so.
#However, if we were recommending movies (categorical outcome) then scaling might work better than non-scaled data