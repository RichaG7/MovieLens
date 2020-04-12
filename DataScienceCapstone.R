################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(BBmisc)) install.packages("BBmisc", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(Metrics)
library(scales)
library(BBmisc)
library(corrplot)
library(lubridate)
library(psych)
library(Hmisc)

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

#########################################INVESTIGATING USER BIAS#####################################################

user_counts = edx %>% group_by(userId) %>% summarise(Movies_Rated=n(), Mean_Rating=mean(rating), Standard_Deviation = sd(rating))
user_counts[1:100,] %>% 
  ggplot(aes(x=userId, y=Movies_Rated)) + 
  geom_bar(stat="identity") + 
  labs(title = "Number of Movies Rated by Each User")+
  ylab("Number of Movies Rated")

user_counts[1:100,] %>% 
  ggplot(aes(x=userId, y=Mean_Rating)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = Mean_Rating-Standard_Deviation, ymax = Mean_Rating+Standard_Deviation)) +
  labs(title = "Mean Rating by User")+
  ylab("Mean Rating")

rm(user_counts, user_ratings)

#########################################INVESTIGATING MOVIE BIAS#########################################

movie_counts = edx %>% group_by(movieId) %>% summarise(Movies_Rated=n(), Mean_Rating=mean(rating), Standard_Deviation = sd(rating))
movie_counts[1:100,] %>% 
  ggplot(aes(x=movieId, y=Movies_Rated)) + 
  geom_bar(stat="identity") + 
  labs(title = "Number of Ratings for Each Movie")+
  ylab("Number of Movies Rated")

movie_counts[1:100,] %>% 
  ggplot(aes(x=movieId, y=Mean_Rating)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = Mean_Rating-Standard_Deviation, ymax = Mean_Rating+Standard_Deviation)) +
  labs(title = "Mean Rating by Movie") +
  ylab("Mean Rating")

rm(movie_counts, movie_ratings)

#########################################INVESTIGATING GENRE BIAS#########################################

genre_matrix = edx[,c(7:24)]
cor_t = rcorr(as.matrix(genre_matrix), type="pearson")
cor_t_r = cor_t$r
corrplot(cor_t_r, method = "color", type = "upper")

rm(cor_t, cor_t_r)

genre_matrix = edx[,c(3,7:24)]
cor_t = rcorr(as.matrix(genre_matrix), type="pearson")

cor_t_r = cor_t$r
cor_t_r = as.data.frame(cor_t_r[-1,1])
cor_t_r$genre <- row.names(cor_t_r)
colnames(cor_t_r) <- c("coef","genre")

cor_t_r %>% 
  ggplot(aes(x=genre, y=coef)) + 
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Coefficients of Correlation between Genre and Rating") +
  ylab("Coefficients")

rm(cor_t, cor_t_r)

genre_matrix = edx[,c(1,2,3,7:24)]

genre_counts = genre_matrix %>% 
  gather('genre', 'present', -c(userId, movieId, rating)) %>%
  filter(present == 1) %>% group_by(genre) %>%
  summarise(Movies_Rated = n(), Mean_Rating=mean(rating), Standard_Deviation=sd(rating))

genre_counts

genre_counts %>% 
  ggplot(aes(x=genre, y=Movies_Rated)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Number of Movies Rated in Each Genre")+
  ylab("Number of Movies Rated")

genre_counts %>% 
  ggplot(aes(x=genre, y=Mean_Rating)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = Mean_Rating-Standard_Deviation, ymax = Mean_Rating+Standard_Deviation)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Mean Rating by Genre")+
  ylab("Mean Rating")

genre_matrix = edx[,c(1,2,3,6:23)]

genre_matrix1 = genre_matrix %>% 
  gather('genre', 'present', -c(userId, movieId, rating)) %>%
  filter(present == 1) %>%
  select(-c(rating, present)) %>%
  group_by_at(vars(userId:movieId)) %>%
  summarize_all(paste, collapse="|")

genre_matrix2 = genre_matrix[,c(1,2,3)]

genre_matrix3 = genre_matrix1 %>% 
  left_join(genre_matrix2, by=c('userId', 'movieId'))

genre_matrix = genre_matrix1 %>% 
  left_join(genre_counts, by='genre')

genre_counts = edx %>% 
  group_by(genres) %>%
  summarise(Mean_Rating = mean(rating), Standard_Deviation = sd(rating))

genre_counts = arrange(genre_counts, desc(Mean_Rating))

genre_counts[1:50,] %>% 
  ggplot(aes(x=genres, y=Mean_Rating)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = Mean_Rating-Standard_Deviation, ymax = Mean_Rating+Standard_Deviation)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Mean Rating for Top 50 Genres")+
  ylab("Mean Rating")

rm(genre_matrix1, genre_matrix2, genre_matrix3)

genre_counts[(nrow(genre_counts)-50):nrow(genre_counts),] %>% 
  ggplot(aes(x=genres, y=Mean_Rating)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = Mean_Rating-Standard_Deviation, ymax = Mean_Rating+Standard_Deviation)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Mean Rating for Bottom 50 Genres")+
  ylab("Mean Rating")

edx = edx %>% 
  left_join(genre_matrix[,1:3], by=c('userId', 'movieId'))

genre_corplot = edx[,c(3,27)]

genre_corplot  = genre_corplot %>% 
  mutate(present = 1) %>%
  group_by_at(vars(-present)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=genre, value=present, fill=0) %>%    # spread
  select(-row_id)  # drop the index

cor_t = rcorr(as.matrix(genre_corplot), type="pearson")

cor_t_p = cor_t$P

heatmap(cor_t_p, Rowv = NA, Colv = NA)

rm(cor_t, cor_t_p, genre_corplot, genre_matrix, genre_counts)

#########################################INVESTIGATING TIME EFFECTS#########################################

#########################################INVESTIGATING YEAR-OF-RELEASE EFFECTS#########################################

year_counts = edx %>% group_by(year) %>% summarise(Movies_Rated=n(), Mean_Rating=mean(rating), Standard_Deviation = sd(rating))
year_counts %>% 
  ggplot(aes(x=year, y=Movies_Rated)) + 
  geom_bar(stat="identity") + 
  labs(title = "Number of Movies Rated from Each Year")+
  ylab("Number of Movies Rated")

year_counts %>% 
  ggplot(aes(x=year, y=Mean_Rating)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = Mean_Rating-Standard_Deviation, ymax = Mean_Rating+Standard_Deviation)) +
  labs(title = "Mean Rating by Year")+
  ylab("Mean Rating")

rm(year_counts)

#########################################INVESTIGATING YEAR-OF-RATING EFFECTS#########################################

rating_datetime_counts = edx %>% group_by(rating_datetime) %>% 
  summarise(Movies_Rated=n(), Mean_Rating=mean(rating), Standard_Deviation=sd(rating))
rating_datetime_counts %>% 
  ggplot(aes(x=rating_datetime, y=Movies_Rated)) + 
  geom_bar(stat="identity") + 
  labs(title = "Number of Movies Rated Each Year")+
  ylab("Number of Movies Rated")

rating_datetime_counts %>% 
  ggplot(aes(x=rating_datetime, y=Mean_Rating)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = Mean_Rating-Standard_Deviation, ymax = Mean_Rating+Standard_Deviation)) +
  labs(title = "Mean Rating by Rating Year")+
  ylab("Mean Rating")

#########################################INVESTIGATING DISTANCE BETWEEN YEAR-OF-RELEASE AND YEAR-OF-RATING EFFECTS#########################################

rating_year_distance = edx %>% mutate(distance_from_release = rating_datetime - year) %>%
  group_by(distance_from_release) %>% 
  summarise(Movies_Rated=n(), Mean_Rating=mean(rating), Standard_Deviation=sd(rating))
rating_year_distance %>% 
  ggplot(aes(x=distance_from_release, y=Mean_Rating)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = Mean_Rating-Standard_Deviation, ymax = Mean_Rating+Standard_Deviation)) +
  labs(title = "Mean Rating by Distance of Year of Rating from Year of Release")+
  ylab("Mean Rating")

rm(rating_datetime_counts, rating_year_distance)

#########################################MOVIE BIAS#########################################

mu = mean(edx$rating)

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))

pred_movie <- validation %>% 
  left_join(movie_avgs,by='movieId') %>% 
  mutate(pred = mu + b_i) %>%
  .$pred

movie_error <- RMSE(pred_movie,validation$rating)

rm(movie_avgs, pred_movie)

#########################################USER BIAS#########################################

user_avgs <- edx %>% 
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu))

pred_user <- validation %>% 
  left_join(user_avgs,by='userId') %>% 
  mutate(pred = mu + b_u) %>%
  .$pred

user_error <- RMSE(pred_user,validation$rating)

rm(user_avgs, pred_user)

#########################################GENRE BIAS#########################################

genre_avgs <- edx %>% 
  group_by(genres) %>% 
  summarise(b_g = mean(rating - mu))

pred_genre <- validation %>% 
  left_join(genre_avgs,by='genres') %>% 
  mutate(pred = mu + b_g) %>%
  .$pred

genre_error <- RMSE(pred_genre,validation$rating, na.rm=TRUE)

rm(genre_avgs, pred_genre)

#########################################YEAR-OF-RELEASE BIAS#########################################

year_avgs <- edx %>% 
  group_by(year) %>% 
  summarise(b_y = mean(rating - mu))

pred_year <- validation %>% 
  mutate(year = as.numeric(substr(substrR(substrR(validation$title, 6),5),1,4))) %>%
  left_join(year_avgs,by='year') %>% 
  mutate(pred = mu + b_y) %>%
  .$pred

year_error <- RMSE(pred_year,validation$rating)

rm(year_avgs, pred_year)

#########################################YEAR-OF-RATING BIAS#########################################

time_avgs <- edx %>% 
  group_by(rating_datetime) %>% 
  summarise(b_t = mean(rating - mu))

pred_time <- validation %>% 
  mutate(rating_datetime = year(as_datetime(timestamp))) %>%
  left_join(time_avgs,by='rating_datetime') %>% 
  mutate(pred = mu + b_t) %>%
  .$pred

time_error <- RMSE(pred_time,validation$rating)

rm(time_avgs, pred_time)

#########################################DIFFERENCE BETWEEN YEAR-OF-RELEASE AND YEAR-OF-RATING BIAS#########################################

edx = edx %>% 
  mutate(distance_from_release = rating_datetime - year)

year_time_avgs <- edx %>% 
  group_by(distance_from_release) %>% 
  summarise(b_y_t = mean(rating - mu))

pred_year_time <- validation %>% 
  mutate(year = as.numeric(substr(substrR(substrR(validation$title, 6),5),1,4))) %>%
  mutate(rating_datetime = year(as_datetime(timestamp))) %>%
  mutate(distance_from_release = rating_datetime - year) %>%
  left_join(year_time_avgs,by='distance_from_release') %>% 
  mutate(pred = mu + b_y_t) %>%
  .$pred

year_time_error <- RMSE(pred_year_time,validation$rating)

rm(year_time_avgs, pred_year_time)

#########################################MOVIE-USER BIAS#########################################

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu, na.rm = TRUE))

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu - b_i, na.rm = TRUE))

pred_um_combo <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = rowSums(as.matrix(mu + b_i + b_u), na.rm=TRUE)) %>%
  .$pred

um_combo_error <- RMSE(pred_um_combo,validation$rating)

rm(movie_avgs, user_avgs, pred_um_combo)

#########################################MOVIE-USER-GENRE BIAS#########################################

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu, na.rm = TRUE))

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu - b_i, na.rm = TRUE))

genre_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>% 
  summarise(b_g = mean(rating - mu - b_i - b_u, na.rm = TRUE))

pred_umg_combo <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = rowSums(as.matrix(mu + b_i + b_u + b_g), na.rm=TRUE)) %>%
  .$pred

umg_combo_error <- RMSE(pred_umg_combo,validation$rating)

rm(movie_avgs, user_avgs, genre_avgs, pred_umg_combo)

#########################################MOVIE-USER-YEAR-OF-RELEASE BIAS#########################################

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu, na.rm = TRUE))

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu - b_i, na.rm = TRUE))

year_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(year) %>% 
  summarise(b_y = mean(rating - mu - b_i - b_u, na.rm = TRUE))

pred_umy_combo <- validation %>% 
  mutate(year = as.numeric(substr(substrR(substrR(validation$title, 6),5),1,4))) %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='year') %>%
  mutate(pred = rowSums(as.matrix(mu + b_i + b_u + b_y), na.rm=TRUE)) %>%
  .$pred

umy_combo_error <- RMSE(pred_umy_combo,validation$rating)

rm(movie_avgs, user_avgs, year_avgs, pred_umy_combo)

#########################################MOVIE-USER-YEAR-OF-RATING BIAS#########################################

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu, na.rm = TRUE))

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu - b_i, na.rm = TRUE))

time_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(rating_datetime) %>% 
  summarise(b_t = mean(rating - mu - b_i - b_u, na.rm = TRUE))

pred_umt_combo <- validation %>% 
  mutate(rating_datetime = year(as_datetime(timestamp))) %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(time_avgs, by='rating_datetime') %>%
  mutate(pred = rowSums(as.matrix(mu + b_i + b_u + b_t), na.rm=TRUE)) %>%
  .$pred

umt_combo_error <- RMSE(pred_umt_combo,validation$rating)

rm(movie_avgs, user_avgs, time_avgs, pred_umt_combo)

#########################################MOVIE-USER-DISTANCE-BETWEEN-YEAR-OF-RATING-AND-RELEASE BIAS#########################################

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu, na.rm = TRUE))

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu - b_i, na.rm = TRUE))

year_time_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(distance_from_release) %>% 
  summarise(b_y_t = mean(rating - mu - b_i - b_u, na.rm = TRUE))

pred_umyt_combo <- validation %>% 
  mutate(year = as.numeric(substr(substrR(substrR(validation$title, 6),5),1,4))) %>%
  mutate(rating_datetime = year(as_datetime(timestamp))) %>%
  mutate(distance_from_release = rating_datetime - year) %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_time_avgs,by='distance_from_release') %>% 
  mutate(pred = rowSums(as.matrix(mu + b_i + b_u + b_y_t), na.rm=TRUE)) %>%
  .$pred

umyt_combo_error <- RMSE(pred_umyt_combo,validation$rating)

rm(movie_avgs, user_avgs, year_time_avgs, pred_umyt_combo)

#########################################STANDARDIZATION#########################################

a = unique(edx$userId)

edx = edx %>% mutate(scaled_rating_user = unlist(lapply(a, function(a){
  normalize(edx$rating[which(edx$userId == a)], method = "range", range = c(0, 5))
})))

edx = edx[order(edx$movieId),]
b = unique(edx$movieId)

edx = edx %>% mutate(scaled_rating_movie = unlist(lapply(b, function(b){
  normalize(edx$rating[which(edx$movieId == b)], method = "range", range = c(0, 5))
})))

edx = edx[order(edx$distance_from_release),]
c = unique(edx$distance_from_release)

edx = edx %>% mutate(scaled_rating_year = unlist(lapply(c, function(c){
  normalize(edx$rating[which(edx$distance_from_release == c)], method = "range", range = c(0, 5))
})))

mu_user = mean(edx$scaled_rating_user, na.rm = TRUE)
mu_movie = mean(edx$scaled_rating_movie, na.rm = TRUE)
mu_year = mean(edx$scaled_rating_year, na.rm = TRUE)

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(scaled_rating_movie - mu_movie, na.rm = TRUE))

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarise(b_u = mean(scaled_rating_user - mu_user - b_i, na.rm = TRUE))

year_time_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(distance_from_release) %>% 
  summarise(b_y_t = mean(scaled_rating_year - mu_year - b_i - b_u, na.rm = TRUE))

pred_umyt_combo <- validation %>% 
  mutate(year = as.numeric(substr(substrR(substrR(validation$title, 6),5),1,4))) %>%
  mutate(rating_datetime = year(as_datetime(timestamp))) %>%
  mutate(distance_from_release = rating_datetime - year) %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_time_avgs,by='distance_from_release') %>% 
  mutate(pred = rowSums(as.matrix(mu + b_i + b_u + b_y_t), na.rm=TRUE)) %>%
  .$pred

s_error <- RMSE(pred_umyt_combo,validation$rating)

rm(movie_avgs, user_avgs, year_time_avgs, pred_umyt_combo, a, b, c)

#########################################REGULARIZATION#########################################

lambda_reg <- seq(0,10,0.25)

pred_umyt_combo_rmses = sapply(lambda_reg,function(l){
  mu_r = mean(edx$rating)
  
  movie_avgs_reg <- edx %>% 
    group_by(movieId) %>% 
    summarise(b_i_r = sum(rating - mu_r, na.rm = TRUE)/(n()+l))
  
  user_avgs_reg <- edx %>% 
    left_join(movie_avgs_reg, by='movieId') %>%
    group_by(userId) %>% 
    summarise(b_u_r = sum(rating - mu_r - b_i_r, na.rm = TRUE)/(n()+l))
  
  year_time_avgs_reg <- edx %>% 
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs_reg, by='userId') %>%
    group_by(distance_from_release) %>% 
    summarise(b_y_t_r = sum(rating - mu_r - b_i_r - b_u_r, na.rm = TRUE)/(n()+l))
  
  pred_umyt_combo_reg <- validation %>% 
    mutate(year = as.numeric(substr(substrR(substrR(validation$title, 6),5),1,4))) %>%
    mutate(rating_datetime = year(as_datetime(timestamp))) %>%
    mutate(distance_from_release = rating_datetime - year) %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs_reg, by='userId') %>%
    left_join(year_time_avgs_reg,by='distance_from_release') %>% 
    mutate(pred = rowSums(as.matrix(mu_r + b_i_r + b_u_r + b_y_t_r), na.rm=TRUE)) %>%
    .$pred
  
  return(RMSE(pred_umyt_combo_reg,validation$rating))
})

qplot(lambda_reg, pred_umyt_combo_rmses)  

r_error = pred_umyt_combo_rmses[which.min(pred_umyt_combo_rmses)]
r_error_lambda = lambda_reg[which.min(pred_umyt_combo_rmses)]

rm(pred_umyt_combo_rmses)

RMSE_table = data.frame(model = c('Movie Bias', 'User Bias', 'Genre Bias', 'Time Bias', 'Year Bias', 'Year of Release vs Time of Rating bias', 'User-Movie Combo', 'User-Movie-Genre Combo', 'User-Movie-Year of Release Combo',  'User-Movie-Time of Rating Combo', 'User-Movie-Distance from Release Combo', 'Standardization', 'Regularization'), RMSE = c(movie_error, user_error, genre_error, time_error, year_error, year_time_error, um_combo_error, umg_combo_error, umy_combo_error, umt_combo_error, umyt_combo_error, s_error, r_error))

RMSE_table = RMSE_table[order(RMSE_table$RMSE),]

RMSE_table