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
if(!require(BBmisc)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("scales", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggfortify)
library(Metrics)
library(scales)
library(BBmisc)
library(corrplot)

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

#Create empty matrix with rows = nrow(edx) + 1
edx_genre_matrix = matrix(0,nrow(edx)+1,18)
genres = c("Action", "Adventure", "Animation", "Children", 
           "Comedy", "Crime","Documentary", "Drama", "Fantasy",
           "Film-Noir", "Horror", "Musical", "Mystery","Romance",
           "Sci-Fi", "Thriller", "War", "Western")

#Pipe in genre names to first row
edx_genre_matrix[1,] = genres

#Assign genre names to matrix columns
colnames(edx_genre_matrix) = genres

#Pull out $genres to separate
genres1 = as.data.frame(edx$genres, stringsAsFactors=FALSE)
genres2 <- as.data.frame(tstrsplit(genres1[,1], '\\|', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
colnames(genres2) <- c(1:8)

#Put in genre information into empty matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genres3 = which(edx_genre_matrix[1,] == genres2[i,c])
    edx_genre_matrix[i+1,genres3] = 1
  }
}

head(edx_genre_matrix)

#convert into dataframe
edx_2 <- as.data.frame(edx_genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
edx_2[,1:18] = sapply(edx_2[,1:18], as.numeric) #convert from characters to integers

#Create a matrix to search for a movie by genre:
years <- as.data.frame(edx$title, stringsAsFactors=FALSE)

#Create function to pull out characters from the right end of a string
substrR <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Separate out movie years
years = as.data.frame(substr(substrR(substrR(years$`edx$title`, 6),5),1,4))

#Create cleaned edx dataset
edx_clean <- cbind(edx[,c(1,2,3)], substr(edx[,5],1,nchar(edx[,5])-6), years, edx_2)
colnames(edx_clean) <- c("userId", "movieId", "rating", "title", "year", genres)
edx_clean[,c(1,5)] = sapply(edx_clean[,c(1,5)], as.character) #convert from factors to characters
edx_clean[,c(1,5)] = sapply(edx_clean[,c(1,5)], as.numeric) #convert from characters to numeric

##########################################################################################

#CLEAN WORKSPACE
rm(years, edx_2, genres1, genres2, genres3, c, i, genres, substrR)

#########################################INVESTIGATING COUNTS#####################################################

movie_counts = edx_clean %>% group_by(movieId) %>% summarise(Users_Rated=n())
mean(movie_counts$Users_Rated)
sd(movie_counts$Users_Rated)
quantile(movie_counts$Users_Rated, c(.25, .50, .75))
movieId_keep = which(movie_counts$Users_Rated > 30)
#We will remove all movies below the count of 30 when calculating our mean

user_counts = edx_clean %>% group_by(userId) %>% summarise(Movies_Rated=n())
mean(user_counts$Movies_Rated)
sd(user_counts$Movies_Rated)
quantile(user_counts$Movies_Rated, c(.25, .50, .75))
userId_keep = which(user_counts$Movies_Rated > 32)
#We will remove all users with fewer than 32 reviews when calculating our mean

rm(movie_counts, user_counts)
#########################################MOVIE EFFECTS#########################################
edx_clean %>% 
  group_by(movieId) %>% 
  summarise(Users_Rated=n(), Mean_Rating=mean(rating), SD_Rating=sd(rating)) %>% 
  filter(Users_Rated > 30)

edx_filtered = edx_clean %>% filter(movieId %in% movieId_keep) %>% filter(userId %in% userId_keep)

rm(movieId_keep, userId_keep)

mu = mean(edx_clean$rating)

mu_clean = mean(edx_filtered$rating)

rm(edx_filtered)

movie_avgs <- edx_clean %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))

predicted_ratings_movie <- validation %>% 
  left_join(movie_avgs,by='movieId') %>% 
  mutate(pred = mu + b_i) %>%
  .$pred

model_movie_rmse <- RMSE(predicted_ratings_movie,validation$rating)
model_movie_rmse #bad

movie_avgs_clean <- edx_clean %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu_clean))

predicted_ratings_movie_clean <- validation %>% 
  left_join(movie_avgs_clean,by='movieId') %>% 
  mutate(pred = mu_clean + b_i) %>%
  .$pred

model_movie_rmse_clean <- RMSE(predicted_ratings_movie_clean,validation$rating)
model_movie_rmse_clean #bad

rm(predicted_ratings_movie, predicted_ratings_movie_clean)

#########################################USER EFFECTS#########################################
edx_clean %>% group_by(userId) %>% summarise(Movies_Rated=n(), Mean_Rating=mean(rating), SD_Rating=sd(rating))

user_avgs <- edx_clean %>% 
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu))

predicted_ratings_user = validation %>% 
  left_join(user_avgs,by = 'userId') %>% 
  mutate(pred = mu+b_u) %>% 
  .$pred

model_user_rmse <- RMSE(predicted_ratings_user,validation$rating)
model_user_rmse #bad

user_avgs_clean <- edx_clean %>% 
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_clean))

predicted_ratings_user_clean = validation %>% 
  left_join(user_avgs,by = 'userId') %>% 
  mutate(pred = mu_clean+b_u) %>% 
  .$pred

model_user_rmse_clean <- RMSE(predicted_ratings_user_clean,validation$rating)
model_user_rmse_clean #worse

rm(predicted_ratings_user, predicted_ratings_user_clean)
#########################################USER AND MOVIE BIAS#########################################

predicted_ratings_movie_user = validation %>%
  left_join(movie_avgs,by='movieId') %>% 
  left_join(user_avgs,by = 'userId') %>% 
  mutate(pred = mu+b_i+b_u) %>% 
  .$pred

model_movie_user_rmse <- RMSE(predicted_ratings_user,validation$rating)
model_movie_user_rmse #bad

predicted_ratings_movie_user_clean = validation %>%
  left_join(movie_avgs_clean,by='movieId') %>% 
  left_join(user_avgs_clean,by = 'userId') %>% 
  mutate(pred = mu+b_i+b_u) %>% 
  .$pred

model_movie_user_rmse_clean <- RMSE(predicted_ratings_movie_user_clean,validation$rating)
model_movie_user_rmse_clean #better!

rm(predicted_ratings_movie_user, predicted_ratings_movie_user_clean)
#########################################USER + MOVIE BIAS#########################################

movieuser_avgs <- edx_clean %>% 
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

movieuser_avgs_clean <- edx_clean %>% 
  left_join(movie_avgs_clean, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_clean - b_i))

predicted_ratings_movieuser_clean = validation %>%
  left_join(movie_avgs_clean,by='movieId') %>% 
  left_join(movieuser_avgs_clean,by = 'userId') %>% 
  mutate(pred = mu+b_i+b_u) %>% 
  .$pred

model_movieuser_rmse_clean <- RMSE(predicted_ratings_movieuser_clean,validation$rating)
model_movieuser_rmse_clean #slightly worse (in 5th dec place) than previous but still better!

rm(predicted_ratings_movieuser, predicted_ratings_movieuser_clean)
#########################################MOVIE+USER BIAS#########################################

usermovie_avgs <- edx_clean %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu - b_u))

predicted_ratings_usermovie = validation %>%
  left_join(user_avgs,by='userId') %>% 
  left_join(usermovie_avgs,by = 'movieId') %>% 
  mutate(pred = mu+b_i+b_u) %>% 
  .$pred

model_usermovie_rmse <- RMSE(predicted_ratings_usermovie,validation$rating)
model_usermovie_rmse #worse than previous but still good!

usermovie_avgs_clean <- edx_clean %>% 
  left_join(user_avgs_clean, by='userId') %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu_clean - b_u))

predicted_ratings_usermovie_clean = validation %>%
  left_join(user_avgs_clean,by='userId') %>% 
  left_join(usermovie_avgs_clean,by = 'movieId') %>% 
  mutate(pred = mu+b_i+b_u) %>% 
  .$pred

model_usermovie_rmse_clean <- RMSE(predicted_ratings_usermovie_clean,validation$rating)
model_usermovie_rmse_clean #worse

rm(predicted_ratings_usermovie, predicted_ratings_usermovie_clean)

#########################################GENRE BIAS#########################################

edx_clean_genre = edx_clean %>% 
  gather('genre', 'present', -c(userId, movieId, rating, title, year, 
                                scaled_score_movie, rescaled_score_movie, 
                                scaled_score_user, rescaled_score_user)) %>%
  filter(present == 1)

genre_avgs <- edx_clean_genre %>% 
  group_by(genre) %>%
  summarise(b_g = mean(rating - mu, na.rm=TRUE), b_g_clean = mean(rating - mu_clean, na.rm=TRUE))

edx_clean_genre = edx_clean_genre %>%
  left_join(genre_avgs, by='genre') %>%
  spread('genre', 'present')

genre_avgs <- edx_clean_genre %>% 
  group_by(movieId) %>%
  summarise(b_g = mean(b_g), b_g_clean = mean(b_g))

predicted_ratings_genre = validation %>%
  left_join(genre_avgs,by = 'movieId') %>% 
  mutate(pred = mu+b_g, pred_clean = mu_clean+b_g_clean) %>% 
  .$pred

predicted_ratings_genre_clean = validation %>%
  left_join(genre_avgs,by = 'movieId') %>% 
  mutate(pred = mu+b_g, pred_clean = mu_clean+b_g_clean) %>% 
  .$pred_clean

model_genre_rmse <- RMSE(predicted_ratings_genre,validation$rating, na.rm=TRUE)
model_genre_rmse #REAL bad

model_genre_rmse_clean <- RMSE(predicted_ratings_genre_clean,validation$rating, na.rm=TRUE)
model_genre_rmse_clean #EVEN worse

rm(predicted_ratings_genre, predicted_ratings_genre_clean)

#filtered mu is consistently worse, therefore we will stop using it.

rm(movie_avgs_clean, user_avgs_clean, movieuser_avgs_clean, usermovie_avgs_clean)

#########################################GENRE + MOVIE + USER BIAS#########################################

genremovieuser_avgs <- edx_clean %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(genre_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i - b_g))

predicted_ratings_genremovieuser = validation %>%
  left_join(genre_avgs,by = 'movieId') %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(genremovieuser_avgs, by='userId') %>%
  mutate(pred = mu+b_i+b_u+b_g) %>% 
  .$pred

model_genremovieuser_rmse <- RMSE(predicted_ratings_genremovieuser,validation$rating, na.rm=TRUE)
model_genremovieuser_rmse #Worse than Movie-User, but p good

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

rmses_regularized <- sapply(lambda_reg,function(l){
  mu <- mean(edx_clean$rating)
  b_i<-edx_clean %>% 
    group_by(movieId) %>% 
    summarise(b_i = sum(rating-mu)/(n()+l))
  b_u <- edx_clean %>% 
    left_join(b_i,by='movieId') %>% 
    group_by(userId) %>% 
    summarise(b_u = sum(rating-mu-b_i)/(n()+l))
  predicted_ratings_movie_user_regn <- validation %>% 
    left_join(b_i,by='movieId') %>% 
    left_join(b_u,by='userId') %>% 
    mutate(pred = mu +b_i+b_u) %>% .$pred
  return(RMSE(predicted_ratings_movie_user_regn,validation$rating))
})

lambda_regularized <- lambda_reg[which.min(rmses_regularized)]

model_movieuserregularization_rmse <- rmses_regularized[which.min(rmses_regularized)]
model_movieuserregularization_rmse #BEST

#########################################AFTER SCALING RATINGS BY USER#########################################
a = unique(edx_clean$userId)

edx_clean = edx_clean %>% mutate(scaled_score_user = unlist(lapply(a, function(a){
  normalize(edx_clean$rating[which(edx_clean$userId == a)], method = "range", range = c(0, 5))
})))

edx_clean = edx_clean[order(edx_clean$movieId),]
b = unique(edx_clean$movieId)

edx_clean = edx_clean %>% mutate(scaled_score_movie = unlist(lapply(b, function(b){
  normalize(edx_clean$rating[which(edx_clean$movieId == b)], method = "range", range = c(0, 5))
})))

mu_user = mean(edx_clean$scaled_score_user, na.rm = TRUE)
mu_movie = mean(edx_clean$scaled_score_movie, na.rm = TRUE)

rmses_standardized <- sapply(lambda_reg,function(l){
  b_i<-edx_clean %>% 
    group_by(movieId) %>% 
    summarise(b_i = sum(scaled_score_movie - mu_movie, na.rm=TRUE)/(n()+l))
  b_u <- edx_clean %>% 
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

#########################################MOVIES THAT GO TOGETHER#########################################
genre_matrix = edx_clean[,c(2,3,6:23)] %>% filter(movieId %in% movieId_keep)
genre_matrix = genre_matrix[!duplicated(genre_matrix$movieId), ]

cor_t = cor(genre_matrix[,-1])
corrplot(cor(genre_matrix[,-1]), method = "color", type = "upper")

#For correlation lower than -0.1975513 (mean-sd), we have comedy-drama, horror-drama, and comedy-thriller
#For correlation greater than 0.2929194 (mean + sd), we have animation-children
#We can see that genre is important, so let's run a linear model that takes everything into account
#########################################LINEAR REGRESSION!#########################################

base = edx_clean %>% filter(!(userId %in% validation$userId & movieId %in% validation$movieId))

base = na.omit(base)

base = as.factor(base[,c(1,2)])

indices = base %>%
  group_by(userId, movieId) %>%
  group_indices()

set.seed(1)
train_index = createDataPartition(y = as.factor(indices), times = 1, p = 0.1, list = FALSE)
train <- base[train_index,]
test <- base[-train_index,]

train = train[,-c(4)]

#########################################PCA#########################################
##PCA TEST


test_val = validation[,c(1,2)]

test = edx_clean %>% filter(!(movieId %in% test_val$movieId) | !(userId %in% test_val$userId))

test = na.omit(test)

test = test[,-c(24,26)]

set.seed(1)
train_index = createDataPartition(y = test$rating, times = 1, p = 0.5, list = FALSE)
test_train <- test[train_index,]
test_test <- test[-train_index,]

test_train = test_train[,-c(4)]
test_train1 = test_train[,-c(4,24,25)]
test_train2 = test_train[,-c(3,4,24)]
test_train3 = test_train[,-c(3,4,25)]

#test_test = test_test[,-4]

fit <- train(rating ~ ., method = "lm", data = test_train, na.action = na.pass)
pred <- predict(fit, test_test)
RMSE = rmse(test_test$rating, pred)

fit1 <- train(rating ~ ., method = "lm", data = test_train1, na.action = na.pass)
pred1 <- predict(fit1, test_test)
RMSE1 = rmse(test_test$rating, pred1)

fit2 <- train(rescaled_score_movie ~ ., method = "lm", data = test_train2, na.action = na.pass)
pred2 <- predict(fit2, test_test)
RMSE2 = rmse(test_test$rating, pred2)

fit3 <- train(rescaled_score_user ~ ., method = "lm", data = test_train3, na.action = na.pass)
pred3 <- predict(fit3, test_test)
RMSE3 = rmse(test_test$rating, pred3)


pca_t = prcomp(test_train[,-c(4,5)])
corrplot(pca_t$rotation, method = "color")
plot(summary(pca_t)$importance[3,], xlab="Primary Component", ylab="Cumulative Proportion")

pca_t2 = prcomp(edx_clean[,-c(4)])
corrplot(pca_t2$rotation, method = "color")
plot(summary(pca_t2)$importance[3,], xlab="Primary Component", ylab="Cumulative Proportion")

pca_edx_rating = prcomp(edx_clean[,-c(4,24,25)])
corrplot(pca_edx_rating$rotation, method = "color")
plot(summary(pca_edx_rating)$importance[3,], xlab="Primary Component", ylab="Cumulative Proportion")

pca_edx_scaled_rating_user = prcomp(edx_clean[,-c(3,4,25)])
corrplot(pca_edx_scaled_rating_user$rotation, method = "color")
plot(summary(pca_edx_scaled_rating_user)$importance[3,], xlab="Primary Component", ylab="Cumulative Proportion")

pca_edx_scaled_rating_movie = prcomp(edx_clean[,-c(3,4,24)])
corrplot(pca_edx_scaled_rating_movie$rotation, method = "color")
plot(summary(pca_edx_scaled_rating_movie)$importance[3,], xlab="Primary Component", ylab="Cumulative Proportion")

#Create test and train sets
set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = edx_clean$rating, times = 1, p = 0.99995, list = FALSE)
train <- edx_clean[-test_index,]
test <- edx_clean[test_index,]

set.seed(1)
fit <- train(rating ~ ., method = "lm", data = train)

pred <- predict(fit, validation)

cv <- confusionMatrix(factor(pred), validation$rating)$overall["Accuracy"]

RMSE = rmse(validation$rating, pred)