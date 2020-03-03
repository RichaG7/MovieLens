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

#########################################OPTIONAL#########################################

#Save cleaned edx dataset for future
write.csv(edx_clean, "edx_clean.csv")
edx_clean <- read.csv("edx_clean.csv", stringsAsFactors=FALSE)

##########################################################################################

#CLEAN WORKSPACE
rm(years, edx_2, genres1, genres2, genres3, c, i)

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
#########################################MOVIE EFFECTS#########################################
edx_clean %>% 
  group_by(movieId) %>% 
  summarise(Users_Rated=n(), Mean_Rating=mean(rating), SD_Rating=sd(rating)) %>% 
  filter(Users_Rated > 30)

test = edx_clean %>% filter(movieId %in% movieId_keep) %>% filter(userId %in% userId_keep) %>% mean(rating)

mu <- mean(test$rating)

movie_avgs <- edx_clean %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings_movie_avg <- mu + validation %>% left_join(movie_avgs,by="movieId") %>% .$b_i
model_movie_rmse <- RMSE(predicted_ratings_movie_avg,validation$rating)
model_movie_rmse

#########################################USER EFFECTS#########################################
edx_clean %>% group_by(userId) %>% summarise(Movies_Rated=n(), Mean_Rating=mean(rating), SD_Rating=sd(rating))

user_avgs <- edx_clean %>% 
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu))

predicted_ratings_user = validation %>% 
  left_join(user_avgs,by = 'userId') %>% 
  mutate(pred = mu+b_u) %>% 
  .$pred

model_user_rmse <- RMSE(predicted_ratings_user,validation$rating)
model_user_rmse

#########################################USER AND MOVIE BIAS#########################################

user_movie_avgs = edx_clean %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs,by = 'userId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

movie_user_avgs = edx_clean %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs,by = 'userId') %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu - b_u))

predicted_ratings_user_movie = validation %>% 
  left_join(movie_user_avgs,by = 'movieId') %>% 
  left_join(user_movie_avgs,by = 'userId') %>% 
  mutate(pred = mu+b_i+b_u) %>% 
  .$pred

model_movie_user_rmse <- RMSE(predicted_ratings_user,validation$rating)
model_movie_user_rmse

#########################################REGULARISATION OF MOVIE RATING#########################################

lambda_reg <- seq(1,10,0.25)

rmses1 <- sapply(lambda_reg,function(l){
  mu <- mean(edx_clean$rating)
  b_i<-edx_clean %>% 
    group_by(movieId) %>% 
    summarise(b_i = sum(rating-mu)/(n()+l))
  predicted_ratings_movie_regn <- validation %>% 
    left_join(b_i,by='movieId') %>% 
    mutate(pred = mu+b_i) %>% .$pred
  return(RMSE(predicted_ratings_movie_regn,validation$rating))
})

lambda1 = lambda_reg[which.min(rmses1)]
movie_regn_avgs = edx_clean %>% 
  group_by(movieId) %>% 
  summarise(b_i = sum(rating-mu)/(n()+lambda1),n_i = n())

predicted_ratings_movie_regn <- validation %>% 
  left_join(movie_regn_avgs,by='movieId') %>% 
  mutate(pred = mu + b_i) %>% .$pred

model_movieregularization_rmse <- RMSE(predicted_ratings_movie_regn,validation$rating)
model_movieregularization_rmse

#########################################REGULARISATION OF USER RATING#########################################

rmses2 <- sapply(lambda_reg,function(l){
  mu <- mean(edx_clean$rating)
  b_u<-edx_clean %>% 
    group_by(userId) %>% 
    summarise(b_u = sum(rating-mu)/(n()+l))
  predicted_ratings_user_regn <- validation %>% 
    left_join(b_u,by='userId') %>% 
    mutate(pred = mu+b_u) %>% .$pred
  return(RMSE(predicted_ratings_user_regn,validation$rating))
})

lambda2 = lambda_reg[which.min(rmses2)]
user_regn_avgs = edx_clean %>% 
  group_by(userId) %>% 
  summarise(b_u = sum(rating-mu)/(n()+lambda2),n_i = n())

predicted_ratings_user_regn <- validation %>% 
  left_join(user_regn_avgs,by='userId') %>% 
  mutate(pred = mu + b_u) %>% .$pred

model_userregularization_rmse <- RMSE(predicted_ratings_user_regn,validation$rating)
model_userregularization_rmse

#########################################REGULARISATION OF MOVIE AND USER RATING#########################################

rmses3 <- sapply(lambda_reg,function(l){
  mu <- mean(edx_clean$rating)
  b_i<-edx_clean %>% 
    group_by(movieId) %>% 
    summarise(b_i = sum(rating-mu)/(n()+l))
  b_u <- edx_clean %>% 
    left_join(b_i,by='movieId') %>% 
    group_by(userId) %>% 
    summarise(b_u = sum(rating-mu)/(n()+l))
  predicted_ratings_movie_user_regn <- validation %>% 
    left_join(b_i,by='movieId') %>% 
    left_join(b_u,by='userId') %>% 
    mutate(pred = mu +b_i+b_u) %>% .$pred
  return(RMSE(predicted_ratings_movie_user_regn,validation$rating))
})

lambda3 <- lambda_reg[which.min(rmses3)]

model_movieuserregularization_rmse <- rmses3[which.min(rmses3)]
model_movieuserregularization_rmse

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

#write.csv(edx_clean, "edx_clean.csv")
mu_user = mean(edx_clean$scaled_score_user)
mu_movie = mean(edx_clean$scaled_score_movie)

rmses_user <- sapply(lambda_reg,function(l){
  mu_user<- mean(edx_clean$scaled_score_user, na.rm=TRUE)
  b_i<-edx_clean %>% 
    group_by(movieId) %>% 
    summarise(b_i = sum(scaled_score_user-mu_user)/(n()+l))
  b_u <- edx_clean %>% 
    left_join(b_i,by='movieId') %>% 
    group_by(userId) %>% 
    summarise(b_u = sum(scaled_score_user-mu_user-b_i)/(n()+l))
  predicted_ratings <- validation %>% 
    left_join(b_i,by='movieId') %>% 
    left_join(b_u,by='userId') %>% 
    mutate(pred = mu_user+b_i+b_u) %>% .$pred
  return(RMSE(predicted_ratings,validation$rating))
})

rmses_movie <- sapply(lambda_reg,function(l){
  mu_movie<- mean(edx_clean$scaled_score_movie, na.rm=TRUE)
  b_i<-edx_clean %>% 
    group_by(movieId) %>% 
    summarise(b_i = sum(scaled_score_movie-mu_movie)/(n()+l))
  b_u <- edx_clean %>% 
    left_join(b_i,by='movieId') %>% 
    group_by(userId) %>% 
    summarise(b_u = sum(scaled_score_movie-mu_movie-b_i)/(n()+l))
  predicted_ratings <- validation %>% 
    left_join(b_i,by='movieId') %>% 
    left_join(b_u,by='userId') %>% 
    mutate(pred = mu_movie+b_i+b_u) %>% .$pred
  return(RMSE(predicted_ratings,validation$rating))
})

lambda3 <- lambda_reg[which.min(rmses_user)]

lambda4 <- lambda_reg[which.min(rmses_movie)]

model_stdnregn_rmse1 <- rmses_user[which.min(rmses_user)]
model_stdnregn_rmse1

model_stdnregn_rmse2 <- rmses_movie[which.min(rmses_movie)]
model_stdnregn_rmse2

#########################################MOVIES THAT GO TOGETHER#########################################
genre_matrix = edx_clean[,c(2,6:23)] %>% filter(movieId %in% movieId_keep)
genre_matrix = genre_matrix[!duplicated(genre_matrix$movieId), ]

cor_t = cor(genre_matrix[,-1])
corrplot(cor(genre_matrix[,-1]), method = "color", type = "upper")

#########################################PCA#########################################
##PCA

pca_t = prcomp(genre_matrix[,-1])
corrplot(pca_t$rotation, method = "color")
plot(summary(pca_t)$importance[3,], xlab="Primary Component", ylab="Cumulative Proportion")

PCA = prcomp(edx_2)
autoplot(PCA)

validation = edx_clean[test_index,]
validation <- validation %>% 
  semi_join(edx_clean, by = "movieId") %>%
  semi_join(edx_clean, by = "userId")

#Create test and train sets
set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = edx_clean$rating, times = 1, p = 0.99995, list = FALSE)
train <- edx_clean[-test_index,]
test <- edx_clean[test_index,]

model <- c("knn")
set.seed(1)
fit <- train(rating ~ ., method = "knn", data = train)

pred <- predict(fit, validation)

cv <- confusionMatrix(factor(pred), validation$rating)$overall["Accuracy"]

RMSE = rmse(validation$rating, pred)