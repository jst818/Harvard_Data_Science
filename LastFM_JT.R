library(ggplot2)
library(viridis)
library(tidyverse)
library(caret)
library(tm)
library(wordcloud)
library(recommenderlab)
library(reshape2)
library(knitr)

options(timeout = 120)

artists_file <- "artists.dat"

USR_artists <- "user_artists.dat"

#create tibbles for data analysis

artists <- read_tsv(artists_file)

artists <- artists %>%
  rename(artistID = id)

artists <- select(artists, -url, -pictureURL)

user_artists <- read_tsv(USR_artists)

fm_total_set <- left_join(artists, user_artists, by = "artistID")

# Create train and test sets

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = fm_total_set$weight, times = 1, p = 0.1, list = FALSE)
last.fm <- fm_total_set[-test_index,]
temp <- fm_total_set[test_index,]

# Make sure uID and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(last.fm, by = "artistID") %>%
  semi_join(last.fm, by = "userID")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
last.fm <- rbind(last.fm, removed)

rm(fm_total_set, artists, user_artists, test_index, temp, removed)

#Train and Test Sets

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = last.fm$weight, times = 1, p = 0.1, list = FALSE)
last.fm_train <- last.fm[-test_index,]
temp <- last.fm[test_index,]

last.fm_test <- temp %>%
  semi_join(last.fm_train, by = "artistID") %>%
  semi_join(last.fm_train, by = "userID")

removed <- anti_join(temp, last.fm_test)
last.fm_train <- rbind(last.fm_train, removed)

rm(test_index, temp, removed)

last.fm %>% as_tibble()

str(last.fm)

summary(last.fm) 

head(last.fm)

tail(last.fm) 

nrow(last.fm) 

ncol(last.fm)

n_distinct(last.fm$userID)

last.fm %>%
  summarize(n_users = n_distinct(userID),
            n_artists = n_distinct(artistID))

head(last.fm)

#Average Listening Count by User
pop_artists <- last.fm %>%
  group_by(artistID) %>%
  summarize(listening_count = sum(as.numeric(weight), na.rm = TRUE)) %>%
  arrange(desc(listening_count))

#Top 10 Users

top_10_users <- last.fm %>%
  group_by(userID) %>%
  summarize(total_songs_listened = sum(as.numeric(weight), na.rm = TRUE)) %>%
  arrange(desc(total_songs_listened)) %>%
  head(10)

top_10_users$userID <- as.factor(top_10_users$userID)

ggplot(top_10_users, aes(x = reorder(userID, -total_songs_listened), y = total_songs_listened, fill = userID)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +  # Use the viridis color palette
  labs(title = "Top 10 Users with Most Songs Listened",
       x = "User ID",
       y = "Total Songs Listened") +
  theme(axis.text.x = element_blank())

#Lowest 10 Users

lowest_10_users <- last.fm %>%
  group_by(userID) %>%
  summarize(total_songs_listened = sum(as.numeric(weight), na.rm = TRUE)) %>%
  arrange(total_songs_listened) %>%
  head(10)

lowest_10_users$userID <- as.factor(lowest_10_users$userID)

ggplot(lowest_10_users, aes(x = reorder(userID, total_songs_listened), y = total_songs_listened, fill = userID)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +  # Use the viridis color palette
  labs(title = "Lowest 10 Users with Fewest Songs Listened",
       x = "User ID",
       y = "Total Songs Listened") +
  theme(axis.text.x = element_blank())

pop_artists <- last.fm %>%
  group_by(name) %>%
  summarize(listening_count = sum(as.numeric(weight))) %>%
  arrange(desc(listening_count))

top_10 <- head(pop_artists, 10)
top_20 <- head(pop_artists,20)

#Top 10
wordcloud(words = top_10$name, freq = top_10$listening_count, scale=c(3,0.5),
          colors=brewer.pal(8, "Set3"), max.words=10,
          random.order=FALSE, rot.per=0.35, 
          main="Top 10 Most Popular Artists")

#need to make the dataset wide
# Calculate the mean of weight
RMSE <- function(true_weight, predicted_weight){
  sqrt(mean((true_weight - predicted_weight)^2))
}

mu_hat <- mean(last.fm_train$weight)
mu_hat

naive_rmse <- RMSE(last.fm_test$weight, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(last.fm_test))
RMSE(last.fm_test$weight, predictions)

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

rmse_results

mu <- mean(last.fm_train$weight) 
listen_avgs <- last.fm_train %>% 
  group_by(artistID) %>% 
  summarize(b_i = mean(weight - mu))


listen_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("black"))

predicted_ratings <- mu + last.fm_test %>% 
  left_join(listen_avgs, by='artistID') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, last.fm_test$weight)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Listen Effect Model",
                                 RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

last.fm_train %>% 
  group_by(userID) %>% 
  summarize(b_u = mean(weight)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# lm(rating ~ as.factor(movieId) + as.factor(userId))
user_avgs <- last.fm_train %>% 
  left_join(listen_avgs, by='artistID') %>%
  group_by(userID) %>%
  summarize(b_u = mean(weight - mu - b_i))

predicted_ratings <- last.fm_test %>% 
  left_join(listen_avgs, by='artistID') %>%
  left_join(user_avgs, by='userID') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, last.fm_test$weight)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Artist + Listen Effects Model",  
                                 RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

last.fm_test %>% 
  left_join(listen_avgs, by='artistID') %>%
  mutate(residual = weight - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  dplyr::select(name,  residual) %>% slice(1:10) %>% pull(name)

artist_names <- last.fm %>% 
  dplyr::select(artistID, name) %>%
  distinct()

last.fm_train %>% count(artistID) %>% 
  left_join(listen_avgs) %>%
  left_join(artist_names, by="artistID") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

lambda <- 3
mu <- mean(last.fm_train$weight)
listen_reg_avgs <- last.fm_train %>% 
  group_by(artistID) %>% 
  summarize(b_i = sum(weight - mu)/(n()+lambda), n_i = n()) 

tibble(original = listen_avgs$b_i, 
       regularlized = listen_reg_avgs$b_i, 
       n = listen_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

last.fm_train %>%
  count(artistID) %>% 
  left_join(listen_reg_avgs, by="artistID") %>%
  left_join(artist_names, by="artistID") %>%
  arrange(desc(b_i)) %>% 
  dplyr::select(name, b_i, n) %>% 
  slice(1:10) %>% 
  pull(name)

last.fm_train %>%
  dplyr::count(artistID) %>% 
  left_join(listen_reg_avgs, by="artistID") %>%
  left_join(artist_names, by="artistID") %>%
  arrange(b_i) %>% 
  dplyr::select(name, b_i, n) %>% 
  slice(1:10) %>% 
  pull(name)

predicted_ratings <- last.fm_test %>% 
  left_join(listen_reg_avgs, by='artistID') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, last.fm_test$weight)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized Listener Effect Model",  
                                 RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()


lambdas <- seq(0, 10, 0.25)
mu <- mean(last.fm_train$weight)
just_the_sum <- last.fm_train %>% 
  group_by(artistID) %>% 
  summarize(s = sum(weight - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- last.fm_test %>% 
    left_join(just_the_sum, by='artistID') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, last.fm_test$weight))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(last.fm_train$weight)
  b_i <- last.fm_train %>% 
    group_by(artistID) %>%
    summarize(b_i = sum(weight - mu)/(n()+l))
  b_u <- last.fm_train %>% 
    left_join(b_i, by="artistID") %>%
    group_by(userID) %>%
    summarize(b_u = sum(weight - b_i - mu)/(n()+l))
  predicted_ratings <- 
    last.fm_test %>% 
    left_join(b_i, by = "artistID") %>%
    left_join(b_u, by = "userID") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, last.fm_test$weight))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized Listener + User Effect Model",  
                                 RMSE = min(rmses)))
rmse_results %>% knitr::kable()

mu <- mean(final_holdout_test$weight)
l <- 0.15
b_i <- final_holdout_test %>%
  group_by(artistID) %>%
  summarize(b_i = sum(weight - mu)/(n() + l))

b_u <- final_holdout_test %>%
  left_join(b_i, by='artistID') %>% 
  group_by(userID) %>%
  summarize(b_u = sum(weight - b_i - mu)/(n() +l))

predicted <- final_holdout_test %>%
  left_join(b_i, by = "artistID") %>%
  left_join(b_u, by = "userID") %>%
  mutate(pred = mu + b_i +  b_u) %>% .$pred

RMSE(predicted, final_holdout_test$weight)