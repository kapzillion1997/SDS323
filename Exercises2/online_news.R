library(tidyverse)
library(mosaic)
online_news = read.csv("./online_news.csv") 

summary(online_news)
head(online_news)

rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

############### FEATURE SELECTION ##############
lm = lm(shares ~ n_tokens_title + n_tokens_content + num_hrefs + num_imgs + num_videos + average_token_length + num_keywords + 
          + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + avg_positive_polarity +
        avg_negative_polarity + abs_title_sentiment_polarity, data=online_news)
coef(lm)

library('randomForest')
rfModel <- randomForest(shares ~ n_tokens_title + n_tokens_content + num_hrefs + num_imgs + num_videos + average_token_length + num_keywords + self_reference_avg_sharess, data=online_news)
importance(rfModel)

cor_matrix = cor(online_news[, c(2:length(online_news))])
cor_matrix

library(MASS)
stepAIC(lm, direction = 'both')



################### LINEAR REGRESSION ###############

n = nrow(online_news)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
viral_train = online_news[train_cases,]
viral_test = online_news[test_cases,]

yhat_test = predict(lm, viral_test)
rmse_vals = do(100)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  viral_train = online_news[train_cases,]
  viral_test = online_news[test_cases,]
  
  # Fit to the training data
  lm = lm(shares ~ num_imgs + num_hrefs + avg_negative_polarity + self_reference_avg_sharess,data=online_news)
  
  # Predictions out of sample
  yhat_test = predict(lm, viral_test)
  
  
  c(rmse(viral_test$shares, yhat_test))
}
rmse_vals
colMeans(rmse_vals)

yhat_test = predict(lm, viral_test)
yhat_test = as.integer(yhat_test)
viral_test$shares = ifelse(viral_test$shares > 1400, 1, 0)
yhat_test = ifelse(yhat_test > 1400, 1, 0)
conf = table(viral_test$shares, yhat_test)
conf
sum(diag(conf))/n_test

######################## KNN REGRESSION #############################


n = nrow(online_news)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
train = online_news[train_cases,]
test = online_news[test_cases,]

X_train = model.matrix(shares ~ n_tokens_title + n_tokens_content + num_hrefs + num_imgs + num_videos + average_token_length + num_keywords + 
                         + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + avg_positive_polarity +
                         avg_negative_polarity + abs_title_sentiment_polarity - 1, data=train)
X_test = model.matrix(shares ~ n_tokens_title + n_tokens_content + num_hrefs + num_imgs + num_videos + average_token_length + num_keywords + 
                        + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + avg_positive_polarity +
                        avg_negative_polarity + abs_title_sentiment_polarity - 1, data=test)
Y_train = train$shares
Y_test = test$shares

scale_factors = apply(X_train, 2, sd)
X_train_sc = scale(X_train, scale=scale_factors)
X_test_sc = scale(X_test, scale=scale_factors)

knn_model = knn.reg(train=X_train_sc, test=X_test_sc, Y_train, k=3)
rmse(Y_test, knn_model$pred)


viral_y_test = ifelse(Y_test > 1400, 1, 0)
knn_model = as.numeric(unlist(knn_model$pred))
viral_test_knn = ifelse(knn_model > 1400, 1, 0)
conf_knn = table(viral_y_test, viral_test_knn)
conf_knn
sum(diag(conf_knn))/n_test
