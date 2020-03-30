library(tidyverse)
library(FNN)

# The target variable is shares, i.e. how many times the article was shared on social media
# Mashable is interested in building a model for whether the article goes viral or not.
# They judge this on the basis of a cutoff of 1400 shares (>1400 = viral)

# Get rid of the url column
no_url = online_news[ , -which(names(online_news) %in% c("url"))]

##################################################### Base Model #####################################################
# How do these numbers compare with a reasonable baseline or "null" model (such as the model which always predicts "not viral")?
# do train/test
total_posts = nrow(online_news)

viral = no_url %>% 
  filter(shares >= 1400)
viral_posts = nrow(viral)

not_viral = no_url %>% 
  filter(shares < 1400)
not_viral_posts = nrow(not_viral)

percent_viral = viral_posts / total_posts

percent_not_viral = not_viral_posts / total_posts

# Model would predict viral for every post with about a 53% success rate


####################################################### Regression Approach ###################################################
# First approach this problem from the standpoint of regression. 
# Note that while the predictions of your model are numerical (shares), the ultimate evaluation is in terms of a binary prediction 
# Report the confusion matrix, overall error rate, true positive rate, and false positive rate for your best model.
# Make sure to average these quantities across multiple train/test splits.


# Probit & Logit Model ################################################################################################################
# https://www.youtube.com/watch?v=A9P888Lxde8&t=555s
new_shares = COPY<-data.frame(online_news)
new_shares <- new_shares[-1] # get rid of url
new_shares$shares = ifelse(new_shares$shares > 1400,1,0)
#attach(new_shares)
Y <- cbind(shares)
X <- cbind(n_tokens_title,n_tokens_content,num_hrefs,num_self_hrefs,num_imgs,num_videos,
           average_token_length,num_keywords,data_channel_is_lifestyle,data_channel_is_entertainment,
           data_channel_is_bus,data_channel_is_socmed,data_channel_is_tech,data_channel_is_world,
           self_reference_min_shares,self_reference_max_shares,self_reference_avg_sharess,weekday_is_monday,
           weekday_is_tuesday,weekday_is_wednesday,weekday_is_thursday,weekday_is_friday,weekday_is_saturday,
           weekday_is_sunday,is_weekend,global_rate_positive_words,global_rate_negative_words,avg_positive_polarity,
           min_positive_polarity,max_positive_polarity,avg_negative_polarity,min_negative_polarity,max_negative_polarity,
           title_subjectivity,title_sentiment_polarity,abs_title_sentiment_polarity)

# Logit Model
logit <- glm(Y ~ X, family=binomial (link = "logit"))
summary(logit)
# Ratios
exp(logit$coefficients)

# Probit Model
probit <- glm(Y ~ X, family=binomial (link = "probit"))
summary(probit)

# Avg Marginal Effects
# Logit
LogitScalar <- mean(dlogis(predict(logit, type = "link")))
LogitScalar * coef(logit)
# Probit
ProbitScalar <- mean(dnorm(predict(probit, type = "link")))
ProbitScalar * coef(probit)

# Predictions
# Logit
plogit <- predict(logit, type="response")
summary(plogit)
# Probit
pprobit <- predict(probit, type="response")
summary(pprobit)

# % Correct
table(true = Y, pred = round(fitted(probit)))
table(true = Y, pred = round(fitted(logit)))





################################################### Classifcation Approach ###################################################
# As a second pass, approach this problem from the standpoint of classification.
# That is, define a new variable viral = ifelse(shares > 1400, 1, 0)
# report the confusion matrix, overall error rate, true positive rate, and false positive rate for your best model. 
# Make sure to average these quantities across multiple train/test splits.

# KNN Class ##################################################################################################################
new_shares = COPY<-data.frame(online_news)
#new_shares$shares = ifelse(new_shares$shares > 1400,"Viral","Not_Viral")
new_shares$viral = ifelse(new_shares$shares > 1400,1,0)
new_shares$viral[new_shares$viral == 0] <- "No"
new_shares$viral[new_shares$viral == 1] <- "Yes"
#OR
# new_shares$shares = ifelse(new_shares$shares > 1400,1,0)
#new_shares$shares[new_shares$shares == 0] <- "No"
#new_shares$shares[new_shares$shares == 1] <- "Yes"
new_shares$viral <- factor(new_shares$viral)
new_shares <- new_shares[-1] # get rid of url
new_shares <- new_shares[-37] # get rid of shares
X = dplyr::select(new_shares, -viral) # -viral
y = new_shares$viral # or shares
n = length(y)
# select a training set
n_train = round(0.8*n)
n_test = n - n_train
library(foreach)
library(mosaic)
k_grid = seq(10, 11, by=1)
err_grid = foreach(k = k_grid,  .combine='c') %do% {
  out = do(1)*{
    train_ind = sample.int(n, n_train)
    X_train = X[train_ind,]
    X_test = X[-train_ind,]
    y_train = y[train_ind]
    y_test = y[-train_ind]
    
    # scale the training set features
    scale_factors = apply(X_train, 2, sd)
    X_train_sc = scale(X_train, scale=scale_factors)
    
    # scale the test set features using the same scale factors
    X_test_sc = scale(X_test, scale=scale_factors)
    
    # Fit KNN models (notice the odd values of K)
    knn_try = class::knn(train=X_train_sc, test= X_test_sc, cl=y_train, k=k)
    
    # Calculating classification errors
    sum(knn_try != y_test)/n_test
  } 
  mean(out$result)
}

plot(k_grid, err_grid)

library(caret)
confusionMatrix(table(knn_try,y_test))

# Which approach performs better: regress first and threshold second (regression approach), 
# or threshold first and regress second (classifcation approach)? Why do you think this is?


