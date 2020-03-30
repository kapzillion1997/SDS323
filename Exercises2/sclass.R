library(mosaic)
library(FNN)
sclass = read.csv('./sclass.csv')
summary(sclass)

rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

# Focus on 2 trim levels: 350 and 65 AMG
sclass350 = subset(sclass, trim == '350')
sclass65AMG = subset(sclass, trim == '65 AMG')
plot(price ~ mileage, data = sclass350)
plot(price ~ mileage, data = sclass65AMG)

library(foreach)
k_grid350 = exp(seq(log(1), log(100), length=100)) %>% round %>% unique
err_grid350 = foreach(k = k_grid350, .combine='c') %do% {
  out = do(250)*{
    n = nrow(sclass350)
    n_train = round(0.8*n)  # round to nearest integer
    n_test = n - n_train
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    sclass350_train = sclass350[train_cases,]
    sclass350_test = sclass350[test_cases,]
    
    X_train = model.matrix(~ mileage - 1, data=sclass350_train)
    X_test = model.matrix(~ mileage - 1, data=sclass350_test)
    Y_train = sclass350_train$price
    Y_test = sclass350_test$price
    
    scale_factors = apply(X_train, 2, sd)
    X_train_sc = scale(X_train, scale=scale_factors)
    X_test_sc = scale(X_test, scale=scale_factors)
    
    knn_model = knn.reg(train=X_train_sc, test=X_test_sc, Y_train, k=k)
    rmse(Y_test, knn_model$pred)
  }
  mean(out$result)
}
plot(k_grid350, err_grid350)
optimal_k350 = k_grid350[match(min(err_grid350), err_grid350)]
knn_model_optimal350 = knn.reg(train=X_train_sc, test=X_test_sc, Y_train, k=optimal_k350)
plot(X_test, knn_model_optimal350$pred, col='blue')
points(X_test, Y_test, col='red')


#sclass 65AMG
k_grid65AMG = exp(seq(log(1), log(100), length=100)) %>% round %>% unique
err_grid65AMG = foreach(k = k_grid65AMG, .combine='c') %do% {
  out = do(250)*{
    n = nrow(sclass65AMG)
    n_train = round(0.8*n)  # round to nearest integer
    n_test = n - n_train
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    sclass65AMG_train = sclass65AMG[train_cases,]
    sclass65AMG_test = sclass65AMG[test_cases,]
    
    X_train = model.matrix(~ mileage - 1, data=sclass65AMG_train)
    X_test = model.matrix(~ mileage - 1, data=sclass65AMG_test)
    Y_train = sclass65AMG_train$price
    Y_test = sclass65AMG_test$price
    
    scale_factors = apply(X_train, 2, sd)
    X_train_sc = scale(X_train, scale=scale_factors)
    X_test_sc = scale(X_test, scale=scale_factors)
    
    knn_model = knn.reg(train=X_train_sc, test=X_test_sc, Y_train, k=k)
    rmse(Y_test, knn_model$pred)
  }
  mean(out$result)
}
plot(k_grid65AMG, err_grid65AMG)
optimal_k65AMG = k_grid65AMG[match(min(err_grid65AMG), err_grid65AMG)]
knn_model_optimal65AMG = knn.reg(train=X_train_sc, test=X_test_sc, Y_train, k=optimal_k65AMG)
plot(X_test, knn_model_optimal65AMG$pred, col='blue')
points(X_test, Y_test, col='red')
