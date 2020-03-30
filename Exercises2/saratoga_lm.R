library(tidyverse)
library(mosaic)
data(SaratogaHouses)

summary(SaratogaHouses)

rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}


lm_small = lm(price ~ bedrooms + bathrooms + lotSize, data=SaratogaHouses)
lm_medium = lm(price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
                 fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=SaratogaHouses)
lm_medium2 = lm(price ~ . - sewer - waterfront - landValue - newConstruction, data=SaratogaHouses)
coef(lm_medium)
coef(lm_medium2)

lm_big = lm(price ~ (. - sewer - waterfront - landValue - newConstruction)^2, data=SaratogaHouses)
coef(lm_big)


##############WORK SPACE################

plot(price ~ lotSize, data=SaratogaHouses)        #mid        O
plot(price ~ age, data=SaratogaHouses)            #mid-strong O
plot(price ~ landValue, data=SaratogaHouses)      #strong     O
plot(price ~ livingArea, data=SaratogaHouses)     #strong     O
plot(price ~ pctCollege, data=SaratogaHouses)     #mid-strong O
plot(price ~ bedrooms, data=SaratogaHouses)       #weak-mid
plot(price ~ fireplaces, data=SaratogaHouses)     #weak-mid
plot(price ~ bathrooms, data=SaratogaHouses)      #mid        O
plot(price ~ rooms, data=SaratogaHouses)          #mid        O
plot(price ~ heating, data=SaratogaHouses)        #weak-mid
plot(price ~ fuel, data=SaratogaHouses)           #weak
plot(price ~ sewer, data=SaratogaHouses)          #weak
plot(price ~ waterfront, data=SaratogaHouses)     #strong     O
plot(price ~ newConstruction, data=SaratogaHouses)#strong     O
plot(price ~ centralAir, data=SaratogaHouses)     #weak

houseVal_prop = SaratogaHouses[, c(1:10)]
propertyVal_prop = SaratogaHouses[, c(11:16)]
head(houseVal_prop)
head(propertyVal_prop)
houseVal_corMat = cor(houseVal_prop)
houseVal_corMat

lm_howard = lm(price ~ livingArea + bathrooms + waterfront + newConstruction + livingArea*bedrooms + livingArea*bathrooms
               + livingArea*rooms + bedrooms*rooms + bathrooms*rooms, data=SaratogaHouses)
coef(lm_howard)


##############WORK SPACE################

n = nrow(SaratogaHouses)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
saratoga_train = SaratogaHouses[train_cases,]
saratoga_test = SaratogaHouses[test_cases,]

lm1 = lm(price ~ lotSize + bedrooms + bathrooms, data=saratoga_train)
lm2 = lm(price ~ . - sewer - waterfront - landValue - newConstruction, data=saratoga_train)
lm3 = lm(price ~ (. - sewer - waterfront - landValue - newConstruction)^2, data=saratoga_train)

# Predictions out of sample
yhat_test1 = predict(lm1, saratoga_test)
yhat_test2 = predict(lm2, saratoga_test)
yhat_test3 = predict(lm3, saratoga_test)
yhat_test_howard = predict(lm_howard, saratoga_test)

rmse_vals = do(100)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  saratoga_train = SaratogaHouses[train_cases,]
  saratoga_test = SaratogaHouses[test_cases,]
  
  # Fit to the training data
  lm1 = lm(price ~ lotSize + bedrooms + bathrooms, data=saratoga_train)
  lm2 = lm(price ~ . - sewer - waterfront - landValue - newConstruction, data=saratoga_train)
  lm3 = lm(price ~ (. - sewer - waterfront - landValue - newConstruction)^2, data=saratoga_train)
  lm_howard = lm(price ~ livingArea + bathrooms + rooms + waterfront + newConstruction + pctCollege + 
                   centralAir + livingArea*bedrooms + livingArea*bathrooms
                 + livingArea*rooms + bedrooms*rooms + fireplaces*fuel + lotSize*sewer, data=SaratogaHouses)
  
  # Predictions out of sample
  yhat_test1 = predict(lm1, saratoga_test)
  yhat_test2 = predict(lm2, saratoga_test)
  yhat_test3 = predict(lm3, saratoga_test)
  yhat_test_howard = predict(lm_howard, saratoga_test)
  
  c(rmse(saratoga_test$price, yhat_test1),
    rmse(saratoga_test$price, yhat_test2),
    rmse(saratoga_test$price, yhat_test3),
    rmse(saratoga_test$price, yhat_test_howard))
}
rmse_vals
colMeans(rmse_vals)
boxplot(rmse_vals)

k_grid = exp(seq(log(1), log(100), length=100)) %>% round %>% unique
err_grid = foreach(k = k_grid, .combine='c') %do% {
  out = do(100)*{
    n = nrow(SaratogaHouses)
    n_train = round(0.8*n)  # round to nearest integer
    n_test = n - n_train
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    train = SaratogaHouses[train_cases,]
    test = SaratogaHouses[test_cases,]
    
    X_train = model.matrix(price ~ livingArea + bathrooms + rooms + waterfront + newConstruction + pctCollege + 
                             centralAir - 1, data=train)
    X_test = model.matrix(price ~ livingArea + bathrooms + rooms + waterfront + newConstruction + pctCollege + 
                            centralAir - 1, data=test)
    Y_train = train$price
    Y_test = test$price
    
    scale_factors = apply(X_train, 2, sd)
    X_train_sc = scale(X_train, scale=scale_factors)
    X_test_sc = scale(X_test, scale=scale_factors)
    
    knn_model = knn.reg(train=X_train_sc, test=X_test_sc, Y_train, k=k)
    rmse(Y_test, knn_model$pred)
  }
  mean(out$result)
}
plot(k_grid, err_grid)
optimal_k = k_grid[match(min(err_grid), err_grid)]
knn_model_optimal = knn.reg(train=X_train_sc, test=X_test_sc, Y_train, k=optimal_k)
min(err_grid)
