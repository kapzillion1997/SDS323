# Exercises 3

# Libraries
library(tidyverse)
library(mosaic)
library(LICORS)  # for kmeans++
library(foreach)
library(mvtnorm)
library(ggplot2)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust)
library(caret)
library(leaps)
library(MASS)
library(FNN)
library("metricsgraphics")

# Data
greenbuildings = read_csv('greenbuildings.csv')
wine = read_csv('wine.csv')
socialmarketing = read_csv('social_marketing.csv')


#######################################################################################################################################################
# 1) Predictive Model Building: Green Buildings
"
Your goals are:
  
to build the best predictive model possible for price; and
to use this model to quantify the average change in rental income per square foot (whether in absolute or percentage terms) associated with green certification, 
holding other features of the building constant.
You can choose whether to consider LEED and EnergyStar separately or to collapse them into a single green certified category. 
You can use any modeling approaches in your toolkit, and you should also feel free to define new variables based on combinations of existing ones. 
Just make sure to explain what youve done

Write a short report detailing your methods, modeling choice, and conclusions
"
# One solution: shrinking or regularizing the coefficient estimates so that they don't just chase noise in the training data.


# Does not include: CS_PropertyID, cluster, Rent, LEED, Energystar, cluster_rent
# Baseline Model: Linear Model, KNN Regression, Logistic Regression, Linear Probability Model

# Visualize Impacts of Variables on Rent between green and not green
ggplot(greenbuildings) + 
  geom_point(aes(x=leasing_rate, y=Rent, color=green_rating))

# Baseline Model
lm_starter = lm(Rent ~ size + empl_gr + leasing_rate + stories + age + renovated + class_a + class_b + green_rating + net + amenities + cd_total_07 + hd_total07 + Precipitation + Gas_Costs + Electricity_Costs, data=greenbuildings)

# Selection
# Feature Selection: Forward/Backward Selection, Stepwise Selection, AIC/BIC
# stepwise selection
# note that we start with a reasonable guess
lm_step = step(lm_starter, scope=~(.)^2)
# the scope statement says:
# "consider all pairwise interactions for everything in lm_medium (.),
# along with the other variables explicitly named that weren't in medium"
# what variables are included?
# # All interactions
# the ()^2 says "include all pairwise interactions"
# the dot (.) means "all variables not named"
# the minus (-) means "exclude this variable"
getCall(lm_step)
coef(lm_step)

# MASS StepAIC
# Thus the AIC estimate is an inflated version of the in-sample MSE. It is not a true out-of-sample estimate.
# This is an estimate of the out-of-sample deviance of a model.
# Don't treat it as an excuse to be careless. Instead proceed cautiously. Always verify that the stepwise-selected model makes sense and doesn't violate any crucial assumptions.
# It's also a good idea to perform a quick train/test split of your data and compute for your final model.
step_model = stepAIC(lm_starter, direction="both")
summary(step_model)

new_model = lm(Rent ~ size + empl_gr + leasing_rate + stories + 
                 age + renovated + class_a + class_b + green_rating + net + 
                 amenities + cd_total_07 + hd_total07 + Precipitation + Gas_Costs + 
                 Electricity_Costs + hd_total07:Electricity_Costs + size:Electricity_Costs + 
                 stories:hd_total07 + empl_gr:Electricity_Costs + empl_gr:Precipitation + 
                 class_a:hd_total07 + age:Electricity_Costs + age:hd_total07 + 
                 cd_total_07:Electricity_Costs + size:leasing_rate + Gas_Costs:Electricity_Costs + 
                 cd_total_07:Precipitation + empl_gr:Gas_Costs + cd_total_07:hd_total07 + 
                 Precipitation:Gas_Costs + age:class_b + stories:renovated + 
                 size:renovated + age:class_a + class_b:Electricity_Costs + 
                 class_b:hd_total07 + net:cd_total_07 + empl_gr:class_a + 
                 age:Gas_Costs + renovated:hd_total07 + Precipitation:Electricity_Costs + 
                 empl_gr:cd_total_07 + class_a:cd_total_07 + class_b:cd_total_07 + 
                 class_a:Electricity_Costs + leasing_rate:Electricity_Costs + 
                 leasing_rate:Precipitation + hd_total07:Gas_Costs + cd_total_07:Gas_Costs + 
                 hd_total07:Precipitation + age:green_rating + size:Gas_Costs + 
                 size:Precipitation + size:stories + amenities:Gas_Costs + 
                 stories:green_rating + green_rating:Precipitation + size:age + 
                 green_rating:Electricity_Costs + green_rating:hd_total07 + 
                 net:hd_total07 + leasing_rate:class_a + class_a:amenities + 
                 stories:amenities + amenities:Precipitation + net:Precipitation + 
                 stories:age + size:hd_total07 + age:renovated + size:class_a + 
                 size:class_b + stories:class_b + empl_gr:stories + renovated:class_a + 
                 renovated:cd_total_07 + renovated:Electricity_Costs + age:Precipitation, 
               data = greenbuildings)

# Performance
# Compare out of sample performance
rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}
n = nrow(greenbuildings)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train
rmse_vals = do(100)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  green_train = greenbuildings[train_cases,]
  green_test = greenbuildings[test_cases,]
  
  # Fit to the training data
  # use `update` to refit the same model with a different set of data
  lm1 = update(new_model, data=green_train)
  
  # Predictions out of sample
  yhat_test1 = predict(lm1, green_test)
  
  c(rmse(green_test$price, yhat_test1))
}
rmse_vals
colMeans(rmse_vals)

# KNN Reg
n = nrow(greenbuildings)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train
# re-split into train and test cases with the same sample sizes
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
green_train = greenbuildings[train_cases,]
green_test = greenbuildings[test_cases,]
# construct the training and test-set feature matrices
# note the "-1": this says "don't add a column of ones for the intercept"
Xtrain = model.matrix(~ . - (CS_PropertyID + cluster + Rent + LEED + Energystar + cluster_rent) - 1, data=green_train)
Xtest = model.matrix(~ . - (CS_PropertyID + cluster + Rent + LEED + Energystar + cluster_rent) - 1, data=green_test)
# training and testing set responses
ytrain = green_train$Rent
ytest = green_test$Rent
# now rescale:
scale_train = apply(Xtrain, 2, sd) # calculate std dev for each column
Xtilde_train = scale(Xtrain, scale = scale_train)
Xtilde_test = scale(Xtest, scale = scale_train) # use the training set scales!
head(Xtrain, 2)
head(Xtilde_train, 2) %>% round(3)
# fit the model
knn_model = knn.reg(Xtilde_train, Xtilde_test, ytrain, k=10)
# calculate test-set performance
rmse(ytest, knn_model$pred)
rmse(ytest, yhat_test2) # from the linear model with the same features
k_grid = exp(seq(log(1), log(300), length=100)) %>% round %>% unique
# Try Many K Values
rmse_grid = foreach(K = k_grid, .combine='c') %do% {
  knn_model = knn.reg(Xtilde_train, Xtilde_test, ytrain, k=K)
  rmse(ytest, knn_model$pred)
}
plot(k_grid, rmse_grid, log='x')
abline(h=rmse(ytest, yhat_test2)) # Linear Model Benchmark


# Compare Green vs Not Green
# Green
green = subset(greenbuildings, green_rating == 1)
summary(green)
# Not Green
not_green = subset(greenbuildings, green_rating == 0)
summary(not_green)

#######################################################################################################################################################
# 2) What Causes What? Planet Money Podcast
# Randomized trial (treatment and control)
# Correlation vs Causation (correlation = chance/trend. causation = direct influence)
# Important because it costs billions of taxpayer dollars
# Isolating a causal relationship between increases in police and reductions in crime has large policy consequences.
# On March 11, 2002, the Office of Homeland Security introduced the Homeland Security Advisory System (HSAS) to inform the public and other government agencies about the risk of terrorist attacks.
# During high-alert times the police increase their presence on the streets of Washington, D.C.
# Annual data are subject to an inherent tradeoff - a longer time-series improves the precision of estimates but increases the possibility of omitted variable bias. 
# Also, our focus on a single city reduces omitted variable bias in the cross sectional component. 
# Our data cover the time period since the alert system began, March 12, 2002 to July 30, 2003. 
# During these 506 days there were 55,882 crimes, or an average of 110 per day.
# The HSAS alert system is broken into 5 color-coded threat conditions: Low (Green), Guarded (Blue), Elevated (Yellow), High (Orange) and Severe (Red).
#  Since its inception, the HSAS has never fallen below Elevated, but, on four occasions during our
# time period, it has risen to High, the second highest level. The alert rose to high on the
# following dates: September 10-14, 2002; February 10-27, 2003; March 17-April 16, 2003; and May 20-30, 2003. 
# Not to advise public, but rather federal agencies and anti-terror efforts
# During a high-alert period, the Washington police department increases the number of patrols, increases the length of shifts in order to put more police on the street
# In addition, to increasing its physical presence, the police department increases its virtual street presence by activating a closed circuit camera system that covers sensitive areas of the National Mall
# The camera system is not permanent; it is only activated during heightened terror alert periods or during major events such as Presidential inaugurations
# The results from our most basic regression are presented in Table 2 where we regress daily D.C. crime totals against the terror alert level (1=High, 0=Elevated) and a day of the week indicator.
# The coefficient on the alert level is statistically significant at the 5 percent level and indicates that on alert days total crimes drop by an average of 7 crimes per day or approximately 6.6 percent. Also potentially of interest, we find that crime is much higher on Fridays (more specifically Friday nights) than on other days.
# We hypothesize that crime falls on high-alert days in Washington D.C. because of greater police presence on the streets
# An alternative hypothesis is that tourism is reduced on high-alert days and, as a result, there are fewer potential victims, leading to fewer crimes
# We are skeptical of the latter explanation on theoretical grounds because holding all else equal, daily crime is unlikely to vary significantly based on the number of daily visitors
# Since there are far more visitors than crimes it seems unlikely that the number of visitors constrains the number of crimes.
# Use other resources during high alert periods
# In Table 5 we examine crime specific regressions. 
# For completeness we examine each of the crime categories but we caution that the daily number of Arsons, Homicides, and Sexual Abuse cases are low relative to the other categories.
# We find statistically significant coefficients for the High Alert*District 1 interactions, and the coefficient is negative for all offense categories except for thefts, robberies (theft accompanied by a threat of force) and homicides. 
# Homicide is likely one of the crimes that is least deterrable by putting police on the streets (it may be deterrable on other margins) so the lack of a negative coefficient is not surprising - because of the low incident rate it would be unwise to draw strong conclusions from the positive coefficient
# The positive coefficients on robberies and thefts are more surprising although the percentage changes they represent, 6 percent for robberies and 1 percent for thefts, suggest that the effect is not very large. 
# For the remaining categories, we estimate that Assaults with a Deadly
# Weapon (ADW) drop by 9 percent in District 1 on high alert days, Burglaries drop by 15 percent, Automobile Thefts decline by 15 percent, and Thefts from Automobiles drop by 40 percent.
# Temporary increases in street police and closed circuit cameras are unlikely to deter crimes such as homicide that often occur in homes among people who know one another but are much more likely to deter street crimes such as automobile theft


# Starts at 8:45
# a) Why can't I just get data from a few different cities and run the regression of Crime on Police to 
# understand how more cops in the streets affect crime? (Crime refers to some measure of crime rate and Police
# measures the number of cops in a city.) 
# Cops put out for terrorism in washington DC not for crime, orange alert days street crime goes down (causal)
# Robbers hiding in room because of terror level
# Hypothesis Tourists less likely so fewer victims (used metro to see incomers and that did not change)
# Cities differ (population size)
# Confounding Variables 
# Regression shows correlation not causation
# All cities differ (not a representative sample)
# Washington is split into 7 police districts. Each distinct might have its own peculiar crime pattern because of differences in geography, population density, income and so forth.
# Table 3, for example, indicates that some districts have twice as many crimes per day as other districts.


# b) How were the researchers from UPenn able to isolate this effect? 
# Briefly describe their approach and discuss their result in the Table 2 below, from the researchers paper.
# By comparing crime to high alert days (crime decreases by an average of about 7.3 crimes for high alert days)
# Low R2 of .14
# Metro Ridership changes by the crimes by about 6.05 crimes for high alert days (not a significant change)
# Low R2 of .17
# Ridership changes by about 17% on high alert days?

# c) Why did they have to control for Metro ridership? What was that trying to capture?
# Because it did not change between the two groups. If there was a difference between these resluts, it would have an impact on the findings.
# To test whether fewer visitors could explain our results we obtained daily data on public transportation (Metro) ridership
# In column ii of Table 2 we verify that highalert levels are not being confounded with tourism levels by including logged mid-day Metro ridership directly in the regression
# The coefficient on the Alert level is slightly smaller at -6.2 crimes per day. Interestingly, we find that increased Metro ridership is correlated with an increase in crime
# However, as the lion-gazelle model (see footnote 11) predicts, the increase is very small - a 10% increase in Metro ridership increases the number of crimes by only 1.7 per day on average. 
# Thus, given that mid-day Metro ridership is a good proxy for tourism, changes in the number of tourists cannot explain the systematic change in crime that we estimate.
# We offer another test of the tourism thesis below when we examine what happens to burglaries (a non-tourist based crime) during high-alert periods
# To control for these differences in our regressions we include district fixed effects. 
# More important, we make use of the fact that the White House, Congress, the Smithsonian and many other prominent government agencies and public areas of Washington are located in District 1, the "National Mall" area.

# d) Below I am showing you "Table 4" from the researchers paper. Just focus on the first column of the table. 
# Can you describe the model being estimated here? What is the conclusion?
# If police presence were decreased in other districts we would expect to see higher levels of crime in other districts during high-alert periods. The analysis below, however, suggests no such effect
# The regression with district fixed effects is in Table 4. 
# During high-alert levels crime in the National Mall area falls by 2.62 crimes per day
# Crime also falls in the other districts, by 0.571 crimes per day but this effect is not statistically significant.
# Recall that on an average day there are 17.1 crimes on the National Mall, implying a decline during high alert days of approximately 15%, more than twice as large as found for the city as a whole. 
# Stated differently, almost one half (43.6%) of the total crime drop during highalert periods is concentrated in district 1, the National Mall area.
# It is revealing to take this argument one step further and assume that all of the increased protection falls on district 1
# Difference between the High Alert*District One and the High Alert*Other Districts coefficients is a difference-in-difference estimator that controls for all common factors between the districts.
# If bad weather, for example, causes decreases in crime, a coincidental correlation with high-alert timing could confound our results. 
# The difference-indifference estimator controls for any factors such as weather, tourism, or other events that affect the districts similarly.
# Even after controlling for all such factors and recognizing that our assumption is too strong we still find that cime falls in district 1 during high-alert periods by some 2 crimes per day or more than 12 percent
# First Column: robust model controlling for ridership and differences between districts 



#######################################################################################################################################################
# 3) Clustering & PCA
"
The data in wine.csv contains information on 11 chemical properties of 6500 different bottles of vinho verde wine from northern Portugal. 
In addition, two other variables about each wine are recorded: whether the wine is red or white
the quality of the wine, as judged on a 1-10 scale by a panel of certified wine snobs.
Run both PCA and a clustering algorithm of your choice on the 11 chemical properties 
(or suitable transformations thereof) and summarize your results. Which dimensionality reduction technique makes more sense to you for this data? 
Convince yourself (and me) that your chosen method is easily capable of distinguishing the reds from the whites, using only the unsupervised information 
contained in the data on chemical properties. Does this technique also seem capable of sorting the higher from the lower quality wines?
"
# Data
head(wine)
summary(wine)

# White Wine Average Rating
white = subset(wine, color == 'white')
summary(white)
mean(white$quality)
# Red Wine Average Rating
red = subset(wine, color == 'red')
summary(red)
mean(red$quality)

# K-Means Clustering (by rating and color)
# Plotting variables by color to see relationships
ggplot(wine) + geom_point(aes(x=sulphates, y=chlorides, color=color))
# fviz_cluster(k2, data = df)
# Sulphates & Chlorides & Total Sulfur Dioxide & Fixed/Volatile Acidity

# As the initial centroids are defined randomly,
# we define a seed for purposes of reprodutability
set.seed(123)
# Let's remove the column with the mammals' names, so it won't be used in the clustering
input <- wine[c(1:11)]
# Extract the centers and scales from the rescaled data (which are named attributes)
X = scale(input, center=TRUE, scale=TRUE)
# Center and scale the data
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")
# Run k-means with 2 clusters and 25 starts
clust1 = kmeans(X, centers=2, nstart=25)
#cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
#centers: A matrix of cluster centers.
#totss: The total sum of squares.
#withinss: Vector of within-cluster sum of squares, one component per cluster.
#tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
#betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
#size: The number of points in each cluster.
fviz_cluster(clust1, data = input)
# What are the clusters?
clust1$center[1,]*sigma + mu
clust1$center[2,]*sigma + mu
# Which wines are in which clusters?
which(clust1$cluster == 1)
which(clust1$cluster == 2)
filter(wine, rownames(wine) == 1018)
# A few plots with cluster membership shown
# qplot is in the ggplot2 library
#qplot(Weight, Length, data=input, color=factor(clust1$cluster))
#qplot(Horsepower, CityMPG, data=input, color=factor(clust1$cluster))
# Using kmeans++ initialization
clust2 = kmeanspp(X, k=2, nstart=25)
clust2$center[1,]*sigma + mu
clust2$center[2,]*sigma + mu
# Which cars are in which clusters?
which(clust2$cluster == 1)
which(clust2$cluster == 2)
# Compare versus within-cluster average distances from the first run
clust1$withinss
clust2$withinss
sum(clust1$withinss)
sum(clust2$withinss)
clust1$tot.withinss
clust2$tot.withinss
clust1$betweenss
clust2$betweenss
# Determining Optimal Clusters
# Elbow Method
set.seed(123)
fviz_nbclust(input, kmeans, method = "wss")
# Average Silhouette
fviz_nbclust(input, kmeans, method = "silhouette")
# Gap Statistic Method
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(input, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
# Visualize
fviz_gap_stat(gap_stat)

# Elbow
k_grid = seq(2, 10, by=1)
SSE_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(X, k, nstart=25)
  cluster_k$tot.withinss
}
plot(k_grid, SSE_grid)
# CH INdex
N = nrow(X)
CH_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(X, k, nstart=25)
  W = cluster_k$tot.withinss
  B = cluster_k$betweenss
  CH = (B/W)*((N-k)/(k-1))
  CH
}
plot(k_grid, CH_grid)
# Gap
library(cluster)
wine_gap = clusGap(X, FUN = kmeans, nstart = 25, K.max = 10, B = 10)
plot(wine_gap)
wine_gap

# PCA (NCI60)
wine.pca <- prcomp(wine[c(1:11)], center = TRUE, scale = TRUE) # Excludes Quality and Color
summary(wine.pca)
# Standard deviation: This is simply the eigenvalues in our case since the data has been centered and scaled (standardized)
# Proportion of Variance: This is the amount of variance the component accounts for in the data, ie. PC1 accounts for >44% of total variance in the data alone!
# Cumulative Proportion: This is simply the accumulated amount of explained variance, ie. if we used the first 10 components we would be able to account for >95% of total variance in the data.
# Since an eigenvalues <1 would mean that the component actually explains less than a single explanatory variable we would like to discard those. (ex: keep first 3, EV of 4 is 0.98)

screeplot(wine.pca, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(wine.pca$sdev^2 / sum(wine.pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 3, col="blue", lty=5)
abline(h = 0.6436, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC3"),
       col=c("blue"), lty=5, cex=0.6)

# We notice is that the first 3 components has an Eigenvalue >1 and explains almost 65% of variance
# We can effectively reduce dimensionality from 11 to 3 while “losing” about 35% of variance

# Let's plot these 3 components
# 1 & 2
plot(wine.pca$x[,1],wine.pca$x[,2], xlab="PC1 (27.54%)", ylab = "PC2 (22.67%)", main = "PC1 / PC2 - plot")
# 2 & 3
plot(wine.pca$x[,2],wine.pca$x[,3], xlab="PC2 (22.67%)", ylab = "PC3 (14.15%)", main = "PC2 / PC3 - plot")
# 1 & 3
plot(wine.pca$x[,1],wine.pca$x[,3], xlab="PC1 (27.54%)", ylab = "PC3 (14.15%)", main = "PC1 / PC3 - plot")

# We want to explain difference between red and white wine
# Let’s  add the response variable (color) to the plot and see if we can make better sense of it
library(factoextra)
fviz_pca_ind(wine.pca, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = wine$color, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Color") +
  ggtitle("2D PCA-plot from 11 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))
# With just the first two components we can clearly see some separation between the white and red wines 


#######################################################################################################################################################
# 4) Market Segmentation: NutrientH20 & Twitter

# They collected every Twitter post ("tweet") by each of their followers over a seven-day period in June 2014. 
# Each tweet was categorized based on its content using a pre-specified scheme of 36 different categories, each representing a broad area of interest (e.g. politics, sports, family, etc.)
# Annotators were allowed to classify a post as belonging to more than one category.
# Each Row is a user
# Two interests of note here are "spam" (i.e. unsolicited advertising) and "adult" (posts that are pornographic or otherwise explicit)
# There's also an "uncategorized" label. (avoided as much as possible, same with chatter) 
# Most bots filtered out but some remain
# there is some inevitable error and noisiness in the annotation process.

# Your task to is analyze this data as you see fit, and to prepare a (short!) report for NutrientH20 that identifies any interesting market segments that appear to stand out in their social-media audience. 
# You have complete freedom in deciding how to pre-process the data and how to define "market segment." (Is it a group of correlated interests? A cluster? A principal component? Etc.)

# How many times did each user tweet? Proportion?
# Add Total Tweets
tweets<-data.frame(social_marketing)
summed <- rowSums(tweets[, 2:37])
tweets$total_tweets = summed
# Turn Into Proportions
props<-data.frame(social_marketing)
props = props[-1]

# K-Means
# As the initial centroids are defined randomly,
# we define a seed for purposes of reprodutability
set.seed(123)
input <- social_marketing[c(2:37)]
# Extract the centers and scales from the rescaled data (which are named attributes)
X = scale(input, center=TRUE, scale=TRUE)
# Center and scale the data
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")
# Run k-means with 2 clusters and 25 starts
clust1 = kmeans(X, centers=36, nstart=25)
fviz_cluster(clust1, data = input)
# What are the clusters?
clust1$center[1,]*sigma + mu
clust1$center[2,]*sigma + mu
# Which wines are in which clusters?
which(clust1$cluster == 1)
which(clust1$cluster == 2)
# A few plots with cluster membership shown
# qplot is in the ggplot2 library
#qplot(Weight, Length, data=input, color=factor(clust1$cluster))
#qplot(Horsepower, CityMPG, data=input, color=factor(clust1$cluster))
# Using kmeans++ initialization
clust2 = kmeanspp(X, k=2, nstart=25)
clust2$center[1,]*sigma + mu
clust2$center[2,]*sigma + mu
# Which cars are in which clusters?
which(clust2$cluster == 1)
which(clust2$cluster == 2)
# Compare versus within-cluster average distances from the first run
clust1$withinss
clust2$withinss
sum(clust1$withinss)
sum(clust2$withinss)
clust1$tot.withinss
clust2$tot.withinss
clust1$betweenss
clust2$betweenss
# Determining Optimal Clusters
# Elbow Method
set.seed(123)
fviz_nbclust(input, kmeans, k.max=50, method = "wss")
# Average Silhouette
fviz_nbclust(input, kmean, k.max=50, method = "silhouette")
# Gap Statistic Method
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(input, FUN = kmeans, nstart = 25,
                    K.max = 50, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
# Visualize
fviz_gap_stat(gap_stat)

# Choosing K
# Elbow
library(foreach)
social_marketing = scale(social_marketing[,2:37]) # cluster on measurables
k_grid = seq(2, 50, by=1)
SSE_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(social_marketing, k, nstart=25)
  cluster_k$tot.withinss
}
plot(k_grid, SSE_grid)
# CH INdex
N = nrow(social_marketing)
CH_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(social_marketing, k, nstart=25)
  W = cluster_k$tot.withinss
  B = cluster_k$betweenss
  CH = (B/W)*((N-k)/(k-1))
  CH
}
plot(k_grid, CH_grid)
# Gap
library(cluster)
social_gap = clusGap(social_marketing, FUN = kmeans, nstart = 25, K.max = 50, B = 100)
plot(social_gap)
social_gap


# Customer Segmentation Using PCA & KMEANS
# PCA
# In short, PCA allows you to take a dataset with a high number of dimensions and compresses it to a dataset with fewer dimensions, which still captures most variance within the original data.
# Loading describes the relationship between the original variables and the new principal component. Specifically, it describes the weight given to an original variable when calculating a new principal component.
# Score describes the relationship between the original data and the newly generated axis. In other words, score is the new value for a data row in the principal component space.
# Proportion of Variance indicates the share of the total data variability each principal component accounts for. It is often used with Cumulative Proportion to evaluate the usefulness of a principal component.
# Cumulative Proportion represents the cumulative proportion of variance explained by consecutive principal components. The cumulative proportion explained by all principal components equals 1 (100% of data variability are explained).
social_marketing = social_marketing[-1]
# PCA
pr_out <- prcomp(social_marketing, center = TRUE, scale = TRUE) #Scaling data before PCA is usually advisable! 
summary(pr_out)
# Screeplot
pr_var <-  pr_out$sdev ^ 2
pve <- pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
# Cumulative PVE plot
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim =c(0,1), type = 'b')
# Another rule of choosing the number of PCs is to choose PCs with eigenvalues higher than 1.
# This is called the Kaiser rule, and it is controversial.
#  In our case, since we are using PCA to determine meaningful and actionable market segmentation, one criterion we should definitely consider is whether the PCs we decide on make sense in the real-world and business settings.
# According to Kaiser ruke, we should use teh first 11 PCs
# Remember that loadings describe the weights given to each raw variable in calculating the new principal component 
# When directly working with the PCA loadings can be tricky and confusing, we can rotate these loadings to make interpretation easier.
# There are multiple rotation methods out there, and we will use a method called "varimax". 
# Rotate loadings
rot_loading <- varimax(pr_out$rotation[, 1:11])
rot_loading
# The numbers in the table correspond to the relationships between our (raw variables) and the selected components.
# If the number is positive, the variable positively contributes to the component. 
# If it's negative, then they are negatively related. 
# Larger the number, stronger the relationship.


# 1) Estimating the optimal number of clusters
features <- social_marketing[,2:37]
scaled_features <- scale(features)

wssplot <- function(data, nc=50, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(scaled_features)

# On entire dataset
set.seed(123) # fix the random starting clusters
kclust10 <- kmeans(features, 10, nstart = 25)

# PCA on Clusters
pca <- prcomp(t(features), scale. = T, center = T)
fviz_eig(pca) + 
  theme_bw() + scale_y_continuous(labels = scales::comma) +
  ggtitle(label='Principal Component Analysis')

cluster.pc10 <- prcomp(features, center = FALSE, scale. = FALSE)$x %>% as.data.frame()
cluster.pc10$kmeans.cluster <- factor(kclust10$cluster)
p<-ggplot(cluster.pc10,aes(x=PC1,y=PC2,color=kmeans.cluster))
p+geom_point() +
  theme_bw() + scale_y_continuous(labels = scales::comma) + 
  ggtitle(label='PCA with 10 cluster K-means')

fviz_cluster(kclust10, data = features, geom = "point",
             stand = FALSE, ellipse.type = "norm") + 
  theme_bw() + scale_y_continuous(labels = scales::comma) +
  ggtitle(label='Customer Clusters')

## retrieve customer ID's in each cluster
head(gather(data.frame(social_marketing[kclust10$cluster == 1,])))
## retrieve customer ID's in each cluster
head(gather(data.frame(social_marketing[kclust10$cluster == 2,])))
head(gather(data.frame(social_marketing[kclust10$cluster == 3,])))
head(gather(data.frame(social_marketing[kclust10$cluster == 4,])))
head(gather(data.frame(social_marketing[kclust10$cluster == 5,])))

#Customer segmentation through aggeration of results by mean
cluster.slice.kmeans.aggregate <- aggregate(features, by = list(kclust10$cluster), mean)
cluster<-c(cluster.slice.kmeans.aggregate$Group.1)
shop1<-c(cluster.slice.kmeans.aggregate$amount_purchased_shop_1)
shop2<-c(cluster.slice.kmeans.aggregate$amount_purchased_shop_2)
shop3<-c(cluster.slice.kmeans.aggregate$amount_purchased_shop_3)
shop4<-c(cluster.slice.kmeans.aggregate$amount_purchased_shop_4)
shop5<-c(cluster.slice.kmeans.aggregate$amount_purchased_shop_5)
# Plot a Bar graph
Legends <-c(rep("Customers Shop 1", 5), rep("Customers Shop 2", 5), rep("Customers Shop 3", 5), rep("Customers Shop 4", 5), rep("Customers Shop 5", 5))
values <-c(shop1,shop2,shop3,shop4,shop5)
mydata <-data.frame(cluster, values)
p <-ggplot(mydata, aes(cluster, values))
p +geom_bar(stat = "identity", aes(fill = Legends)) +
  xlab("Cluster") + ylab("Total") +
  ggtitle("Customer Segmentation") +
  theme_bw() + scale_y_continuous(labels = scales::comma)

# Work with centroids rather than users themselves
filter(social_marketing, X == 'y2g68vhkf')

kclust10$centers

kclust10$center[1,]*sigma + mu








