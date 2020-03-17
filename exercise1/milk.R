library(mosaic)
library(tidyverse)

getwd()
setwd('/Users/howardyong/Documents/College/School/Spring 2020/Statistical Learning:Inference/exercise1')
milk = read_csv('milk.csv')

plot(sales ~ price, data=milk)
plot(log(sales) ~ log(price), data=milk)
lm_ped = lm(log(sales) ~ log(price), data=milk)
coefficients = coef(lm_ped)
coefficients

#Power regression = 112.236*P^(-1.618567)
#N = (P-C)*(112.24P^(-1.618567))
c <- readline(prompt='Enter unit-cost: ')
curve((x-as.integer(c))*(112.24*x^(-1.618567)), from=1, to=10)
optimal_net_profit = 2.61292 * as.integer(c)
paste0("Optimal Price: ", optimal_net_profit)
