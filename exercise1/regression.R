library(tidyverse)
library(mosaic)

creatinine = read_csv('creatinine.csv')

# Problem 2: Regression Practice

# age: patient's age in years. (X)
# creatclear: patient's creatine clearance rate in mL/minute, a measure of kidney health (higher is better). (Y)

# Linear Regresion Model
lrm = lm(creatclear ~ age, data=creatinine)

summary(lrm)

# 1) What creatinine clearance rate should we expect, on average, for a 55-year-old?
new_data = data.frame(age = c(55))
predict(lrm, new_data)
# About 113.723 mL/minute

# 2) How does creatinine clearance rate change with age? (This should be a number with units ml/minute per year.)
coef(lrm)
# y = -0.6198159X + 147.8129158
# For every one year increase in age, creatinine clearance rate is expected to drop by an average of about 0.6198159 mL/minute

# 3) Whose creatinine clearance rate is healthier (higher) for their age: a 40-year-old with a rate of 135, or a 60-year-old with a rate of 112?
new_data = data.frame(age = c(40))
predict(lrm, new_data)
# 123.0203

new_data = data.frame(age = c(60))
predict(lrm, new_data)
# 110.624

# According to the regression model, a 40-year-old with a rate of 135 is healthier (higher) then a 60-year-old with a rate of 112
# The rate of 135 mL/min is significantly higher than the predicted rate of 123.0203 mL/min for an age of 40 years old 
# This is better than the rate of 112 mL/min compared to a predicted rate of 110.624 mL/min for an age of 60 years old

# Include a plot of clearance rate vs age, along with your fitted model used to reach these conclusions.
plot(creatclear ~ age, data=creatinine)
plotModel(lrm)
