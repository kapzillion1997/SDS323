library(mosaic)
library(tidyverse)

getwd()
setwd('/Users/howardyong/Documents/College/School/Spring 2020/Statistical Learning:Inference/exercise1')
greenbuildings = read_csv('greenbuildings.csv')

#FACTOR 1: AMENITIES
#Part 1: Tabulate comparisons for 3 categorical variabls (class_a, class_b, amenities)
xtabs(~amenities + green_rating, data=greenbuildings) %>%
  prop.table(margin=2)

pct_green_amen = greenbuildings %>%
  group_by(green_rating) %>%
  summarize(pct_amen = sum(amenities==1)/n())
pct_green_amen

avg_rent_amen = greenbuildings %>%
  group_by(amenities) %>%
  summarize(mean_rent = mean(Rent,na.rm=TRUE))
avg_rent_amen

#Part 2: Plot both the relatinoships in bar charts
pct_green_amen_plot = ggplot(data = pct_green_amen) +
  geom_bar(mapping = aes(x=green_rating, y=pct_amen, fill=green_rating), stat='identity', width=0.3) +
  ggtitle('Proportion of Green & Non-Green\nBuildings with Amenities') +
  labs(y='Prop. of Buildings w/ Amenities', x='Green Rating') + scale_x_continuous(breaks=c(0, 1), labels=c('Non-Green','Green')) +
  geom_text(aes(x=green_rating, y=pct_amen + 0.025, label=round(pct_amen,2))) + theme(legend.position = 'none')
pct_green_amen_plot

avg_rent_amen_plot = ggplot(data = avg_rent_amen) +
  geom_bar(mapping = aes(x=amenities, y=mean_rent, fill=amenities), stat='identity', width=0.3) + 
  ggtitle('Average Rent for\nBuildings w/ Amenities') +
  labs(y='Average Rent ($/sqft)', x='Amenities Status') + scale_x_continuous(breaks=c(0, 1), labels=c('No Amenities', 'Amenities')) +
  geom_text(aes(x=amenities, y=mean_rent + 1, label=round(mean_rent,2))) + theme(legend.position = 'none')
avg_rent_amen_plot

#FACTOR 2: CLASS
#CLASS A
xtabs(~class_a + green_rating, data=greenbuildings) %>%
  prop.table(margin=2)

pct_green_classA = greenbuildings %>%
  group_by(green_rating) %>%
  summarize(pct_classA = sum(class_a==1)/n())
pct_green_classA
  
avg_rent_classA = greenbuildings %>%
  group_by(class_a) %>%
  summarize(mean_rent2 = mean(Rent,na.rm=TRUE))
avg_rent_classA

pct_green_classA_plot = ggplot(data = pct_green_classA) +
  geom_bar(mapping = aes(x=green_rating, y=pct_classA, fill=green_rating), stat='identity', width=0.3) +
  ggtitle('Proportion of Green & Non-Green\nBuildings with Class A Status') +
  labs(y='Prop. of Buildings Class A', x='Green Rating') + scale_x_continuous(breaks=c(0, 1), labels=c('Non-Green','Green')) +
  geom_text(aes(x=green_rating, y=pct_classA + 0.025, label=round(pct_classA,2))) + theme(legend.position = 'none')
pct_green_classA_plot

avg_rent_classA_plot = ggplot(data = avg_rent_classA) +
  geom_bar(mapping = aes(x=class_a, y=mean_rent2, fill=class_a), stat='identity', width=0.3) + ggtitle('Average Rent for\nBuildings w/ Class A Status') +
  labs(y='Average Rent ($/sqft)', x='Class A Status') + scale_x_continuous(breaks=c(0, 1), labels=c('Non-Class A', 'Class A')) +
  geom_text(aes(x=class_a, y=mean_rent2 + 1, label=round(mean_rent2,2))) + theme(legend.position = 'none')
avg_rent_classA_plot

#CLASS B
xtabs(~class_b + green_rating, data=greenbuildings) %>%
  prop.table(margin=2)

pct_green_classB = greenbuildings %>%
  group_by(green_rating) %>%
  summarize(pct_classB = sum(class_b==1)/n())
pct_green_classB

avg_rent_classB = greenbuildings %>%
  group_by(class_b) %>%
  summarize(mean_rent3 = mean(Rent,na.rm=TRUE))
avg_rent_classB

pct_green_classB_plot = ggplot(data = pct_green_classB) +
  geom_bar(mapping = aes(x=green_rating, y=pct_classB, fill=green_rating), stat='identity', width=0.3) +
  ggtitle('Proportion of Green & Non-Green\nBuildings with Class B Status') +
  labs(y='Prop. of Buildings Class B', x='Green Rating') + scale_x_continuous(breaks=c(0, 1), labels=c('Non-Green','Green')) +
  geom_text(aes(x=green_rating, y=pct_classB + 0.025, label=round(pct_classB,2))) + theme(legend.position = 'none')
pct_green_classB_plot

avg_rent_classB_plot = ggplot(data = avg_rent_classB) +
  geom_bar(mapping = aes(x=class_b, y=mean_rent3, fill=class_b), stat='identity', width=0.3) + 
  ggtitle('Average Rent for\nBuildings w/ Class B Status') +
  labs(y='Average Rent ($/sqft)', x='Class B Status') + scale_x_continuous(breaks=c(0, 1), labels=c('Non-Class B', 'Class B')) +
  geom_text(aes(x=class_b, y=mean_rent3 + 1, label=round(mean_rent3,2))) + theme(legend.position = 'none')
avg_rent_classB_plot
