library(mosaic)
library(tidyverse)
library(xlsx)

getwd()
setwd('/Users/howardyong/Documents/College/School/Spring 2020/Statistical Learning:Inference/exercise1')

ABIA = read_csv('ABIA.csv')
head(ABIA)
summary = summary(ABIA)

#Build new data frames for (flight vs. delays) and (flight vs. months)
flight_delay = ABIA %>%
  group_by(UniqueCarrier, Month) %>%
  summarize(TotalDelays = sum(ArrDelay, na.rm=TRUE))
flight_delay
flight_delay_df = data.frame(flight_delay)
flight_delay_df

#---TEST Flight_Delay----
#flight_delay2 = ABIA %>%
#  group_by(UniqueCarrier, Month) %>%
#  summarize(TotalProp = sum(ArrDelay >= 30)/n())
#flight_delay2

flight_vs_month = ABIA %>%
  group_by(UniqueCarrier, Month) %>%
  tally()
flight_vs_month
flight_month_df = data.frame(flight_vs_month)
flight_month_df

final_df = data.frame(flight_delay_df, flight_month_df[3])

plot = ggplot(final_df, aes(x=UniqueCarrier, y=TotalDelays/n, fill=ifelse((TotalDelays/n)>15, 'green', 'red'))) + 
  geom_bar(stat='identity') + facet_wrap(Month ~ .) + ggtitle('Monthly Average Arrival Delay\nPer Flight for Different Airlines') +
  labs(y="Average Arrival Delay/Flight (min/flight)", x = "Airline Unique Carrier") + scale_y_continuous(breaks=seq(0,100,10)) +
  scale_fill_discrete(name = "Delay Severity", labels = c("Heavy ( > 15min)", "Light ( < 15min)"), guide=guide_legend(reverse = TRUE))
plot
#1. find out why different rows in the 2 dfs 
#2. combine the 2 dfs into 1
#3. aggregate to find minDelay/flight per Month into final df
#4. make plots based off of final df