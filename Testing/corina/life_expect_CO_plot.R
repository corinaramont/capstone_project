library(ggplot2)
library(dplyr)
library(tidyr)
load("datasets/scrub_daddied_dataset.Rda")

#2000
Y_2000 = na.exclude(Y%>%filter(Year==2000))

ggplot(data=Y_2000, aes(x = CO, y=LifeExpect)) + geom_point() + 
  ggtitle("Life Expectancy vs. CO Level in 2000")

#2011

#2019
