library(ggplot2)
library(dplyr)
library(tidyr)

#load dataset Y
load("datasets/scrub_daddied_dataset.Rda")

#model
Y_new = na.exclude(Y)
model = lmer(LifeExpect ~ CO + NO2 + PM2.5 + Year + (1|state), data = Y_new)
model_coef = summary(model)$coef
expect = c()

#function to predict life expectancies
predict_life_expect = function(data_set, year){
  expect = c()
  model = lmer(LifeExpect ~ CO + NO2 + PM2.5 + Year + (1|state), data = Y_new)
  model_coef = summary(model)$coef
  
  for(i in 1:nrow(data_set)){
    x = data_set$CO[i]
    y = data_set$NO2[i]
    z = data_set$PM2.5[i]
    w = year
    temp = model_coef[1] + (model_coef[2]*x) + (model_coef[3]*y) + 
      (model_coef[4]*z) + (model_coef[5]*w)
    
    expect = c(expect, temp)
  }
  
  return(expect)
}

#2000
Y_2000 = na.exclude(Y%>%filter(Year==2000))

ggplot(data=Y_2000, aes(x = CO, y=LifeExpect)) + geom_point() + 
  ggtitle("Life Expectancy vs. CO Level in 2000")


expect_2000 = predict_life_expect(Y_2000, 2000)
plot(Y_2000$CO, expect_2000)

#2011
Y_2011 = na.exclude(Y%>%filter(Year==2011))

ggplot(data=Y_2011, aes(x = CO, y=LifeExpect)) + geom_point() + 
  ggtitle("Life Expectancy vs. CO Level in 2011")

expect_2011 = predict_life_expect(Y_2011, 2011)
plot(Y_2011$CO, expect_2011)

#2019
Y_2019 = na.exclude(Y%>%filter(Year==2019))

ggplot(data=Y_2019, aes(x = CO, y=LifeExpect)) + geom_point() + 
  ggtitle("Life Expectancy vs. CO Level in 2019")

expect_2019 = predict_life_expect(Y_2019, 2019)
plot(Y_2019$CO, expect_2019)