library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)

#load dataset Y
load("datasets/scrub_daddied_dataset.Rda")

#model
model = lmer(LifeExpect ~ CO + NO2 + PM2.5 + Year + (1|state), data = na.exclude(Y))
model_coef = summary(model)$coef
state_int = coef(model)$state[1]
Y = Y%>%filter(!state %in% c("Nebraska") )
State_coef = rep(state_int[,1], 20)
Y = cbind(Y, State_coef)
Y_new = na.exclude(Y)


#function to predict life expectancies
predict_life_expect = function(data_set, year, y, z){
  expect = c()
  model = lmer(LifeExpect ~ CO + NO2 + PM2.5 + Year + (1|state), data = Y_new)
  model_coef = summary(model)$coef
  
  for(i in 1:nrow(data_set)){
    x = data_set$CO[i]
    w = year
    temp = data_set$State_coef[i] + (model_coef[2]*x) + (model_coef[3]*y) + 
      (model_coef[4]*z) + (model_coef[5]*w)
    
    expect = c(expect, temp)
  }
  
  return(expect)
}

#2000
Y_2000 = Y_new%>%filter(Year==2000)
expect_2000 = predict_life_expect(Y_2000, 2000, y=mean(Y_2000$NO2),
                                  z=mean(Y_2000$PM2.5))

plot(Y_2000$CO, Y_2000$LifeExpect, xlab="CO Level", 
     ylab = "Average Life Expectancy (Years)", 
     main = "Average Life Expectancy vs. CO Level in 2000")
points(Y_2000$CO, expect_2000, col="red",pch=4)
legend(0.98,76,legend=c("Original", "Calculated"), cex=.9,
       col=c("black","red"),pch=c(1,4))

#compare differences in calculations vs original
Y_2000_new = data.frame(State = Y_2000$state, 
                        Actual.Life.Expect = Y_2000$LifeExpect,
                        Calc.Life.Expect = expect_2000, 
                        Prop.Life.Expect = round(((Y_2000$LifeExpect - expect_2000)/Y_2000$LifeExpect)*100,
                        digits=4))


#2011
Y_2011 = Y_new%>%filter(Year==2011)
expect_2011 = predict_life_expect(Y_2011, 2011,y=mean(Y_2011$NO2),
                                  z=mean(Y_2011$PM2.5))

plot(Y_2011$CO, Y_2011$LifeExpect, xlab="CO Level", 
     ylab = "Average Life Expectancy (Years)", 
     main = "Average Life Expectancy vs. CO Level in 2011")
points(Y_2011$CO, expect_2011, col="red",pch=4)
legend(0.12,76,legend=c("Original", "Calculated"), cex=.9,
       col=c("black","red"),pch=c(1,4))

#compare differences in calculations vs original
Y_2011_new = data.frame(State = Y_2011$state, Actual.Life.Expect = Y_2011$LifeExpect,
                        Calc.Life.Expect = expect_2011,
                        Prop.Life.Expect = round(((Y_2011$LifeExpect - expect_2011)/Y_2011$LifeExpect)*100,
                                                 digits=4))

#2019
Y_2019 = Y_new%>%filter(Year==2019)
expect_2019 = predict_life_expect(Y_2019, 2019,y=mean(Y_2019$NO2),
                                  z=mean(Y_2019$PM2.5))

plot(Y_2019$CO, Y_2019$LifeExpect, xlab="CO Level", 
     ylab = "Average Life Expectancy (Years)", 
     main = "Average Life Expectancy vs. CO Level in 2019")
points(Y_2019$CO, expect_2019, col="red",pch=4)
legend(0.12,76,legend=c("Original", "Calculated"), cex=.9,
       col=c("black","red"),pch=c(1,4))

#compare differences in calculations vs original
Y_2019_new = data.frame(State = Y_2019$state, Actual.Life.Expect = Y_2019$LifeExpect,
                        Calc.Life.Expect = expect_2019,
                        Prop.Life.Expect = round(((Y_2019$LifeExpect - expect_2019)/Y_2019$LifeExpect)*100,
                                                 digits=4))

######################################3

#2001
Y_2001 = Y_new%>%filter(Year==2001)
expect_2001 = predict_life_expect(Y_2001, 2001,y=mean(Y_2001$NO2),
                                  z=mean(Y_2001$PM2.5))
#compare differences in calculations vs original
Y_2001_new = data.frame(State = Y_2001$state, 
                        Actual.Life.Expect = Y_2001$LifeExpect,
                        Calc.Life.Expect = expect_2001,
                        Prop.Life.Expect = round(((Y_2001$LifeExpect - expect_2001)/Y_2001$LifeExpect)*100,
                                                 digits=4))

#2005
Y_2005 = Y_new%>%filter(Year==2005)
expect_2005 = predict_life_expect(Y_2005, 2005,y=mean(Y_2005$NO2),
                                  z=mean(Y_2005$PM2.5))
#compare differences in calculations vs original
Y_2005_new = data.frame(State = Y_2005$state, 
                        Actual.Life.Expect = Y_2005$LifeExpect,
                        Calc.Life.Expect = expect_2005,
                        Prop.Life.Expect = round(((Y_2005$LifeExpect - expect_2005)/Y_2005$LifeExpect)*100,
                                                 digits=4))

#2018
Y_2018 = Y_new%>%filter(Year==2018)
expect_2018 = predict_life_expect(Y_2018, 2018,y=mean(Y_2018$NO2),
                                  z=mean(Y_2018$PM2.5))
#compare differences in calculations vs original
Y_2018_new = data.frame(State = Y_2018$state, 
                        Actual.Life.Expect = Y_2018$LifeExpect,
                        Calc.Life.Expect = expect_2018,
                        Prop.Life.Expect = round(((Y_2018$LifeExpect - expect_2018)/Y_2018$LifeExpect)*100,
                                                 digits=4))