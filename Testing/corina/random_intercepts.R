library(lme4)
library(dplyr)
library(tidyr)
library(ggplot2)

#cleaned_pollutants_data = readRDS("datasets/cleaned_pollutant_data.dat")
#life_expect = readRDS("datasets/life_expectancies_1959_2019.dat")

cleaned_pollutants_data = readRDS("/Users/corinaramont/Documents/cleaned_pollutant_data.dat")
life_expect = readRDS("/Users/corinaramont/Documents/life_expectancies_1959_2019.dat")

#function to filter annualmeans by years, state name, param
pollcol = function(years, states, param){
  annualmeans_by_state=c()
  
  for (i in c(years)) {
    temp = cleaned_pollutants_data %>% 
      filter(year == i, state==states, parameter == param)
    if(nrow(temp) == 0){
      #adds NA value if particular year does not have data
      annualmeans_by_state = c(annualmeans_by_state, NA)
    }else{
      annualmeans_by_state = c(annualmeans_by_state, temp$annualmean)
    } 
  }
  
  #returns vector of annualmeans by state, year, parameter
  return(annualmeans_by_state)
}

#getting big dataframe
X = c()
Y = c()
for(i in c(1:48)){
  print(i)
  state_name = unique(cleaned_pollutants_data$state)[i]
  state_abb = unique(life_expect$State)[i]
  X = c(rep(state_name, 20))
  X = cbind(X, c(2000:2019))
  for (j in c(1:5,8)) {
    param = unique(cleaned_pollutants_data$parameter)[j]
    #print(j)
    X = cbind(X, pollcol(2000:2019, state_name, param = param))
  }
  life_expect_states = (life_expect %>% 
                          filter(Year %in% (2000:2019), State==state_abb))$Life_Expectancies
  life_expect_states1 = c(life_expect_states[1:17], NA, life_expect_states[18:19])
  X = cbind(X, life_expect_states1)
  
  Y = rbind(Y, X)
  X=c()
}


colnames(Y) = c("state", "Year","CO", "NO2", "Ozone", "SO2", "PM10", "PM2.5", "LifeExpect")
Y = as.data.frame(Y)

for(i in 2:9){
  Y[,i] = as.double(Y[,i])
}
save(Y,file = "/Users/corinaramont/capstone_project/datasets/scrub_daddied_dataset.Rda")

######################################################
load("datasets/scrub_daddied_dataset.Rda")
library(lme4)

pollutant_name = c("CO", "NO2", "Ozone", "SO2", "PM10", "PM2.5")

Y_new = na.exclude(Y)
#normal linear regression
lin_out = lm(LifeExpect ~ CO + NO2 + Ozone + SO2 + PM10 + PM2.5 
               + Year ,data = Y_new)
summary(lin_out)

#random intercepts model: full model
rand_int_out = lmer(LifeExpect ~ CO + NO2 + Ozone + SO2 + PM10 + PM2.5 
                    + Year + (1|state) ,data = Y_new)
summary(rand_int_out)

#full model with year^2
rand_int_out1 = lmer(LifeExpect ~ CO + NO2 + Ozone + SO2 + PM10 + PM2.5 
                    + (Year**2) + (1|state) ,data = Y_new)
summary(rand_int_out1)

#check random intercepts model assumptions?
plot(rand_int_out)

qqnorm(resid(rand_int_out))
qqline(resid(rand_int_out))

#model w/o PM10 & SO2
rand_int_out2 = lmer(LifeExpect ~ CO + NO2 + Ozone + PM2.5 
                    + Year + (1|state) ,data = Y_new)
summary(rand_int_out2)

#w/o PM10 only
rand_int_out3 = lmer(LifeExpect ~ CO + NO2 + Ozone + PM2.5 + SO2 
                     + Year + (1|state) ,data = Y_new)
summary(rand_int_out3)

#w/o PM10 only and year^2
rand_int_out4 = lmer(LifeExpect ~ CO + NO2 + Ozone + PM2.5 + SO2 
                     + (Year**2) + (1|state) ,data = Y_new)
#anova
anova(rand_int_out, rand_int_out1, rand_int_out2, rand_int_out3,
      rand_int_out4)
