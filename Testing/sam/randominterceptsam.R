library(lme4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(HRW)
library(mgcv)
library(multilevel)
library(buildmer)
library(splines)
library(lmerTest)

load("datasets/scrub_daddied_dataset.Rda")

######### Attempting to use MGCV to do the random intercepts
colnames(Y)

length(unique(Y$state))
p = 2
paste0("as.integer(state== unique(state)[",1:p,"])",collapse="+")

fitlinear = gamm(LifeExpect ~ Year + 
                   paste0("as.integer(state== unique(state)[",1:p,"])",collapse="+"), 
                 random = list(state = ~1), data = Y)
summary(fitlinear$gam)

year = Y$Year[Y$state == "Alabama"][-18]
lifeexpect = (fitlinear$gam$fitted.values)[1:19]
fitlinear$gam$fitted.values[seq(from = 1, to = 500, by = 19)]

plot(year, lifeexpect)


addline = function(state, col){
  year = Y$Year[Y$state == state]
  lifeexpect1 = fitlinear$gam$fitted.values[Y$state == state]
  points(year, lifeexpect1, col = col)
}
plot(fitlinear$gam$fitted.values, fitlinear$gam$residuals)

fitlinear$gam$fitted.values[1:20]

plot(year, lifeexpect)
addline("Arizona", 3)
addline("Arkansas", 2)


for (i in 1:10) {
  addline(unique(Y$state)[i], col = i)
  
}



#Using lme4 with splines

#Original Model
fit <- lmer(LifeExpect ~ CO + NO2 + Ozone + SO2 + PM10 + PM2.5 + Year + (1|state), verbose = T, data = Y)
plot(residuals(fit), fitted(fit))
plot(fit)
qqnorm(resid(fit)); qqline(resid(fit)) #not quite as normal 
hist(resid(fit))                       #slightly skewed to the left 
summary(fit)
ICC1(aov.1); ICC2(aov.1)

#Added the spline fit
splinefit <- lmer(LifeExpect ~ bs(CO) + bs(NO2) + bs(Ozone) + bs(SO2) + bs(PM10) + bs(PM2.5) + (1|state), verbose = T, data = Y)
plot(residuals(splinefit), fitted(splinefit))
plot(splinefit)
qqnorm(resid(splinefit)); qqline(resid(splinefit)) # Maybe a bit better than fit

#Quadratic term for year
fityearquad <- lmer(LifeExpect ~ CO + NO2 + Ozone + SO2 + PM10 + PM2.5 + Year + I(Year^2) + (1|state), verbose = T, data = Y)
summary(fityearquad)
qqnorm(resid(fityearquad)); qqline(resid(fityearquad))
#I think the quad term for year is pretty good











