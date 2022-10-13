library(lme4)
library(multilevel)
library(buildmer)
#corina <- loadRda("datasets/scrub_daddied_dataset.Rda")

data <- corina$Y
fit <- lmer(LifeExpect ~ CO + NO2 + Ozone + SO2 + PM10 + PM2.5 + Year + (1|state), verbose = T, data = data)
plot(residuals(fit), fitted(fit))
plot(fit)
qqnorm(resid(fit)); qqline(resid(fit)) #not quite as normal 
hist(resid(fit))                       #slightly skewed to the left 
aov.1 <- aov(LifeExpect ~ CO + NO2 + Ozone + SO2 + PM10 + PM2.5 + Year, data = data)
summary(aov.1)
ICC1(aov.1); ICC2(aov.1)


fit2 <- lmer(LifeExpect ~ CO + NO2 + Ozone + SO2 + PM10 + PM2.5 + Year + (1|state) + (1|Year), data = data) #prob not a good model because fixed year doesn not add much to variance 
plot(residuals(fit2), fitted(fit2))
plot(fit2)
qqnorm(resid(fit2)); qqline(resid(fit2)) #much more normal 



fit3 <- lmer(log(LifeExpect) ~ CO + NO2 + Ozone + SO2 + PM10 + PM2.5 + Year + (1|state), data = data)
plot(residuals(fit3), fitted(fit3))
plot(fit3)
qqnorm(resid(fit3)); qqline(resid(fit3)) #BIC = -5724.332



fit4 <- lmer(LifeExpect ~ CO + NO2 + PM2.5 + Year + (1|state), verbose = T, data = data)
summary(fit4)
