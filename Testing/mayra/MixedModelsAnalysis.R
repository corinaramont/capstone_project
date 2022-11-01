library(lme4)
library(lmerTest)
library(multilevel)
library(buildmer)
library(glmmLasso)

# buildmer2 <- function(x, data, ...)
# {
#   f = switch(as.character(class(x)), "formula" = x, formula(x))
#   buildmer(f, data, buildmerControl = buildmerControl(formuler, data, ...))@model
# }
# 
# buildmore <- function(model, data, crit)
# {
#   f = formula(model)
#   list(forward  = buildmer2(f, data, direction = 'forward',  crit = crit),
#        backward = buildmer2(f, data, direction = 'backward', crit = crit))
# }

loadRda <- function(file)
{
  env = new.env()
  load(file, env)
  env
}
corina <- loadRda("datasets/scrub_daddied_dataset.Rda")

data <- na.exclude(corina$Y)
full_linear_fit <- lmer(LifeExpect ~ CO + NO2 + Ozone + SO2 + PM2.5 + Year + (1|state), data = data)
no_SO2_fit <- lmer(LifeExpect ~ CO + NO2 + Ozone + PM2.5 + Year + (1|state), data = data)
anova(full_linear_fit, no_SO2_fit) #no_So2_fit is better 

################################################################################################################################
no_SO2_Ozone_fit <- lmer(LifeExpect ~ CO + NO2 + PM2.5 + Year + (1|state), data = data)
anova(no_SO2_Ozone_fit, no_SO2_fit) #no_SO2_Ozone_fit is better


#checking model assumptions
plot(fitted(no_SO2_Ozone_fit), residuals(no_SO2_Ozone_fit)) #random, no pattern  
#independence is not met since time relates with time 
qqnorm(resid(no_SO2_Ozone_fit)); qqline(resid(no_SO2_Ozone_fit)) #not quite normal, the ends do not follow normal line 
hist(resid(no_SO2_Ozone_fit)) #skewed to the left 
################################################################################################################################
 
log_lifeexpect <- lmer(log(LifeExpect) ~ CO + NO2 + PM2.5 + Year + (1|state), data = data)
anova(no_SO2_Ozone_fit, log_lifeexpect) #log_lifeexpect is better
plot(fitted(log_lifeexpect), residuals(log_lifeexpect)) 
hist(resid(log_lifeexpect))
qqnorm(resid(log_lifeexpect)); qqline(resid(log_lifeexpect))








lmer(LifeExpect ~ CO + NO2 + Ozone + SO2 + PM2.5 + Year + (1|state), data = data) #mixed model


model1 <- lm(response ~ variable1) #linear model 
model2 <- lm(response ~ variable1 + log(variable2)) #linear 

