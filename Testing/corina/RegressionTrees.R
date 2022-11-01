library(rpart)
library(lme4)
library(lmerTest)
library(glmertree)
library(dplyr)
library(caret)

#load dataset Y
load("datasets/scrub_daddied_dataset.Rda")

Y_new = na.exclude(Y)

#generalized linear mixed effects model trees

############### full model ##################################
model_trees = lmertree(LifeExpect ~ 1 | state | 
                         Year + CO + NO2 + Ozone + SO2 + PM10 + PM2.5,
                       data = Y_new)
plot(model_trees)

resids = residuals(model_trees)
preds = predict(model_trees)

state_counts = as.data.frame(table(Y_new$state))
x_var = c()
for(i in 1:47){
  x_var = c(x_var, rep(i, state_counts$Freq[i]))
}
par(mfrow=c(1,1))
plot(state_counts, resids)
scatter.smooth(preds, resids)

############### orig model ##################################
orig_model_trees = lmertree(LifeExpect ~ 1 | state | CO + NO2 + PM2.5 + Year,
                            data = Y_new)
plot(orig_model_trees)
resids = residuals(orig_model_trees)
preds = predict(orig_model_trees)
state_counts = as.data.frame(table(Y_new$state))
x_var = c()
for(i in 1:47){
  x_var = c(x_var, rep(i, state_counts$Freq[i]))
}
par(mfrow=c(1,1))
plot(state_counts, resids)
scatter.smooth(preds, resids)

MSE = mean((preds - Y_new$LifeExpect)^2) #0.0882
MAE = mean(abs((preds - Y_new$LifeExpect))) #0.2316

############### orig model w/ cross valid??? ##################################

