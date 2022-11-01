library(rpart)
library(lme4)
library(lmerTest)
library(glmertree)


#load dataset Y
load("datasets/scrub_daddied_dataset.Rda")

Y_new = na.exclude(Y)

#generalized linear mixed effects model trees
model_trees = lmertree(LifeExpect ~ 1 | state | 
                         Year + CO + NO2 + Ozone + SO2 + PM10 + PM2.5,
                       data = Y_new)

plot(model_trees)