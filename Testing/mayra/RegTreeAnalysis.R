library(rpart)
library(lme4)
library(lmerTest)

#loading data###################################################################
loadRda <- function(file)
{
  env = new.env()
  load(file, env)
  env
}
corina <- loadRda("datasets/scrub_daddied_dataset.Rda")
data <- na.exclude(corina$Y)


#regresssion tree for full linear fit###########################################
full_fit <- rpart(formula = LifeExpect ~ Year + CO + NO2 + Ozone + SO2 + PM10 + PM2.5, data = data, method = 'anova')

printcp(full_fit)                                                       #getting the results
plot(full_fit, uniform = T, main="Regression Tree for Life Expectancy"); text(full_fit, use.n = T, all = T, cex=.8) #plotting the tree 

par(mfrow=c(1,2))   #two plots on one page
rsq.rpart(full_fit) #visualize cross-validation results


#pruning the tree###############################################################
pfit<- prune(full_fit, cp= full_fit$cptable[which.min(full_fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=TRUE, main="Pruned Regression Tree for Life Expectancy")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)






full_fit2 <- rpart(formula = LifeExpect ~ CO + NO2 + Ozone + SO2 + PM2.5 + Year + (1|state), data = data, method = 'anova')
