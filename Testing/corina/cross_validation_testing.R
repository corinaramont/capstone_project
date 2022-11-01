library(tidyverse)
library(caret) #cross validation methods
library(lme4)

#load dataset Y
load("datasets/scrub_daddied_dataset.Rda")

Y_new = na.exclude(Y)
#origmodel 
#= lmer(LifeExpect ~ CO + NO2 + PM2.5 + Year + (1|state), data = Y_new)

###########validation set approach##############
set.seed(123)

#create training data as 80% of dataset
random_sample = createDataPartition(Y_new$LifeExpect,
                                     p = 0.8, list = F)

training_data = Y_new[random_sample,]
testing_data = Y_new[-random_sample,]

model = lmer(LifeExpect ~ CO + NO2 + PM2.5 + Year + (1|state), 
             data = training_data)

predictions = predict(model, testing_data)

data.frame(R2 = R2(predictions, testing_data$LifeExpect),
           RMSE = RMSE(predictions, testing_data$LifeExpect),
           MAE = MAE(predictions, testing_data$Life_Expect))
#doesn't return MAE??
model_MSE = mean((predictions-testing_data$LifeExpect)^2)
#MSE = 0.1752
model_MAE = mean(abs((predictions-testing_data$LifeExpect))) #0.3480

##############3Leave One Out Cross Validation##################
train_control = trainControl(method = "LOOCV")

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model =- train(LifeExpect ~ CO + NO2 + PM2.5 + Year + (1|state), data = Y_new,
               method = "lmerMod",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)



