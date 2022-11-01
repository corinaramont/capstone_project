library(tidyverse)
library(caret) #cross validation methods
library(lme4)

#load dataset Y
load("datasets/scrub_daddied_dataset.Rda")

Y_new = na.exclude(Y)
#origmodel 
#= lmer(LifeExpect ~ CO + NO2 + PM2.5 + Year + (1|state), data = Y_new)

#validation set approach
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

