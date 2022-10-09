library(lme4)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("~/capstone_project/datasets")
load("clean_pollutant_life_data.Rda")

pollcol = function(years, states, param){
  return((cleaned_pollutants_data%>%
            filter(year %in% years, state == states, parameter == param))$annualmean)
}


#lmer(lifeexpect ~  ,data=bama)