library(dplyr)
library(tidyverse)
library(ggplot2)

##############################FUNCTIONS START###################################







summarise_region <- function(data, region){
  (data %>% filter(state_code %in% region) %>% summarise(avg = mean(stand_mean_avg)))$avg
}

summarise_regions <- function(data) {} # TODO

#######################################FUNCTIONS END############################

CO_regions = bind_data(CO)
CO_regions_plot <- CO_regions%>%group_by(year, region)%>%summarise(means = mean(stand_mean_avg))


# calculates multipliers for each row's mean
multipliers = mapply(normalizer, all_data$parameter, all_data$units_of_measure, USE.NAMES = F)

# normalizes means using multipliers
standardized_means = all_data$arithmetic_mean * multipliers

df <- data.frame(cbind(all_data%>%select(state_code, county_code, site_number, parameter_code, 
                        parameter, year, standard_deviation, first_max_value, 
                        second_max_value), standardized_means))

#analysis for carbon monoxide (CO)
CO <- df%>%filter(parameter_code == '42101')%>%group_by(year, state_code)%>%
  summarise(stand_mean = mean(standardized_means))

#plotting C0 mean for every state in each year, 1980 - 2019
ggplot(data = CO_regions, aes(x = year, y = stand_mean_avg)) +
  geom_line(aes(color = abrv)) + ggtitle('Carbon Monoxide(CO) Levels') + 
  ylab('CO average (ppb)') + xlab('year') + theme(legend.title = 'States')

hist(CO_regions$stand_mean_avg) #the data is skewed to the right 

#looking at life exp and CO
life1980 <- life_expect%>%filter(Year >= 1980)
new_df <- cbind(life1980, CO_regions)



#goals: look at the missing data 
#want to make a plot for each pollutant for every state in every year. (will probably want a function) 
#for ^, look at distributions and outliers 
# add a region column to all_data? maybe
#gotta look at the missind data and run a test on it 
