library(dplyr)
library(tidyverse)
library(ggplot2)

printf <- function(fmt, ...) { print(sprintf(fmt, ...)) }

normalizer <- function(parameter, units)
{
  if (units == "Parts per million")
  {
    return(1000)
  }
  if (units == "Micrograms/cubic meter (LC)")
  {
    return(0)
    stop("No scaling for micrograms!")
  }
  return(1)
}
# calculates multipliers for each row's mean
multipliers = mapply(normalizer, all_data$parameter, all_data$units_of_measure, USE.NAMES = F)

# normalizes means using multipliers
standardized_means = all_data$arithmetic_mean * multipliers

df <- data.frame(cbind(all_data%>%select(state_code, county_code, site_number, parameter_code, 
                        parameter, year, standard_deviation, first_max_value, 
                        second_max_value), standardized_means))

#analysis for carbon monoxide (CO)
CO <- df%>%filter(parameter_code == '42101')%>%group_by(year, state_code)%>%
  summarise(stand_mean_avg = mean(standardized_means))

#plotting C0 mean for every state in each year, 1980 - 2019 
ggplot(data = CO, aes(x = year, y = stand_mean_avg)) + 
  geom_line(aes(color = state_code))

