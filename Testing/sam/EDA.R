
library(dplyr)
library(tidyverse)
library(ggplot2)

load_data <- function() { readRDS("datasets/all_data.dat") }

all_data = load_data()

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

df <- data.frame(cbind(all_data%>%select(state, state_code, county_code, site_number, parameter_code, 
                                         parameter, year, standard_deviation, first_max_value, 
                                         second_max_value, arithmetic_mean), standardized_means))

pm25 = df %>% filter(parameter == "Acceptable PM2.5 AQI & Speciation Mass") %>% group_by(state, parameter, year) %>% summarise(annualmean = mean(arithmetic_mean))



ggplot(pm25) +
  geom_line(aes(x = year, y = annualmean, color = state)) +
  ylab("Acceptable PM2.5 AQI & Speciation Mass")


