library(dplyr)
library(tidyverse)
library(ggplot2)

##############################FUNCTIONS START###################################

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


get_abrv <- function(state_code)
{
  state_info[state_info$FIPS == state_code,]$abrv
}

get_region <- function(state_code)
{
  state_info[state_info$FIPS == state_code,]$region
}


bind_data <- function(data){
  cbind(data,
        abrv   = transmute(data$state_code, get_abrv),
        region = transmute(data$state_code, get_region)
  )
}


summarise_region <- function(data, region){
  (data %>% filter(state_code %in% region) %>% summarise(avg = mean(stand_mean_avg)))$avg
}


summarise_regions <- function(data){
  cbind(
    data$year,
    summarise_region(data, regions$northeast),
    summarise_region(data, regions$midwest),
    summarise_region(data, regions$south),
    summarise_region(data, regions$west)
  )
}

#######################################FUNCTIONS END############################

state_info = readRDS("datasets/state_info.dat")

bound_data = bind_data(all_data)

expand.data <- function(data, ...)
{
  grid <- expand.grid(sapply(list(...), unique))
  merge(data, grid, all = T)
}

vals <- expand.data(CO_regions, CO_regions$year, CO_regions$abrv)

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
