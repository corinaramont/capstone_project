library(dplyr)
library(tidyverse)
library(ggplot2)
library("gridExtra")                        # Load gridExtra package

#loads data
load_data <- function() { readRDS("~/Documents/fall 2022/all_data.dat") }

all_data = load_data()

#########################################

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
#seeing which missing values we have
which(is.na(all_data$arithmetic_mean))

all_data[527098,]$parameter
all_data[527100,]$parameter

all_data = all_data[-c(527100, 527098),]

# calculates multipliers for each row's mean
multipliers = mapply(normalizer, all_data$parameter, all_data$units_of_measure, USE.NAMES = F)

# normalizes means using multipliers
standardized_means = all_data$arithmetic_mean * multipliers


#Checks missing measurements
sum(is.na(all_data$arithmetic_mean))

df <- data.frame(cbind(all_data%>%select(state, state_code, county_code, site_number, parameter_code, 
                                         parameter, year, standard_deviation, first_max_value, 
                                         second_max_value, arithmetic_mean), standardized_means))

#regions by state codes
newengland_codes = c("09","23","25","33","44","50")
middleatlantic_codes = c("34","36","42")
northeastcentral_codes = c("17","18","26","39","55")
westnorthcentral_codes = c("19","20","27","29","31","38","46")
southatlantic_codes = c("10",'11','12','13','24','37','45','51','54')
eastsouthcentral_codes = c('01','21','28','47')
westsouthcentral_codes = c('05','22','40','48')
mountain_codes = c('04','08','16','30','32','35','49','56')
pacific_codes = c('02','06','15','41','53')


#This starts the process of adding a division column to all_data
temp1 = all_data %>%filter(state_code %in% newengland_codes) %>% mutate(division = "New England")
temp2 = all_data %>%filter(state_code %in% middleatlantic_codes) %>% mutate(division = "Middle Atlantic")
temp3 = all_data %>%filter(state_code %in% northeastcentral_codes) %>% mutate(division = "East North Central")
temp4 = all_data %>%filter(state_code %in% westnorthcentral_codes) %>% mutate(division = "West North Central")
temp5 = all_data %>%filter(state_code %in% southatlantic_codes) %>% mutate(division = "South Atlantic")
temp6 = all_data %>%filter(state_code %in% eastsouthcentral_codes) %>% mutate(division = "East South Central")
temp7 = all_data %>%filter(state_code %in% westsouthcentral_codes) %>% mutate(division = "West South Central")
temp8 = all_data %>%filter(state_code %in% mountain_codes) %>% mutate(division = "Mountain")
temp9 = all_data %>%filter(state_code %in% pacific_codes) %>% mutate(division = "Pacific")

#now we have the divisions in the data
all_data = rbind(temp1,
                 temp2,
                 temp3,
                 temp4,
                 temp5,
                 temp6,
                 temp7,
                 temp8,
                 temp9)


#Aggregates the data by year, state, and parameter and gives the annual mean
pollutantsbyyearandstate = all_data  %>% group_by(year, state, parameter, division) %>%
  summarise(annualmean = mean(arithmetic_mean))


# plots only the Carbon Monoxide annual measurements over time
# Plots all 50 states as seperate lines, but colors them by division for 
# easier reading
ggplot(pollutantsbyyearandstate %>% filter(parameter == "Carbon monoxide")) +
  geom_line(aes(x = year, y = annualmean, group = state, color = division)) +
  ylab("Annual mean measurement") + 
  ggtitle("Particlate Matter 2.5 micrometers and smaller")


#This plots all All the parameter of interest over time similar to the plot above
for(i in 1:length(unique(pollutantsbyyearandstate$parameter))){
  print(i)
  print(ggplot(pollutantsbyyearandstate %>% filter(parameter == unique(pollutantsbyyearandstate$parameter)[i])) +
          geom_line(aes(x = year, y = annualmean, group = state, color = division)) +
          ylab("Annual mean measurement") + ggtitle(unique(pollutantsbyyearandstate$parameter)[i]))
}

cleaned = all_data %>% group_by(year,state_code,parameter_code)

######corina######
load("~/capstone_project/datasets/life_expectancies_1959_2019.Rda")
clean_life_expect = life_expect%>%filter(!Year %in% c(1959:1979))

#create new column
cleaned_pollutants_data = pollutantsbyyearandstate
life_expect1 = data.frame(lifeexpect = rep(0, nrow(cleaned_pollutants_data)))
cleaned_pollutants_data = cbind(cleaned_pollutants_data, life_expect1)

#variables
years = c(1980:2016, 2018, 2019)
statenames = state.name[c(-2,-11)]
stateabb = state.abb[c(-2,-11)]
life_expect_value = 0

#clean up
for(i in 1:39){
  
  for(j in 1:48){
    cat(i,j)
    
    life_expect_value = (clean_life_expect%>%
                           filter(State == stateabb[j] & Year == years[i]))$Life_Expectancies
    
    index_values = which(cleaned_pollutants_data$year == years[i] & cleaned_pollutants_data$state == statenames[j])
    
    if(length(index_values) == 0){
      
      next
      
    }else{
      
      for(k in 1:length(index_values)){
        
        cleaned_pollutants_data[index_values[k],]$lifeexpect = life_expect_value
        
      }
    }
  }  
  
}

cleaned_pollutants_data$lifeexpect = ifelse(cleaned_pollutants_data$lifeexpect==0, NA, cleaned_pollutants_data$lifeexpect)
save(cleaned_pollutants_data, file = "/capstone_project/datasets/cleaned_pollutant_data.dat")

save(cleaned_pollutants_data, file="clean_pollutant_life_data.Rda")

for(i in 1:length(unique(cleaned_pollutants_data$parameter))){
  print(i)
  print(ggplot(cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[i])) +
          geom_line(aes(x = lifeexpect, y = annualmean, group = state, color = division)) +
          ylab("Annual mean measurement") + ggtitle(unique(cleaned_pollutants_data$parameter)[i]))
}

#carbon monoxide
print(ggplot(cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[1])) +
        geom_line(aes(x = annualmean, y = lifeexpect, group = state, color = division)) +
        ylab("Life Expectancy") + xlab("Annual Mean Measurement") + ggtitle(unique(cleaned_pollutants_data$parameter)[1]))

#mountain (idaho, utah) & new england (new hampshire) have high stuff in 1980s
cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[1] & annualmean > 2.5)

#nitrogen dioxide
print(ggplot(cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[2])) +
        geom_line(aes(x = annualmean, y = lifeexpect, group = state, color = division)) +
        ylab("Life Expectancy") + xlab("Annual Mean Measurement") + ggtitle(unique(cleaned_pollutants_data$parameter)[2]))

#nevada has annualmean over 50 in 1980s
cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[2] & annualmean > 50)

#Ozone
print(ggplot(cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[3])) +
        geom_line(aes(x = annualmean, y = lifeexpect, group = state, color = division)) +
        ylab("Life Expectancy") + xlab("Annual Mean Measurement") + ggtitle(unique(cleaned_pollutants_data$parameter)[3]))

#new england (connecticut), south atlantic (Delaware), Mountain (Utah), east south central (tennessee)
cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[3] & annualmean > 0.06)

#sulfur dioxide
print(ggplot(cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[4])) +
        geom_line(aes(x = annualmean, y = lifeexpect, group = state, color = division)) +
        ylab("Life Expectancy") + xlab("Annual Mean Measurement") + ggtitle(unique(cleaned_pollutants_data$parameter)[4]))

#mountain (arizona & wyoming)
cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[4] & annualmean > 40)

#PM10 STP
print(ggplot(cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[5])) +
        geom_line(aes(x = annualmean, y = lifeexpect, group = state, color = division)) +
        ylab("Life Expectancy") + xlab("Annual Mean Measurement") + ggtitle(unique(cleaned_pollutants_data$parameter)[5]))

#PM2.5 AQI
print(ggplot(cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[6])) +
        geom_line(aes(x = annualmean, y = lifeexpect, group = state, color = division)) +
        ylab("Life Expectancy") + xlab("Annual Mean Measurement") + ggtitle(unique(cleaned_pollutants_data$parameter)[6]))

#PM10 LC
print(ggplot(cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[7])) +
        geom_line(aes(x = annualmean, y = lifeexpect, group = state, color = division)) +
        ylab("Life Expectancy") + xlab("Annual Mean Measurement") + ggtitle(unique(cleaned_pollutants_data$parameter)[7]))

#PM2.5 LC
print(ggplot(cleaned_pollutants_data %>% filter(parameter == unique(cleaned_pollutants_data$parameter)[8])) +
        geom_line(aes(x = annualmean, y = lifeexpect, group = state, color = division)) +
        ylab("Life Expectancy") + xlab("Annual Mean Measurement") + ggtitle(unique(cleaned_pollutants_data$parameter)[8]))

#remove California PM2.5 high
pm2.5LC = cleaned_pollutants_data%>%filter(parameter == unique(cleaned_pollutants_data$parameter)[8] & annualmean < 43)
print(ggplot(data = pm2.5LC, aes(x = annualmean, y = lifeexpect, 
                                 group = state, color = division)) + geom_line())
