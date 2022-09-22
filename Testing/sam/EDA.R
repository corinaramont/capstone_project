
library(dplyr)
library(tidyverse)
library(ggplot2)
library("gridExtra")                        # Load gridExtra package


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
#seeing which missing values we have
which(is.na(all_data$arithmetic_mean))

all_data[527098,]$parameter
all_data[527100,]$parameter

all_data = all_data[-c(527100, 527098),]

# calculates multipliers for each row's mean
multipliers = mapply(normalizer, all_data$parameter, all_data$units_of_measure, USE.NAMES = F)

# normalizes means using multipliers
standardized_means = all_data$arithmetic_mean * multipliers

ttemp = all_data %>% filter(parameter == "Acceptable PM2.5 AQI & Speciation Mass")

unique(ttemp$units_of_measure)

sum(is.na(all_data$arithmetic_mean))

df <- data.frame(cbind(all_data%>%select(state, state_code, county_code, site_number, parameter_code, 
                                         parameter, year, standard_deviation, first_max_value, 
                                         second_max_value, arithmetic_mean), standardized_means))

pm25 = df %>% filter(parameter == "Acceptable PM2.5 AQI & Speciation Mass") %>% group_by(state, parameter, year) %>% summarise(annualmean = mean(arithmetic_mean))

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

sum(is.na(all_data$arithmetic_mean))



pollutantsbyyearandstate = all_data  %>% group_by(year, state, parameter, division) %>%
  summarise(annualmean = mean(arithmetic_mean))

ggplot(pollutantsbyyearandstate %>% filter(parameter == "Carbon monoxide")) +
  geom_line(aes(x = year, y = annualmean, group = state, color = division)) +
  ylab("Annual mean measurement") + 
  ggtitle("Particlate Matter 2.5 micrometers and smaller")

ggplot(pollutantsbyyearandstate %>% filter(parameter == "Carbon monoxide")) +
  geom_line(aes(x = year, y = annualmean, group = state, color = division)) +
  ylab("Annual mean measurement") + 
  ggtitle("Annual Average Carbon Monoxide Measurements per Division")


par(mfrow = c(2,4))

for(i in unique(pollutantsbyyearandstate$parameter)){
  

  print(ggplot(pollutantsbyyearandstate %>% filter(parameter == i)) +
  geom_line(aes(x = year, y = annualmean, group = state, color = division)) +
  ylab("Annual mean measurement") + ggtitle(i))
}





northeast = df%>%filter(state_code %in% northeast_codes)
northeastPM25 = northeast %>% filter(parameter == "Acceptable PM2.5 AQI & Speciation Mass")%>%group_by(year)%>%
  summarise(annualmean=mean(arithmetic_mean))%>%mutate(Region="Northeast")



midwest = df%>%filter(state_code %in% midwest_codes)
midwestPM25 = midwest%>% filter(parameter == "Acceptable PM2.5 AQI & Speciation Mass")%>%group_by(year)%>%
  summarise(annualmean=mean(arithmetic_mean))%>%mutate(Region="Midwest")

south = df%>%filter(state_code %in% south_codes)
southPM25 = south%>% filter(parameter == "Acceptable PM2.5 AQI & Speciation Mass")%>%group_by(year)%>%
  summarise(annualmean=mean(arithmetic_mean))%>%mutate(Region="South")

west = df%>%filter(state_code %in% west_codes)
westPM25 = west%>% filter(parameter == "Acceptable PM2.5 AQI & Speciation Mass")%>%group_by(year)%>%
  summarise(annualmean=mean(arithmetic_mean))%>%mutate(Region="West")

ttemp=west%>% filter(parameter == "Acceptable PM2.5 AQI & Speciation Mass")%>%group_by(year)


pm25annualmeanbyregion = rbind(northeastPM25,midwestPM25, southPM25, westPM25)

ggplot(data = pm25annualmeanbyregion, aes(x = year, y = annualmean, group = Region)) +
  geom_line(aes(color = Region)) + 
  ylab("Annual Average PM2.5 measurements")




ggplot(pm25) +
  geom_line(aes(x = year, y = annualmean, color = state)) +
  ylab("Acceptable PM2.5 AQI & Speciation Mass")


