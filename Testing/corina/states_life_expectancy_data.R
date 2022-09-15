library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

#reading in life expectancy data
life1 = read.csv("~/capstone_project/datasets/life-expectancy-1959-1978.csv")
life2 = read.csv("~/capstone_project/datasets/life-expectancy-1979-1998.csv")
life3 = read.csv("~/capstone_project/datasets/life-expectancy-1999-2016.csv")
life4 = read.csv("~/capstone_project/datasets/cdc-life-expectancy-2018-2020.csv")

life4 = life4[,-4]
life4 = life4 %>% pivot_wider(names_from = YEAR, 
                              values_from = RATE) %>% arrange(STATE)

#combines all the life expectancy data into one dataframe life_expect
life_expect = cbind(life1, life2[,-1], life3[,-1], life4[,4], life4[,3], life4[,2])
life_expect[,1] = str_trim(life_expect[,1], side="right")
life_expect = life_expect[,-62]

#clean column names
year_names = c()
for(i in c(1959:2016,2018,2019)){
  year_names = c(year_names,paste("year_", toString(i), sep=""))
}
colnames(life_expect) = c("State", year_names)

#check for existing missing values (before we add 2017)
table(is.na(life_expect)) #there are no other missing values

life_expect = life_expect %>% 
  pivot_longer(cols = c(2:61), names_to = "Year", values_to = "Life_Expectancies")
life_expect$Year = rep(c(1959:2016,2018,2020), 50)
life_expect = life_expect%>%filter(!State %in% c("AK", "HI"))

#saves life_expect dataframe to an Rda file
save(life_expect,file="~/capstone_project/datasets/life_expectancies_1959_2020.Rda")
