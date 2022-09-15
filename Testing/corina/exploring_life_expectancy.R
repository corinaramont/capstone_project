library(dplyr)
library(tidyr)
library(ggplot2)

#loads life expectancy data into file
load("~/capstone_project/datasets/life_expectancies_1959_2020.Rda")

ggplot(data=life_expect, aes(x=Year, y=Life_Expectancies, group=State)) + 
  geom_line(aes(color=State)) + geom_point(aes(color=State)) + 
  ggtitle("US States Life Expectancies 1959-2020") + ylab("Life Expectancies (Years)")

#region 1 - northeast "CO", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"
region1 = life_expect%>%filter(State %in% c("CT", "ME", "MA", "NH", 
                                            "RI", "VT", "NJ", "NY", "PA"))
region1_life_expect = life_expect%>%group_by(Year)%>%
  summarise(avg_by_year=mean(Life_Expectancies))

#region 2 - midwest
region2 = life_expect%>%filter(State %in% c("IL", "IN", "MI",
                                            "OH", "WI", "IA", "KS", "MN",
                                            "MO", "NE", "ND", "SD"))
region2_life_expect = life_expect%>%group_by(Year)%>%
  summarise(avg_by_year=mean(Life_Expectancies))

#region 3 - south
region3 = life_expect%>%filter(State %in% c("DE","FL","GA","MD","NC",
                                            "SC","VA", "WV","AL","KY",
                                            "MS","TN","AR","LA","OK","TX"))
region3_life_expect = life_expect%>%group_by(Year)%>%
  summarise(avg_by_year=mean(Life_Expectancies))

#region 4 - west
region4 = life_expect%>%filter(State %in% c("AZ","CO","ID","MT","NV","NM",
                                            "UT","WY","AK","CA","HI","OR","WA"))

region4_life_expect = life_expect%>%group_by(Year)%>%
  summarise(avg_by_year=mean(Life_Expectancies))

#new data frame for US state life expectancies by region
