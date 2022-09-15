library(dplyr)
library(tidyr)
library(ggplot2)

#loads life expectancy data into file
load("~/capstone_project/datasets/life_expectancies_1959_2020.Rda")

#graph for all states life expectancies 1959-2020
ggplot(data=life_expect, aes(x=Year, y=Life_Expectancies, group=State)) + 
  geom_line(aes(color=State)) + geom_point(aes(color=State)) + 
  ggtitle("US States Life Expectancies 1959-2020") + ylab("Life Expectancies (Years)")

#census region 1 - northeast "CO", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"
region1 = life_expect%>%filter(State %in% c("CT", "ME", "MA", "NH", 
                                            "RI", "VT", "NJ", "NY", "PA"))
region1_life_expect = region1%>%group_by(Year)%>%
  summarise(avg_by_year=mean(Life_Expectancies))%>%mutate(Region="Northeast")

#census region 2 - midwest
region2 = life_expect%>%filter(State %in% c("IL", "IN", "MI",
                                            "OH", "WI", "IA", "KS", "MN",
                                            "MO", "NE", "ND", "SD"))
region2_life_expect = region2%>%group_by(Year)%>%
  summarise(avg_by_year=mean(Life_Expectancies))%>%mutate(Region="Midwest")

#census region 3 - south
region3 = life_expect%>%filter(State %in% c("DE","FL","GA","MD","NC",
                                            "SC","VA", "WV","AL","KY",
                                            "MS","TN","AR","LA","OK","TX"))
region3_life_expect = region3%>%group_by(Year)%>%
  summarise(avg_by_year=mean(Life_Expectancies))%>%mutate(Region="South")

#census region 4 - west
region4 = life_expect%>%filter(State %in% c("AZ","CO","ID","MT","NV","NM",
                                            "UT","WY","AK","CA","HI","OR","WA"))

region4_life_expect = region4%>%group_by(Year)%>%
  summarise(avg_by_year=mean(Life_Expectancies))%>%mutate(Region="West")

#new data frame for US state life expectancies by region
regions_life_expect = rbind(region1_life_expect,region2_life_expect,
                            region3_life_expect,region4_life_expect)

#graph for life expectancies by region
ggplot(data=regions_life_expect, aes(x=Year, y=avg_by_year, group=Region)) + 
  geom_line(aes(color=Region)) + geom_point(aes(color=Region)) + 
  ggtitle("US Regions Life Expectancies 1959-2020") + ylab("Life Expectancies (Years)")

#boxplot for life expectancies by region
ggplot(data=regions_life_expect, aes(x=Region, y=avg_by_year, group=Region)) + 
  geom_boxplot(aes(fill=Region)) +
  ggtitle("US Regions Life Expectancies 1959-2020") + ylab("Life Expectancies (Years)")
