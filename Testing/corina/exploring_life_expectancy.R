library(dplyr)
library(tidyr)
library(ggplot2)

#loads life expectancy data into file
load("~/capstone_project/datasets/life_expectancies_1959_2019.Rda")

################ DATA ORGANIZING & FILTERING ###################################

#census region 1 - northeast "CO", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"
region1 = life_expect%>%filter(State %in% c("CT", "ME", "MA", "NH", 
                                            "RI", "VT", "NJ", "NY", "PA"))
region1_life_expect = region1%>%group_by(Year)%>%
  summarise(avg_by_year=mean(Life_Expectancies))%>%mutate(Region="Northeast")

#(region 1) division 1 - new england
division1 = life_expect%>%filter(State %in% c("CO", "ME", "MA", "NH", "RI", "VT"))
division1_life_expect = division1%>%group_by(Year)%>%mutate(Division="New England")

#(region 1) division 2 - mid-atlantic
division2 = life_expect%>%filter(State %in% c("NJ", "NY", "PA"))
division2_life_expect = division2%>%group_by(Year)%>%mutate(Division="Mid-Atlantic")



#census region 2 - midwest
region2 = life_expect%>%filter(State %in% c("IL", "IN", "MI",
                                            "OH", "WI", "IA", "KS", "MN",
                                            "MO", "NE", "ND", "SD"))
region2_life_expect = region2%>%group_by(Year)%>%mutate(Region="Midwest")

#(region 2) division 3 - east north central
division3 = life_expect%>%filter(State %in% c("IL", "IN", "MI",
                                              "OH", "WI"))
division3_life_expect = division3%>%group_by(Year)%>%mutate(Division="East North Central")

#(region 2) division 4 - west north central
division4 = life_expect%>%filter(State %in% c("IA", "KS", "MN",
                                              "MO", "NE", "ND", "SD"))
division4_life_expect = division4%>%group_by(Year)%>%mutate(Division="West North Central")



#census region 3 - south
region3 = life_expect%>%filter(State %in% c("DE","FL","GA","MD","NC",
                                            "SC","VA", "WV","AL","KY",
                                            "MS","TN","AR","LA","OK","TX"))
region3_life_expect = region3%>%group_by(Year)%>%mutate(Region="South")

#(region 3) division 5 - south atlantic
division5 = life_expect%>%filter(State %in% c("DE","FL","GA","MD","NC",
                                              "SC","VA","WV"))
division5_life_expect = division5%>%group_by(Year)%>%mutate(Division="South Atlantic")

#(region 3) division 6 - east south central
division6 = life_expect%>%filter(State %in% c("AL","KY", "MS","TN"))
division6_life_expect = division6%>%group_by(Year)%>%mutate(Division="East South Central")

#(region 3) division 7 - west south central
division7 = life_expect%>%filter(State %in% c("AR","LA","OK","TX"))
division7_life_expect = division7%>%group_by(Year)%>%mutate(Division="West South Central")

#census region 4 - west
region4 = life_expect%>%filter(State %in% c("AZ","CO","ID","MT","NV","NM",
                                            "UT","WY","CA","OR","WA"))

region4_life_expect = region4%>%group_by(Year)%>%mutate(Region="West")

#(region 4) division 8 - mountain
division8 = life_expect%>%filter(State %in% c("AZ","CO","ID","MT","NV","NM",
                                            "UT","WY"))
division8_life_expect = division8%>%group_by(Year)%>%mutate(Division="Mountain")

#(region 4) division 9 - pacific
division9 = life_expect%>%filter(State %in% c("CA","OR","WA"))
division9_life_expect = division9%>%group_by(Year)%>%mutate(Division="Pacific")


##################### FINAL DATA FRAMES ########################################

#new data frame for US state life expectancies by region
regions_life_expect = rbind(region1_life_expect,region2_life_expect,
                            region3_life_expect,region4_life_expect)

#new data frame for US state life expectancies by division
divisions_life_expect = rbind(division1_life_expect, division2_life_expect, 
                              division3_life_expect, division4_life_expect, 
                              division5_life_expect, division6_life_expect,
                              division7_life_expect, division8_life_expect, 
                              division9_life_expect)


##################### ALL GRAPHS ##############################################

#graph for all states life expectancies 1959-2019
states_graph = ggplot(data=divisions_life_expect, aes(x=Year, y=Life_Expectancies, group=State)) + 
  geom_line(aes(color=Division)) + geom_point(aes(color=Division)) + 
  ggtitle("US States Life Expectancies 1959-2019") + ylab("Life Expectancy (Years)")
print(states_graph)


#graph for life expectancies by region
regions_graph = ggplot(data=regions_life_expect, aes(x=Year, y=avg_by_year, group=Region)) + 
  geom_line(aes(color=Region)) + geom_point(aes(color=Region)) + 
  ggtitle("US Regions Life Expectancies 1959-2019") + ylab("Life Expectancy (Years)")
print(regions_graph)


#boxplot for life expectancies by region
regions_box = ggplot(data=regions_life_expect, aes(x=Region, y=avg_by_year, group=Region)) + 
  geom_boxplot(aes(fill=Region)) +
  ggtitle("US Regions Life Expectancies 1959-2019") + ylab("Life Expectancy (Years)")
print(regions_box)

#graph for life expectancies by division
divisions_graph = ggplot(data=divisions_life_expect, aes(x=Year, y=avg_by_year, group=Division)) + 
  geom_line(aes(color=Division)) + geom_point(aes(color=Division)) + 
  ggtitle("US Divisions Life Expectancies 1959-2019") + ylab("Life Expectancy (Years)")
print(divisions_graph)

#boxplot for life expectancies by division
divisions_box = ggplot(data=divisions_life_expect, aes(x=Division, y=avg_by_year, group=Division)) + 
  geom_boxplot(aes(fill=Division)) +
  ggtitle("US Divisions Life Expectancies 1959-2019") + ylab("Life Expectancy (Years)") +
  theme(axis.text.x = element_text(size = 5.4)) 
print(divisions_box)

##################### TESTING FOR STUFF ##############################################

#tukey's test for the divisions (which mean is different from the rest?)
divisions_aov = aov(avg_by_year~Division, data=divisions_life_expect)
divisions_Tukey = TukeyHSD(divisions_aov)

#tukey's test for regions
regions_aov = aov(avg_by_year~Region, data=regions_life_expect)
regions_Tukey = TukeyHSD(regions_aov)
