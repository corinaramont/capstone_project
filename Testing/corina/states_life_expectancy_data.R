library(dplyr)
library(tidyr)

#reading in life expectancy data
life1 = read.csv("~/capstone_project/datasets/life-expectancy-1959-1978.csv")
life2 = read.csv("~/capstone_project/datasets/life-expectancy-1979-1998.csv")
life3 = read.csv("~/capstone_project/datasets/life-expectancy-1999-2016.csv")
life4 = read.csv("~/capstone_project/datasets/cdc-life-expectancy-2018-2020.csv")

life4 = life4[,-4]
life4 = life4%>%pivot_wider(names_from=YEAR, values_from=RATE)%>%arrange(STATE)

#combines all the life expectancy data into one dataframe life_expect
life_expect = cbind(life1,life2[,-1],life3[,-1],life4[,4],life4[,3],life4[,2])

#clean column names
colnames(life_expect) = c("State",1959:2016,2018:2020)

#questions - are we removing particular states? what do we do about 2017?
