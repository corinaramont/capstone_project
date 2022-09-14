library(dplyr)
library(tidyr)
library(tibble)

#reading in life expectancy data
life1 = read.csv("~/capstone_project/datasets/life-expectancy-1959-1978.csv")
life2 = read.csv("~/capstone_project/datasets/life-expectancy-1979-1998.csv")
life3 = read.csv("~/capstone_project/datasets/life-expectancy-1999-2016.csv")
life4 = read.csv("~/capstone_project/datasets/cdc-life-expectancy-2018-2020.csv")

life4 = life4[,-4]
life4 = life4%>%pivot_wider(names_from=YEAR, values_from=RATE)%>%arrange(STATE)

#combines all the life expectancy data into one dataframe life_expect
life_expect = cbind(life1,life2[,-1],life3[,-1],c(rep(0,50)),life4[,4],life4[,3],life4[,2])

#clean column names
year_names = c()
for(i in 1959:2020){
  year_names = c(year_names,paste("year_",toString(i),sep=""))
}
colnames(life_expect) = c("State",year_names)

#check for existing missing values (before we add 2017)
table(is.na(life_expect))

#missing column for 2017 - add values to it
mean_state_life_expect = round(rowMeans(life_expect[,c(-1,-60)]),digits=1)
life_expect$year_2017 = mean_state_life_expect

#saves life_expect dataframe to an Rda file
save(life_expect,file="~/capstone_project/datasets/life_expectancies_1959_2020.Rda")
