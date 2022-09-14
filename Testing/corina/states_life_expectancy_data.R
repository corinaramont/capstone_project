#reading in life expectancy data
life1 = read.csv("~/capstone_project/datasets/life-expectancy-1959-1978.csv")
life2 = read.csv("~/capstone_project/datasets/life-expectancy-1979-1998.csv")
life3 = read.csv("~/capstone_project/datasets/life-expectancy-1999-2016.csv")

#combines all the life expectancy data into one dataframe life_expect
life_expect = cbind(life1,life2[,-1],life3[,-1])