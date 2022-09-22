library(RAQSAPI)

#aqs_sign_up(email = "sam_currans@tamu.edu")


aqs_credentials(username = "sam_currans@tamu.edu", key = "russetosprey91 ")



#When making API calls use this in the sta
#Gets the  codes for each state
statecodes = aqs_states()


#Shortens the FIPS data to only the 48 continental states 
continental= statecodes[1:which(statecodes$state == "Wyoming"),][c(-which(statecodes$state == "Alaska"),-which(statecodes$state == "Hawaii"),-which(statecodes$state == "District Of Columbia") ),]
#teFIPS parameter below 
# cat("c(",continental$statesFIPS,")", sep = "")
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#This is a list of the state codes for the 48 states for API calls
statecodes48 = c("01", "04", "05", "06", "08", "09", "10", "12", "13", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27",
"28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44","45", "46", "47", "48", "49",
"50", "51", "53", "54", "55", "56")

pollutatntcodes = c("42101", "12128","14129", "42602", "44201", "81102", "85101", "88502", "88101", "42401")


#For checking the county codes for Texas
texascounties = aqs_counties_by_state(stateFIPS = 48)

#Gets the CO data from bexar county Texas, near San Antonio
bexarcounties = aqs_dailysummary_by_county(parameter = "42101",
                                           bdate = as.Date("20220101", format = "%Y%m%d"),
                                           edate = as.Date("20220201", format = "%Y%m%d"),
                                           stateFIPS = "48",
                                           countycode = "029")
colnames(bexarcounties)

#Gets data from 1980-2020 from all STATES in the CO parameter
all1995CO = aqs_annualsummary_by_state(parameter = "42101",
                           bdate = as.Date("19800515", format="%Y%m%d"),
                           edate = as.Date("19800515", format = "%Y%m%d"),
                           stateFIPS = statecodes48)



#gets all the pollutants for all the states in 1980
all1980pollutants = aqs_annualsummary_by_state(parameter = pollutatntcodes,
                                       bdate = as.Date("19800515", format="%Y%m%d"),
                                       edate = as.Date("19800515", format = "%Y%m%d"),
                                       stateFIPS = "01")
saveRDS(all1980pollutants, "all1980pollutants.rds")

#gets all the pollutants for all the states in 1981
all1981pollutants = aqs_annualsummary_by_state(parameter = pollutatntcodes,
                                               bdate = as.Date("19810515", format="%Y%m%d"),
                                               edate = as.Date("19810515", format = "%Y%m%d"),
                                               stateFIPS = statecodes48)
saveRDS(all1981pollutants, "all1981pollutants.rds")


a = "19810515"

paste0(a,a[1:4])

#loops to call the api to get the data for all pollutants for all states for a single year
#initializing at year 1980
year = 19800515

#40 loops gets us up to year 2020
for (i in 0:40) {

#creates the variable names for each year of data 
nam = paste("pollutantdata",substr(year,1,4),sep="" )  

#gives each new variable created the value of the pollutant data for that year
assign(nam, aqs_annualsummary_by_state(parameter = pollutatntcodes,
                                       bdate = as.Date(paste(year), format="%Y%m%d"),
                                       edate = as.Date(paste(year), format = "%Y%m%d"),
                                       stateFIPS = statecodes48))

#temp var that gives the name for the file to be saved
temp = paste("pollutantdata",substr(year,1,4),".rds",sep="" )

#saves the data so that we dont have to rely on the api calls 
saveRDS(nam, temp)

unique(pollutantdata2002$state)

#chekcs to make sure that the loops is working
print(nam)
#this makes it so that we loop by a single year 
year = year + 10000
}


######################################
########## 2003-2020 API CALLS########
######################################

#loops to call the api to get the data for all pollutants for all states for a single year
#initializing at year 1980
year = 20030515

#18 loops gets us up to year 2020
for (i in 0:17) {
  
  #creates the variable names for each year of data 
  nam = paste("pollutantdata",substr(year,1,4),sep="" )  
  
  #gives each new variable created the value of the pollutant data for that year
  assign(nam, aqs_annualsummary_by_state(parameter = pollutatntcodes,
                                         bdate = as.Date(paste(year), format="%Y%m%d"),
                                         edate = as.Date(paste(year), format = "%Y%m%d"),
                                         stateFIPS = statecodes48))
  
  #temp var that gives the name for the file to be saved
  temp = paste("pollutantdata",substr(year,1,4),".rds",sep="" )
  
  #saves the data so that we dont have to rely on the api calls 
  saveRDS(nam, temp)
  
  #chekcs to make sure that the loops is working
  print(nam)
  #this makes it so that we loop by a single year 
  year = year + 10000
}
#forgive me for this 
saveRDS(pollutantdata1980, "pollutantdata1980.rds")
saveRDS(pollutantdata1981, "pollutantdata1981.rds")
saveRDS(pollutantdata1982, "pollutantdata1982.rds")
saveRDS(pollutantdata1983, "pollutantdata1983.rds")
saveRDS(pollutantdata1984, "pollutantdata1984.rds")
saveRDS(pollutantdata1985, "pollutantdata1985.rds")
saveRDS(pollutantdata1986, "pollutantdata1986.rds")
saveRDS(pollutantdata1987, "pollutantdata1987.rds")
saveRDS(pollutantdata1988, "pollutantdata1988.rds")
saveRDS(pollutantdata1989, "pollutantdata1989.rds")

saveRDS(pollutantdata1990, "pollutantdata1990.rds")
saveRDS(pollutantdata1991, "pollutantdata1991.rds")
saveRDS(pollutantdata1992, "pollutantdata1992.rds")
saveRDS(pollutantdata1993, "pollutantdata1993.rds")
saveRDS(pollutantdata1994, "pollutantdata1994.rds")
saveRDS(pollutantdata1995, "pollutantdata1995.rds")
saveRDS(pollutantdata1996, "pollutantdata1996.rds")
saveRDS(pollutantdata1997, "pollutantdata1997.rds")
saveRDS(pollutantdata1998, "pollutantdata1998.rds")
saveRDS(pollutantdata1999, "pollutantdata1999.rds")

saveRDS(pollutantdata2000, "pollutantdata2000.rds")
saveRDS(pollutantdata2001, "pollutantdata2001.rds")
saveRDS(pollutantdata2002, "pollutantdata2002.rds")
saveRDS(pollutantdata2003, "pollutantdata2003.rds")
saveRDS(pollutantdata2004, "pollutantdata2004.rds")
saveRDS(pollutantdata2005, "pollutantdata2005.rds")
saveRDS(pollutantdata2006, "pollutantdata2006.rds")
saveRDS(pollutantdata2007, "pollutantdata2007.rds")
saveRDS(pollutantdata2008, "pollutantdata2008.rds")
saveRDS(pollutantdata2009, "pollutantdata2009.rds")

saveRDS(pollutantdata2010, "pollutantdata2010.rds")
saveRDS(pollutantdata2011, "pollutantdata2011.rds")
saveRDS(pollutantdata2012, "pollutantdata2012.rds")
saveRDS(pollutantdata2013, "pollutantdata2013.rds")
saveRDS(pollutantdata2014, "pollutantdata2014.rds")
saveRDS(pollutantdata2015, "pollutantdata2015.rds")
saveRDS(pollutantdata2016, "pollutantdata2016.rds")
saveRDS(pollutantdata2017, "pollutantdata2017.rds")
saveRDS(pollutantdata2018, "pollutantdata2018.rds")
saveRDS(pollutantdata2019, "pollutantdata2019.rds")

saveRDS(pollutantdata2020, "pollutantdata2020.rds")





pollutantdata1980$parameter

temp = paste("pollutantdata",substr(year,1,4),".rds",sep="" )
  

saveRDS(nam, temp)




a[1]


#Verifies that the function works
colnames(all1995CO)
dim(all1995CO)
all1995CO$year

all1995CO$state

#gets all 48 states CO data from 1995 to 
startcall = Sys.time()

airdata1995 = aqs_annualsummary_by_state(parameter = pollutatntcodes,
                           bdate = as.Date("19950515", format="%Y%m%d"),
                           edate = as.Date("19950515", format = "%Y%m%d"),
                           stateFIPS = statecodes48)
endcall = Sys.time()

elapsed = endcall-startcall

#Verifies that the function worked
allstateCO1995$state
allstateCO1995$parameter


unique(allstateCO1995$state)

unique(allstateCO1995$parameter)

temp = aqs_monitors_by_state(parameter="42101",
                      bdate=as.Date("20170101",
                                    format="%Y%m%d"),
                      edate=as.Date("20171231",
                                    format="%Y%m%d"),
                      stateFIPS=" "
)

colnames(temp)

temp$csa_name

criteria = aqs_parameters_by_class(class = "CRITERIA")

aqs_annualsummary_by_state()