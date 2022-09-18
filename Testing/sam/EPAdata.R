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
                           edate = as.Date("20200515", format = "%Y%m%d"),
                           stateFIPS = "37")

#Verifies that the function works
colnames(all1995CO)
dim(all1995CO)
all1995CO$year

#gets all 48 states CO data from 1995 to 
allstateCO1995 = aqs_annualsummary_by_state(parameter = "42101",
                           bdate = as.Date("19950515", format="%Y%m%d"),
                           edate = as.Date("19950515", format = "%Y%m%d"),
                           stateFIPS = statecodes48)

#Verifies that the function worked
allstateCO1995$state


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