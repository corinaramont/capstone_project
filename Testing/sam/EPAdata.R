library(RAQSAPI)

#aqs_sign_up(email = "sam_currans@tamu.edu")


aqs_credentials(username = "sam_currans@tamu.edu", key = "russetosprey91 ")


#Gets the State codes in format that can be pasted into API calls
cleanedstatecodes = (paste("'", continentalstates$stateFIPS, "'", sep = "",collapse = ","))

#When making API calls use this in the sta
#Gets the  codes for each state
statecodes = aqs_states()


#Shortens the FIPS data to only the 48 continental states 
continentalstates FIPS= statecodes[1:which(statecodes$state == "Wyoming"),][c(-which(statecodes$state == "Alaska"),-which(statecodes$state == "Hawaii"),-which(statecodes$state == "District Of Columbia") ),]
teFIPS parameter below 
# cat("c(",cleanedstatecodes,")", sep = "")
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


#For checking the county codes for Texas
texascounties = aqs_counties_by_state(stateFIPS = 48)

#Gets the CO data from bexar county Texas, near San Antonio
bexarcounties = aqs_dailysummary_by_county(parameter = "42101",
                                           bdate = as.Date("20220101", format = "%Y%m%d"),
                                           edate = as.Date("20220201", format = "%Y%m%d"),
                                           stateFIPS = "48",
                                           countycode = "029")




firsttwostates = aqs_monitors_by_state(parameter=c("42101","14129"),
                           bdate=as.Date("19800101",
                                         format="%Y%m%d"),
                           edate=as.Date("20220101",
                                         format="%Y%m%d"),
                           stateFIPS=c("01","04")
)

head(TX)


criteria = aqs_parameters_by_class(class = "CRITERIA")
head(criteria)

aqs_annualsummary_by_state()