library(RAQSAPI)

#aqs_sign_up(email = "sam_currans@tamu.edu")

aqs_credentials(username = "sam_currans@tamu.edu", key = "russetosprey91 ")



statecodes = aqs_states()

texascounties = aqs_counties_by_state(stateFIPS = 48)


bexarcounties = aqs_dailysummary_by_county(parameter = "42101",
                                           bdate = as.Date("20220101", format = "%Y%m%d"),
                                           edate = as.Date("20220201", format = "%Y%m%d"),
                                           stateFIPS = "48",
                                           countycode = "29")


sleep(5)
temp = aqs_dailysummary_by_county(parameter = "42101",
                                  bdate = as.Date("20160101", format = "%Y%m%d"),
                                  edate = as.Date("20170228", format = "%Y%m%d"),
                                  stateFIPS = "48",
                                  countycode = "029")
head(temp)

HA = aqs_monitors_by_state(parameter="42101",
                           bdate=as.Date("20170101",
                                         format="%Y%m%d"),
                           edate=as.Date("20171231",
                                         format="%Y%m%d"),
                           stateFIPS="02"
)

colnames(HA)
dim(HA)


TX = aqs_monitors_by_state(parameter=c("14129","42101"),
                           bdate=as.Date("19800101",
                                         format="%Y%m%d"),
                           edate=as.Date("20220101",
                                         format="%Y%m%d"),
                           stateFIPS=c("48","01")
)

head(TX)


criteria = aqs_parameters_by_class(class = "CRITERIA")
head(criteria)

aqs_annualsummary_by_state()