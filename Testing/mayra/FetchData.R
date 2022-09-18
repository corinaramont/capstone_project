library(RAQSAPI)
library(sys)

#aqs_sign_up(email = "mhdz@tamu.edu")

aqs_credentials(username = "mhdz@tamu.edu", key = "berrywolf85")

statecodes = aqs_states()[1:51,][c(-2, -9, -12),]

params = c(
  "14129", #Lead
  "42101", #Carbon Monoxide
  "42401", #Sulfur Dioxide
  "42602"  #Nitrogen Dioxide
  #"????", #PM10
  #"????"  #PM2.5
)

get_date <- function(year, month = 1, day = 1)
{
  as.Date(sprintf("%i-%i-%i", month, day, year), "%m-%d-%Y")
}

query_year_toxic <- function(param, state, year)
{
  bdate = get_date(year,  1,  1)
  edate = get_date(year, 12, 31)
  
  aqs_annualsummary_by_state(param, bdate, edate, state)
}

query_years_toxic <- function(param, state, years)
{
  data = NULL
  for (year in years) {
    data = rbind(data, query_year_toxic(param, state, year))
  }
  return(data)
}

get_cache_name <- function(param, state)
{
  sprintf("datasets/%s-%s.RData", state, param)
}

cache_data <- function(param, state, years = 1980:2019)
{
  file = get_cache_name(param, state)
  data = query_years_toxic(param, state, years)
  
  saveRDS(data, file)
}

cache_all_data <- function(param, years = 1980:2019)
{
  print("Beginning cache, this will take some time...")
  for (state in statecodes$stateFIPS)
  {
    cache_data(cur_param, state)
  }
  print("Caching finished.")
}

load_cached_data <- function(param, state)
{
  readRDS(get_cache_name(param, state))
}

# PLEASE RUN THE FUNCTION BELOW TO DOWNLOAD AND CACHE THE DATA. 
# EACH FILE IS FOR ONE STATE, ONE PARAMETER, FROM 1980 TO 2019. 
# FILES ARE SAVED AS '.RData' BUT WILL SEEM CORRUPTED WHEN THEY ARE NOT.
# EACH FUNCTION CALL WILL TAKE A VERY LONG TIME 

#EX:
#cache_all_data(param$CO)
#         OR
#cache_all_data("42401")


