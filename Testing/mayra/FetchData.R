library(RAQSAPI)

#aqs_sign_up(email = "mhdz@tamu.edu")

aqs_credentials(username = "mhdz@tamu.edu", key = "berrywolf85")

states = aqs_states()[1:51,][c(-2, -9, -12),]
stateFIPS = states$stateFIPS

cur_params = c(
# "14129", #Lead
  "42101", #Carbon Monoxide
  "42401", #Sulfur Dioxide
  "42602", #Nitrogen Dioxide
  "44201", #Ozone
  "81102", #PM10
  "85101", #PM10
  "88101", #PM2.5
  "88502"  #PM2.5
)

printf <- function(fmt, ...)
{
  print(sprintf(fmt, ...))
}

get_date <- function(year, month = 1, day = 1)
{
  as.Date(sprintf("%i-%i-%i", month, day, year), "%m-%d-%Y")
}

query_year <- function(params, states, year)
{
  date = get_date(year)
  
  aqs_annualsummary_by_state(params, date, date, states)
}

#query_years_toxic <- function(param, state, years)
#{
#  data = NULL
#  for (year in years) {
#    data = rbind(data, query_year_toxic(param, state, year))
#  }
#  return(data)
#}

get_cache_name_old <- function(param, state)
{
  sprintf("datasets/%s-%s.RData", state, param)
}

get_cache_name <- function(year)
{
  sprintf("datasets/%i.dat", year)
}

cache_year <- function(year, params, states)
{
  saveRDS(query_year(params, states, year), get_cache_name(year))
}

for (year in 1980:2019)
{
  cache_year(year, cur_params, stateFIPS)
  printf("year %i data cached", year)
}

