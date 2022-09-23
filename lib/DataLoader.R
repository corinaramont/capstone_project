
get_data_file <- function(name) { sprintf("datasets/%s.dat", name) }

save_data <- function(data, name) { saveRDS(data, get_data_file(name)) }
load_data <- function(name)       { readRDS(      get_data_file(name)) }

get_abrv     <- function(state_code) { state_info[state_info$FIPS == state_code,]$abrv     }
get_region   <- function(state_code) { state_info[state_info$FIPS == state_code,]$region   }
get_division <- function(state_code) { state_info[state_info$FIPS == state_code,]$division }

get_unit_scale <- function(pollutant, units)
{
  switch(units,
         "Parts per billion" = 1,
         "Parts per million" = 1000,
         NA)
}

extrapolate_data <- function(data)
{
  unit_scales = mapply(get_unit_scale, data$parameter, data$units_of_measure)
  
  cbind(abrv     = sapply(data$state_code, get_abrv),
        region   = sapply(data$state_code, get_region),
        division = sapply(data$state_code, get_division),
        scaled_mean = data$arithmetic_mean * unit_scales)
}

process_data <- function(data)
{
  data = data[,-c(5:8, 14, 23:27, 30:41)]
  exdata = extrapolate_data(data)
  
  cbind(exdata, data)
}

state_info  = load_data("state_info")
cached_data = load_data("cached_data")

#raw_data = load_data("raw_data")

#cached_data = process_data(raw_data)
#save_data(cached_data, "cached_data")
