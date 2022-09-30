
load_data <- function() { readRDS("datasets/all_data.dat") }

# EXAMPLE #

# data = load_data()

# that's it! (:)










# MISSING DATA : RUN THIS AND LOOK AT "missing_entries..."
library(dplyr)

cached_data = load_data()

df_table <- function(...) { data.frame(table(...)) }

find_missing_entries <- function(data)
{
  df = df_table(data$parameter, data$state, data$year) %>% filter(Freq == 0)
  df = cbind(df[,1:2], as.numeric(as.character(df[,3])))
  names(df) = c("parameter", "state", "year")
  df
}

missing_entries = find_missing_entries(cached_data)

# missing data 2000-present
missing_entries2k <- missing_entries[yrs >= 2000,]
missing_entries2k_ct <- df_table(missing_entries2k$parameter, missing_entries2k$year) %>% filter(Freq > 0)

# missing data PM2.5 1980-1999
missing_entriesPM25 <- missing_entries[yrs < 2000,] %>% filter(grepl('PM2.5', as.character(parameter), fixed = TRUE))
missing_entriesPM25_ct <- df_table(missing_entriesPM25$parameter, missing_entriesPM25$year) %>% filter(Freq > 0)

