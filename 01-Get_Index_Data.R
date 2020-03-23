library(BatchGetSymbols)
library(tidyverse)

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

if (!dir.exists('data')) dir.create('data')

set.seed(1)

first_date <- '2000-01-01'
last_date <- '2020-03-20' # set Sys.Date() for current date (it updates)
my_tickers <- c('^BVSP') # Ibovespa ticker (fell free to change to any other)

l_out <- BatchGetSymbols(tickers = my_tickers, 
                         first.date = first_date, 
                         last.date = last_date)

df_prices <- l_out$df.tickers %>%
  select(ref.date, ticker, price.adjusted) %>%
  mutate(ret = log(price.adjusted/dplyr::lag(price.adjusted) ) ) %>%
  na.omit()

rds_out <- 'data/RAC-GARCH-Data.rds'
write_rds(df_prices, rds_out)

