library(BatchGetSymbols)
library(tidyverse)

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

## MAIN OPTIONS

first_date <- '2000-01-01' # first date in sample
last_date <- '2020-03-20' # set Sys.Date() for current date 
my_ticker <- '^BVSP' # Ibovespa ticker (fell free to change to any 
                     # other from YFinance: ^GSCP, ^FTSE, ITSA3.SA
                     # check https://finance.yahoo.com/ for more tickers
series_name <- 'Ibovespa' # Name of index/stock that will show up in all plots

## END OPTION

if (!dir.exists('data')) dir.create('data')

l_out <- BatchGetSymbols(tickers = my_ticker, 
                         first.date = first_date, 
                         last.date = last_date)

df_prices <- l_out$df.tickers %>%
  select(ref.date, ticker, price.adjusted) %>%
  mutate(log_ret = log(price.adjusted/dplyr::lag(price.adjusted) ),
         arim_ret = price.adjusted/dplyr::lag(price.adjusted) - 1,
         series_name = series_name) %>%
  na.omit()

rds_out <- 'data/RAC-GARCH-Data.rds'
write_rds(df_prices, rds_out)

