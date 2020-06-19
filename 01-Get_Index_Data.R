# A Garch Tutorial with R - Get Index data
# Paper at <link_paper_here>
#
# This script will import price data for market index Ibovespa from Yahoo Finance.
# 
# The resulting dataset is serialized (saved) in a rds file named data/RAC-GARCH-Data.rds,
# to be used in the next step.

## MAIN OPTIONS (fell free to edit it)

first_date <- '2000-01-01' # first date in sample ("2000-01-01" in paper)
last_date <- '2020-06-01' # set Sys.Date() for current date ("2020-06-01" in paper)
my_ticker <- '^BVSP' # Ibovespa ticker (fell free to change to any 
                     # other from YFinance: ^GSCP, ^FTSE, ITSA3.SA
                     # head over to https://finance.yahoo.com/ for more tickers
series_name <- 'Ibovespa' # Name of index/stock that will show up in all plots

## END OPTIONS

# load required libraries
library(BatchGetSymbols)
library(tidyverse)

# change directory to where the script located
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# makes sure the directory "data" exists
if (!dir.exists('data')) dir.create('data')

# download price data for "my_ticker"
l_out <- BatchGetSymbols(tickers = my_ticker, 
                         first.date = first_date, 
                         last.date = last_date)

# select columns and calculated log_ret and arim_ret
df_prices <- l_out$df.tickers %>%
  select(ref.date, ticker, price.adjusted) %>%
  mutate(log_ret = log(price.adjusted/dplyr::lag(price.adjusted) ),
         arim_ret = price.adjusted/dplyr::lag(price.adjusted) - 1,
         series_name = series_name) %>%
  na.omit() # remove all NA values

# save data into file
rds_out <- 'data/RAC-GARCH-Data.rds'
write_rds(df_prices, rds_out)

