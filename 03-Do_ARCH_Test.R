library(tidyverse)
library(writexl)

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

source('fcts/garch_fcts.R')

## MAIN OPTIONS

max_lag <- 5
xlsx_file <- 'tabs/Arch-test.xlsx'

## END OPTIONS

# get price data
df_prices <- read_rds('data/RAC-GARCH-Data.rds')

tab_out <- do_arch_test(x = df_prices$log_ret, max_lag = max_lag)

tab_out

write_xlsx(x = tab_out, path = xlsx_file)