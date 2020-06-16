# A Garch Tutorial with R - Perform ARCH test in series of returns
# Paper at <link_paper_here>
#
# This script will test for arch effect and save results in excel file

## OPTIONS

max_lag <- 5
xlsx_file <- 'tabs/Arch-test.xlsx'

## END OPTIONS

library(tidyverse)
library(writexl)

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

source('fcts/garch_fcts.R')

# get price data
df_prices <- read_rds('data/RAC-GARCH-Data.rds')

# do arch test
tab_out <- do_arch_test(x = df_prices$log_ret, max_lag = max_lag)

tab_out

# save data in xlsx
write_xlsx(x = tab_out, path = xlsx_file)