library(tidyverse)

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

source('fcts/garch_fcts.R')

max_lag <- 5

# get price data
df_ibov <- read_rds('data/RAC-GARCH-Data.rds')

tab_out <- do_arch_test(x = df_ibov$ret, max_lag = max_lag)

tab_out
