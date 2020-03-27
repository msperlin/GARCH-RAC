library(tidyverse)
library(FinTS)
library(texreg)
library(fGarch)

graphics.off()

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

source('fcts/garch_fcts.R')

# get price data
df_prices <- read_rds('data/RAC-GARCH-Data.rds')

my_garch <- garchFit(formula = ~arma(0,0)+garch(1,1),
                     data = df_prices$log_ret)

my_garch

texreg::htmlreg(my_garch, file = 'tabs/garch_tab.html', 
                custom.model.names = df_prices$series_name[1], digits = 3)

texreg::screenreg(my_garch, custom.model.names = df_prices$series_name[1], digits = 3)


