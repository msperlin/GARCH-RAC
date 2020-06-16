# A Garch Tutorial with R <link_paper_here>
#
# This script will estimate a simple garch model and save estimation results
# in a html file

# OPTIONS
my_html_file <- 'tabs/garch_tab.html' # where to save html file?

# END OPTIONS

library(tidyverse)
library(FinTS)
library(texreg)
library(fGarch)

# close all openned windows
graphics.off()

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# source functions
source('fcts/garch_fcts.R')

# get price data
df_prices <- read_rds('data/RAC-GARCH-Data.rds')

# estimate a ARMA(1,1)~GARCH(1,1) model
my_garch <- garchFit(formula = '~arma(0,0)+garch(1,1)',
                     data = df_prices$log_ret)

my_garch

# make sure dir "tabs" exists
if (!dir.exists('tabs')) dir.create('tabs')

# save to html
htmlreg(my_garch, file = my_html_file, 
                custom.model.names = df_prices$series_name[1], digits = 3)

# print to screen
screenreg(my_garch, custom.model.names = df_prices$series_name[1], digits = 3)


