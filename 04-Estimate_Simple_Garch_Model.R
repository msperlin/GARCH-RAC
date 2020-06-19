# A Garch Tutorial with R - Estimate simple garch model
# Paper at <link_paper_here>
#
# This script will estimate a simple garch model and save estimation results
# in a html file

# OPTIONS
ar_lag <- 0 # lag used for ar term in mean equation (0 in paper)
ma_lag <- 0 # lag used for ma term in mean equation (0 in paper)
arch_lag <- 1 # lag in arch effect (1 in paper)
garch_lag <- 1 # lag in garch effect (1 in paper)
models_to_estimate <- c('sGARCH', 'eGARCH', 'gjrGARCH') # see rugarch manual for more
distribution_to_estimate <- 'snorm' # distribution used in all models
my_html_file <- 'tabs/garch_tab.html' # where to save html file?

# END OPTIONS

library(tidyverse)
library(FinTS)
library(texreg)
library(rugarch)

# close all openned windows
graphics.off()

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# source functions
source('fcts/garch_fcts.R')

# get price data
df_prices <- read_rds('data/RAC-GARCH-Data.rds')

estimate_garch <- function(type_model) {
  
  message('Estimating ', type_model)
  
  # estimate model
  my_spec <- ugarchspec(variance.model = list(model = type_model,
                                              garchOrder = c(arch_lag, 
                                                             garch_lag)),
                        mean.model = list(armaOrder = c(ar_lag,
                                                        ma_lag)))
  
  my_garch <- ugarchfit(spec = my_spec, data = df_prices$log_ret)
  
  return(my_garch)
}

# estimate all models
l_models <- map(.x = models_to_estimate, .f = estimate_garch)

# make sure dir "tabs" exists
if (!dir.exists('tabs')) dir.create('tabs')

# reformat models for texreg
l_models <- map(l_models, extract.rugarch, include.rsquared = FALSE)

# write custom row
custom_row <- list('Variance Model' = models_to_estimate,
                   'Distribution' = rep(distribution_to_estimate, 
                                        length(l_models)))
custom_names <- paste0('Model ', 1:length(l_models))

# save to html
htmlreg(l_models, 
        file = my_html_file, 
        custom.gof.rows = custom_row,
        custom.model.names = custom_names, 
        digits = 3)

# print to screen
screenreg(l_models,
          custom.gof.rows = custom_row,
          custom.model.names = custom_names, 
          digits = 3)


