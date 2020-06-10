# This script will install all missing dependencies for the R code related to 
# "A Garch Tutorial in R"
#
# LINUX users: 1) Install additional libraries: libssl-dev, libxml2-dev, libcurl4-openssl-dev
#                 In terminal (control+alt+t): "sudo apt install libssl-dev libxml2-dev libcurl4-openssl-dev"
#              2) Run this script
#
# WINDOWS users: 1) Run this script
#
# MAC users: 1) ????
#

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

required_pkgs <- c('tidyverse', 'fGarch', 'BatchGetSymbols',
                   'GetBCBData', 'cowplot', 'purrr', 'tidyr',
                   'FinTS', 'scales', 'texreg', 'writexl')

installed_pkgs <- installed.packages()

missing_pkgs <- required_pkgs[!(required_pkgs %in% installed_pkgs[, 1])]

if (length(missing_pkgs) == 0 ) {
  message("No missing dependencies. You are good to go!")
} else {
  install.packages(missing_pkgs)
  
  message("All packages installed. You are good to go!")
}


