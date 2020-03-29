# This script will install all missing dependencies 
#
# LINUX users: Install additional libraries: libssl-dev, libxml2-dev, libcurl4-openssl-dev
#              In terminal: "sudo apt install libssl-dev libxml2-dev libcurl4-openssl-dev"

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


