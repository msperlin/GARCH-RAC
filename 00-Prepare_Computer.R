# This script will install all missing dependencies 

required_pkgs <- c('tidyverse', 'fGarch', 'BatchGetSymbols',
                   'GetBCBData', 'cowplot', 'purrr', 'GetBCBData',
                   'FinTS', 'scales')

installed_pkgs <- installed.packages()

missing_pkgs <- required_pkgs[!(required_pkgs %in% installed_pkgs[, 1])]

if (length(missing_pkgs) == 0 ) {
  message("No missing dependencies. You are good to go!")
} else {
  install.packages(missing_pkgs)
  
  message("All packages installed. You are good to go!")
}

