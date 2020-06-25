# A Garch Tutorial with R - R Script for Preparing Computer
# Paper at <link_paper_here>
#
# This script will install all missing dependencies for the R code related to article
# "A Garch Tutorial in R" <link-to-paper>. If this is the first time running a R script, 
# make sure you got the right software: 
#
# 1) [required] Install latest R version <https://www.r-project.org/>
# 2) [optional] Install latest RStudio <https://rstudio.com/products/rstudio/download/>
# 
# Preferably, you should execute all scripts in RStudio. If you use other IDE (or none), 
# make sure to change the path for setwd() in all scripts.
#
# LINUX users: 1) Install additional libraries: libssl-dev, libxml2-dev, libcurl4-openssl-dev
#                 In terminal (control+alt+t): "sudo apt install libssl-dev libxml2-dev libcurl4-openssl-dev"
#              2) Just execute this script to install required libraries
#
# WINDOWS users: 1) Simply execute this script to install required libraries
#
# MAC users: 1) Simply execute this script to install required libraries
# 
# Alternatively, you can execute the code using RStudio Cloud at the following,
# public available, project: https://rstudio.cloud/project/1371589


# list of required packages
required_pkgs <- c('tidyverse', 'ggtext', 'rugarch', 'BatchGetSymbols',
                   'GetBCBData', 'cowplot', 'purrr', 'tidyr',
                   'FinTS', 'scales', 'texreg', 'knitr', 'kableExtra')

# finds installed pkgs from R session
installed_pkgs <- installed.packages()

# find missing packages
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed_pkgs[, 1])]

if (length(missing_pkgs) == 0 ) {
  message("No missing dependencies. You are good to go!")
} else {
  install.packages(missing_pkgs)
  
  message("All packages installed. You are good to go!")
  
}

message("You should now execute the first script: 01-Get_Index_Data.R")


