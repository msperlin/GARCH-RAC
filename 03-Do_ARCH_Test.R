# A Garch Tutorial with R - Perform ARCH test in series of returns
# Paper at <https://rac.anpad.org.br/index.php/rac/article/view/1420>
#
# This script will test for arch effects for a given vector of returns, given
# lags and save results in .html file.

## OPTIONS

max_lag <- 5
my_html_file <- 'tabs/tab03-Arch_Test.html'
my_xlsx_file <- 'tabs/tab03-Arch_Test.xlsx'

## END OPTIONS

library(tidyverse)
library(knitr)
library(kableExtra)
library(writexl)

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

source('fcts/garch_fcts.R')

# create directory
if (!dir.exists(dirname(my_html_file))) dir.create(dirname(my_html_file))

# get price data
df_prices <- read_rds('data/RAC-GARCH-Data.rds')

# do arch test
tab_out <- do_arch_test(x = df_prices$log_ret, max_lag = max_lag)

tab_out

# remove attributes of table so it can be correctly parsed in html
tab_out <- as.data.frame(
  lapply(tab_out, function(x) { attributes(x) <- NULL; x })
)
str(tab_out)

rownames(tab_out) <- NULL

# save table in html
my_tbl <- knitr::kable(tab_out, format = 'html' ) %>%
  kable_styling(bootstrap_options = c("striped"), 
                full_width = FALSE ) 

my_tbl

cat(my_tbl, file = my_html_file)  

# write to excel
write_xlsx(x = tab_out, path = my_xlsx_file)
