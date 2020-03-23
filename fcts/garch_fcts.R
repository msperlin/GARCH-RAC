my_perc <- function(x) {
  require(scales)
  x <- scales::percent(x, accuracy = 0.01)
}

do_arch_test <- function(x, max_lag = 5) {
  require(FinTS)
  require(tidyverse)
  
  do_single_arch <- function(x, used_lag)  {
    test_out <- FinTS::ArchTest(x, lags = used_lag)
    
    res_out <- tibble(lag = used_lag,
                      statistic = test_out$statistic, 
                      pvalue = test_out$p.value)
  }
  
  tab_out <- bind_rows(map(1:max_lag,.f = do_single_arch, x = x))
  
  return(tab_out)
}