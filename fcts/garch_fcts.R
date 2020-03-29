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

do_sim <- function(n_sim = 1000, n_t = 1000, my_garch, df_prices) {
  require(tidyverse)
  
  do_single_sim <- function(i_sim, n_t, my_garch, df_prices) {
    
    message('Simulation ', i_sim)
    df_for <- garchSim(spec = garchSpec(model = coef(my_garch)), n = n_t)
    
    df_sim_out <- tibble(i_sim = i_sim, 
                         i_t = 0:length(df_for$garch),
                         ref_date = last(df_prices$ref.date) + i_t,
                         sim_log_ret = c(0, df_for$garch), # model was estimated on log returns
                         sim_arit_ret = exp(sim_log_ret)-1, # use arit return for price calc
                         sim_price = last(df_prices$price.adjusted)*(cumprod(1+sim_arit_ret)) )
    
    return(df_sim_out) 
  }
  
  df_out <- bind_rows(map(.x = 1:n_sim, .f = do_single_sim, my_garch = my_garch, n_t = n_t,
                          df_prices=df_prices))
  
  
}

find_best_arch_model <- function(x, max_global_lag = 5) {
  
  require(tidyr)
  
  df_grid <- expand_grid(arma_lag = 1:max_global_lag,
                         ma_lag = 1:max_global_lag,
                         arch_lag = 1:max_global_lag,
                         garch_lag = 1:max_global_lag)
  
  df_grid <- expand_grid(arma_lag = 0:max_global_lag,
                         garch_lag = 1:max_global_lag)
  
  l_out <- pmap(.l = list(x = rep(list(x), nrow(df_grid)), 
                          lag_ar = df_grid$arma_lag,
                          lag_ma = df_grid$arma_lag,
                          lag_arch = df_grid$garch_lag,
                          lag_garch  = df_grid$garch_lag),
                do_single_garch)
  
  tab_out <- bind_rows(l_out)
  
  # find by AIC
  idx <- which.min(tab_out$AIC)
  best_aic <- tab_out[idx, ]
  
  # find by BIC
  idx <- which.min(tab_out$BIC)
  best_bic <- tab_out[idx, ]
  
  l_out <- list(best_aic = best_aic,
                best_bic = best_bic,
                tab_out = tab_out)
  
  return(l_out)
}

do_single_garch <- function(x, lag_ar, lag_ma, lag_arch, lag_garch) {
  require(fGarch)
  
  my_formula <- formula(paste0('~arma(', lag_ar, ',',lag_ma,')', ' + ',
                               'garch(', lag_arch, ',', lag_garch, ')'))
  
  message('Estimating ', as.character(my_formula)[2])
  
  capture.output({
  my_garch <- fGarch::garchFit(formula = my_formula,
                               data = x, trace = FALSE)
  
  my_summary <- summary(my_garch)
  }, file = NULL)
  
  
  est_tab <- tibble(lag_ar, 
                    lag_ma,
                    lag_arch,
                    lag_garch,
                    AIC = my_garch@fit$ics['AIC'],
                    BIC = my_garch@fit$ics['BIC'],
                    model_name = paste0('ARMA(', lag_ar, ',', lag_ma, ')+',
                                        'GARCH(', lag_arch, ',', lag_garch, ')') )
  
  return(est_tab)
}
