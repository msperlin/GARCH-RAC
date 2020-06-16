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
  require(rugarch)
  
  do_single_sim <- function(i_sim, n_t, my_garch, df_prices) {
    
    
    message('Simulation ', i_sim)
    
    rugarch_sim = ugarchsim(my_garch, n.sim = n_t, 
                            m.sim = 1)
    
    sim_series <- rugarch_sim@simulation$seriesSim
    
    df_sim_out <- tibble(i_sim = i_sim, 
                         i_t = 0:length(sim_series),
                         ref_date = last(df_prices$ref.date) + i_t,
                         sim_log_ret = c(0, sim_series), # model was estimated on log returns
                         sim_arit_ret = exp(sim_log_ret)-1, # use arit return for price calc
                         sim_price = last(df_prices$price.adjusted)*(cumprod(1+sim_arit_ret)) )
    
    return(df_sim_out) 
  }
  
  df_out <- bind_rows(map(.x = 1:n_sim, 
                          .f = do_single_sim, 
                          my_garch = my_garch, 
                          n_t = n_t,
                          df_prices=df_prices))
  
  
}

find_best_arch_model <- function(x, type_models, max_global_lag = 5) {
  
  require(tidyr)
  
  df_grid <- expand_grid(type_models = type_models,
                         arma_lag = 1:max_global_lag,
                         ma_lag = 1:max_global_lag,
                         arch_lag = 1:max_global_lag,
                         garch_lag = 1:max_global_lag)
  
  df_grid <- expand_grid(type_models = type_models,
                         arma_lag = 0:max_global_lag,
                         garch_lag = 1:max_global_lag)
  
  l_out <- pmap(.l = list(x = rep(list(x), nrow(df_grid)), 
                          type_model = df_grid$type_models,
                          lag_ar = df_grid$arma_lag,
                          lag_ma = df_grid$arma_lag,
                          lag_arch = df_grid$garch_lag,
                          lag_garch  = df_grid$garch_lag),
                do_single_garch_rugarch)
  
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

do_single_garch_fGarch <- function(x, lag_ar, lag_ma, lag_arch, lag_garch) {
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

do_single_garch_rugarch <- function(x, type_model, lag_ar, lag_ma, lag_arch, lag_garch) {
  require(rugarch)
  
  
  spec = ugarchspec(variance.model = list(model =  type_model, 
                                          garchOrder = c(lag_arch, lag_garch)),
                    mean.model = list(armaOrder = c(lag_ar, lag_ma)),
                    distribution = 'std')
  
  message('Estimating ARMA(',lag_ar, ',', lag_ma,')-',
          type_model, '(', lag_arch, ',', lag_garch, ')', appendLF = FALSE)
  
  try({
    my_rugarch <- list()
    my_rugarch <- ugarchfit(spec = spec, data = x)
  })
  
  if (!is.null(coef(my_rugarch))) {
    message('\tDone')
  } else {
    message('\tEstimation failed..')
    
    est_tab <- tibble(lag_ar = NA, 
                      lag_ma = NA,
                      lag_arch = NA,
                      lag_garch = NA,
                      AIC =  NA,
                      BIC = NA,
                      type_model = type_model,
                      model_name = paste0('ARMA(', lag_ar, ',', lag_ma, ')+',
                                          type_model, '(', lag_arch, ',', lag_garch, ')') )
    return(est_tab)
  }
  
  AIC <- rugarch::infocriteria(my_rugarch)[1]
  BIC <- rugarch::infocriteria(my_rugarch)[2]
  
  est_tab <- tibble(lag_ar, 
                    lag_ma,
                    lag_arch,
                    lag_garch,
                    AIC =  AIC,
                    BIC = BIC,
                    type_model = type_model,
                    model_name = paste0('ARMA(', lag_ar, ',', lag_ma, ')+',
                                        type_model, '(', lag_arch, ',', lag_garch, ')') )
  
  return(est_tab)
}

# Reformating rugarch output to texreg
# https://stackoverflow.com/questions/57312645/how-to-export-garch-output-to-latex
extract.rugarch <- function(fit, 
                            include.rsquared = TRUE, 
                            include.loglike = TRUE, 
                            include.aic = TRUE, 
                            include.bic = TRUE) {
  
  require(texreg)
  
  # extract coefficient table from fit:
  coefnames <- rownames(as.data.frame(fit@fit$coef))
  coefs <- fit@fit$coef
  se <- as.vector(fit@fit$matcoef[, c(2)])
  pvalues <-  as.vector(fit@fit$matcoef[, c(4)])       # numeric vector with p-values
  
  # create empty GOF vectors and subsequently add GOF statistics from model:
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.rsquared == TRUE) {
    r2 <-  1 - (var(fit@fit$residuals) / var(y))
    gof <- c(gof, r2)
    gof.names <- c(gof.names, "R^2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglike == TRUE) {
    loglike <- fit@fit$LLH
    gof <- c(gof, loglike)
    gof.names <- c(gof.names, "Log likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aic == TRUE) {
    aic <- infocriteria(fit)[c(1)]
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  if (include.bic == TRUE) {
    bic <- infocriteria(fit)[c(2)]
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  # create texreg object:
  tr <- createTexreg(
    coef.names = coefnames, 
    coef = coefs,
    se = se,
    pvalues = pvalues, 
    gof.names = gof.names, 
    gof = gof, 
    gof.decimal = gof.decimal
  )
  return(tr)
}