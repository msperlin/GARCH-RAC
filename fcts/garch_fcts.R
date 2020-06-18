#' Returns a percentage as character vector
#'
#' @param x A numeric
#'
#' @return A string with percentages
#' @export
#'
#' @examples
#' my_perc(0.1)
my_perc <- function(x) {
  require(scales)
  x <- scales::percent(x, accuracy = 0.01)
}

#' Performan Arch text in a numerical vector
#'
#' @param x Numerical vector, such as returns
#' @param max_lag Maximum lag to use in arch test
#'
#' @return A dataframe with results
#' @export
#'
#' @examples
#' do_arch_test(runif(100))
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

#' Run Garch simulation
#'
#' @param n_sim Number of simulations
#' @param n_t Number of time periods in each simulation
#' @param my_garch A garch model estimated with rugarch
#' @param df_prices A dataframe with prices with columns ref.date and price.adjusted
#'
#' @return A dataframe with simulated prices and returns
#' @export
#'
#' @examples
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

#' Finds best ARMA-GARCH model 
#'
#' @param x A (numeric) vector of returns
#' @param type_models Type of models (see rugarch::rugarchspec)
#' @param dist_to_use Type of distributions to use (see rugarch::rugarchspec)
#' @param max_lag_AR Maximum lag for AR parameter
#' @param max_lag_MA Maximum lag for MA parameter
#' @param max_lag_ARCH Maximum lag for ARCH parameter
#' @param max_lag_GARCH Maximum lag for GARCH parameter
#'
#' @return A list with results
#' @export
#'
#' @examples
find_best_arch_model <- function(x, 
                                 type_models, 
                                 dist_to_use,
                                 max_lag_AR,
                                 max_lag_MA,
                                 max_lag_ARCH,
                                 max_lag_GARCH) {
  
  require(tidyr)
  
  df_grid <- expand_grid(type_models = type_models,
                         dist_to_use = dist_to_use,
                         arma_lag = 0:max_lag_AR,
                         ma_lag = 0:max_lag_MA,
                         arch_lag = 1:max_lag_ARCH,
                         garch_lag = 1:max_lag_GARCH)
  
  
  l_out <- pmap(.l = list(x = rep(list(x), nrow(df_grid)), 
                          type_model = df_grid$type_models,
                          type_dist = df_grid$dist_to_use,
                          lag_ar = df_grid$arma_lag,
                          lag_ma = df_grid$ma_lag,
                          lag_arch = df_grid$arch_lag,
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


#' Estimates a single Garch model
#'
#' @param x Numeric vector (tipicaly log returns)
#' @param type_model Type of model (see rugarch::rugarchspec)
#' @param type_dist Type of distribution (see rugarch::rugarchspec)
#' @param lag_ar Lag at AR parameter
#' @param lag_ma Lag at MA parameter
#' @param lag_arch Lag at ARCH parameter
#' @param lag_garch Lag at GARCH parameter
#'
#' @return A dataframe with estimation results
#' @export
#'
#' @examples
do_single_garch <- function(x, 
                            type_model, 
                            type_dist, 
                            lag_ar, 
                            lag_ma, 
                            lag_arch, 
                            lag_garch) {
  require(rugarch)
  
  
  spec = ugarchspec(variance.model = list(model =  type_model, 
                                          garchOrder = c(lag_arch, lag_garch)),
                    mean.model = list(armaOrder = c(lag_ar, lag_ma)),
                    distribution = type_dist)
  
  message('Estimating ARMA(',lag_ar, ',', lag_ma,')-',
          type_model, '(', lag_arch, ',', lag_garch, ')', 
          ' dist = ', type_dist,
          appendLF = FALSE)
  
  try({
    my_rugarch <- list()
    my_rugarch <- ugarchfit(spec = spec, data = x)
  })
  
  if (!is.null(coef(my_rugarch))) {
    message('\tDone')
    
    AIC <- rugarch::infocriteria(my_rugarch)[1]
    BIC <- rugarch::infocriteria(my_rugarch)[2]
  } else {
    message('\tEstimation failed..')
    
    AIC <- NA
    BIC <- NA
  }

  est_tab <- tibble(lag_ar, 
                    lag_ma,
                    lag_arch,
                    lag_garch,
                    AIC =  AIC,
                    BIC = BIC,
                    type_model = type_model,
                    type_dist,
                    model_name = paste0('ARMA(', lag_ar, ',', lag_ma, ')+',
                                        type_model, '(', lag_arch, ',', lag_garch, ') ',
                                        type_dist) ) 
  
  return(est_tab)
}


#' Reformats rugarch output to texreg
#' 
#' <https://stackoverflow.com/questions/57312645/how-to-export-garch-output-to-latex>   
#'
#' @param fit Rugarch model object
#' @param include.rsquared Should include rquared?
#' @param include.loglike Should include loglike?
#' @param include.aic  Should include AIC?
#' @param include.bic Should include BIC?
#'
#' @return A texreg friendly object
#' @export
#'
#' @examples
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