library(tidyverse)
library(purrr)

graphics.off()

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

source('fcts/garch_fcts.R')

# get price data
df_ibov <- read_rds('data/RAC-GARCH-Data.rds')

find_best_arch_model <- function(x, max_global_lag = 5) {
  
  
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
  my_garch <- fGarch::garchFit(formula = my_formula,
                               data = x, trace = FALSE)
  my_summary <- summary(my_garch)
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

out <- find_best_arch_model(x = df_ibov$ret, max_global_lag = 5)

out$best_bic

tab_out <- out$tab_out

df_long <- tidyr::pivot_longer(data = tab_out %>%
                                 select(model_name, AIC, BIC),  cols = c('AIC', 'BIC'))

models_names <- unique(df_long$model_name)
best_models <- c(tab_out$model_name[which.min(tab_out$AIC)],
                 tab_out$model_name[which.min(tab_out$BIC)])

df_long <- df_long %>%
  mutate(order_model = if_else(model_name %in% best_models, 'Best Model', 'Not Best Model') )

df_best_models <- df_long %>%
  group_by(name) %>%
  summarise(model_name = model_name[which.min(value)],
            value = value[which.min(value)])


p1 <- ggplot(df_long, aes(x = model_name, y = value, shape = name)) + 
  geom_point(size = 4) + coord_flip() + 
  theme_bw() + facet_wrap(~name, scales = 'free_x') + 
  labs(title = 'Selecting Garch Models', 
       subtitle = 'The best model is the one with lowest AIC or BIC',
       x = '',
       y = 'Value of Fitness Criteria') + 
  geom_point(data = df_best_models, inherit.aes = TRUE, 
             color = 'blue', size = 5) + 
  coord_flip()  + 
  theme(legend.position = "none")

x11()  ; p1 ; ggsave('figs/02-best-garch.png')



# estimate best garch model by BIC (used in next section)
my_formula <- formula(paste0('~arma(', out$best_bic$lag_ar, ',',out$best_bic$lag_ma,')', ' + ',
                             'garch(', out$best_bic$lag_arch, ',', out$best_bic$lag_garch, ')'))
my_garch <- fGarch::garchFit(formula = my_formula,
                             data = df_ibov$ret, trace = FALSE)

write_rds(my_garch, 'data/garch_model.rds')


