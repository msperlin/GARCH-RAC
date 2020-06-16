# A Garch Tutorial with R <link_paper_here>
#
# This script will estimate several garch models and find the best using the BIC
# criteria. A plot with the results, Figure 02 in the paper, is saved in a png file. 

## MAIN OPTIONS

max_global_lag <- 5 # max lag in all estimations

## END OPTIONS

library(tidyverse)
library(purrr)

graphics.off()

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

source('fcts/garch_fcts.R')

# get price data
df_prices <- read_rds('data/RAC-GARCH-Data.rds')


out <- find_best_arch_model(x = df_prices$log_ret, 
                            max_global_lag = max_global_lag)

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
                             data = df_prices$log_ret, trace = FALSE)

write_rds(my_garch, 'data/garch_model.rds')


