# A Garch Tutorial with R - Finding the best model 
# Paper at <link_paper_here>
#
# This script will estimate several garch models and find the best using the BIC
# criteria. A plot with the results, Figure 02 in the paper, is saved in a .png file
# at folder /figs. 

## MAIN OPTIONS

max_global_lag <- 4 # max lag in all estimations, used in mean and variance equations
models_to_estimate <- c('sGARCH', 'eGARCH', 'gjrGARCH') # see rugarch manual for more

## END OPTIONS

library(tidyverse)
library(purrr)

graphics.off()

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

source('fcts/garch_fcts.R')

# get price data
df_prices <- read_rds('data/RAC-GARCH-Data.rds')


out <- find_best_arch_model(x = df_prices$log_ret, type_models = models_to_estimate,
                            max_global_lag = max_global_lag)

# get table with estimation results
tab_out <- out$tab_out

# pivot table to long format (better for plotting)
df_long <- tidyr::pivot_longer(data = tab_out %>%
                                 select(model_name,
                                        type_model,
                                        AIC, BIC),  cols = c('AIC', 'BIC'))

models_names <- unique(df_long$model_name)
best_models <- c(tab_out$model_name[which.min(tab_out$AIC)],
                 tab_out$model_name[which.min(tab_out$BIC)])

# figure out where is the best model
df_long <- df_long %>%
  mutate(order_model = if_else(model_name %in% best_models, 'Best Model', 'Not Best Model') ) %>%
  na.omit()

# make table with best models
df_best_models <- df_long %>%
  group_by(name) %>%
  summarise(model_name = model_name[which.min(value)],
            value = value[which.min(value)],
            type_model = type_model[which.min(value)])

# plot results
p1 <- ggplot(df_long %>%
               arrange(type_model), 
             aes(x = reorder(model_name, 
                             order(type_model)),
                 y = value, 
                 shape = name)) + 
  geom_point(size = 3.5) + 
  coord_flip() + 
  theme_bw() + facet_wrap(~name, scales = 'free_x') + 
  labs(title = 'Selecting Garch Models', 
       subtitle = 'The best model (in blue) is the one with lowest AIC or BIC',
       x = '',
       y = 'Value of Fitness Criteria') + 
  geom_point(data = df_best_models, inherit.aes = TRUE, 
             color = 'blue', size = 5) + 
  theme(legend.position = "none")

x11()  ; p1 ; ggsave('figs/02-best-garch.png')


# estimate best garch model by BIC (used in next section)
best_spec = ugarchspec(variance.model = list(model =  out$best_bic$type_model, 
                                             garchOrder = c(out$best_bic$lag_arch,
                                                            out$best_bic$lag_garch)),
                       mean.model = list(armaOrder = c(out$best_bic$lag_ar, 
                                                       out$best_bic$lag_ma)),
                       distribution = 'std')

my_best_garch <- ugarchfit(spec = best_spec, 
                           data = df_prices$log_ret)

write_rds(my_best_garch, 'data/garch_model.rds')


