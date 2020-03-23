library(tidyverse)
library(FinTS)
library(fGarch)

graphics.off()

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

source('fcts/garch_fcts.R')

n_sim <- 1000 # number of simulations

# get price and model data
df_ibov <- read_rds('data/RAC-GARCH-Data.rds')
my_garch <- read_rds('data/garch_model.rds')


do_sim <- function(n_sim = 1000, n_t = 1000, my_garch, df_ibov) {
  require(tidyverse)
  
  do_single_sim <- function(i_sim, n_t, my_garch, df_ibov) {
    
    message('Simulation ', i_sim)
    df_for <- garchSim(spec = garchSpec(model = coef(my_garch)), n = n_t)
    
    return(tibble(i_sim = i_sim, 
                  i_t = 0:length(df_for$garch),
                  ref.date = last(df_ibov$ref.date) + i_t,
                  sim_ret = c(0, df_for$garch),
                  sim_price = last(df_ibov$price.adjusted)*(1+cumsum(sim_ret)))  ) 
  }
  
  df_out <- bind_rows(map(.x = 1:n_sim, .f = do_single_sim, my_garch = my_garch, n_t = n_t,
                          df_ibov=df_ibov))
  

}

df_sim <- do_sim(n_sim = n_sim, 
                 n_t = 2500, my_garch, df_ibov = df_ibov)

df_sim 

df_ibov_temp <- df_ibov %>%
  dplyr::filter(ref.date > as.Date('2015-01-01'))

p1 <- ggplot() + 
  geom_line(data = df_ibov_temp, 
            aes(x = ref.date, y = price.adjusted), color = 'black', size = 0.75)  + 
  geom_line(data = df_sim, aes(x =ref.date, y = sim_price, group = i_sim),
            color = 'grey', 
            size = 0.35) + 
  theme_bw() + 
  geom_hline(yintercept = max(df_ibov_temp$price.adjusted)) + 
  labs(title = 'Projections of Ibovespa Based on GARCH Model',
       subtitle = paste0('Total of ', n_sim, ' simulations based on a ',
                         toupper(as.character(my_garch@formula)[3]), ' model selected by BIC'),
       x = '',
       y = 'Index Value')


x11(); p1 ; ggsave('figs/03-Ibov-Simulation.png')

tab_prob <- df_sim %>%
  group_by(ref.date) %>%
  summarise(prob = mean(sim_price > max(df_ibov_temp$price.adjusted)))

my_idx_date <- first(which(tab_prob$prob > 0.5))
df_date <- tibble(idx = c(first(which(tab_prob$prob > 0.001)),
                          first(which(tab_prob$prob > 0.5)),
                          first(which(tab_prob$prob > 0.99))),
                  ref.date = tab_prob$ref.date[idx],
                  prob = tab_prob$prob[idx],
                  my_text = format(ref.date, '%d/%m/%Y'))

p2 <- ggplot(tab_prob, aes(x = ref.date, y = prob) ) + 
  geom_line(size = 2) + 
  labs(title = paste0('Probabilities of Ibovespa Reaching its Peak (', 
                      max(df_ibov_temp$price.adjusted), ' points)'),
       subtitle = paste0('Calculations based on simulations of GARCH model'),
       x = '',
       y = 'Probability') + 
  scale_y_continuous(labels = scales::percent) + 
  geom_point(data = df_date,
             aes(x = ref.date, y = prob), size = 5, color = 'red') + 
  geom_text(data = df_date, aes(x = ref.date, y = prob, 
                                label = my_text), 
            nudge_x = nrow(tab_prob)*0.085,
            nudge_y = -0.05,
            color ='red', check_overlap = TRUE) + 
  theme_bw()


x11(); p2 ; ggsave('figs/04-Ibov-Probs.png')
