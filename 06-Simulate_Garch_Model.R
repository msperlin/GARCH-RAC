# A Garch Tutorial with R - Simulation of prices
# Paper at <link_paper_here>
# 
# This script will use the best garch model from previous script and simulate
# many return series into the future. After the simulations, the code calculates 
# probabilities for the simulated paths to reach the maximum value of index Ibovespa.

## OPTIONS

set.seed(20200320) # fix seed for simulations (20200320 replicates the paper's results)
n_sim <- 5000 # number of simulations (5000 was used in paper,
# be aware that this code is memory intensive and might freeze your computer. 
# Increase n_sim at your own risk!!
n_days_ahead <- 6*365 # Number of days ahead to simulate (6*365 in paper)

## END OPTIONS

library(tidyverse)
library(fGarch)
library(ggtext)

graphics.off()

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# get price and model data
df_prices <- read_rds('data/RAC-GARCH-Data.rds')
my_garch <- read_rds('data/garch_model.rds')
series_name <- df_prices$series_name[1]

# source functions
source('fcts/garch_fcts.R')

# do simulations
df_sim <- do_sim(n_sim = n_sim, 
                 n_t = n_days_ahead, 
                 my_garch, 
                 df_prices = df_prices)

glimpse(df_sim )

# calculate probabilities of reaching peak value
tab_prob <- df_sim %>%
  group_by(ref_date) %>%
  summarise(prob = mean(sim_price > max(df_prices$price.adjusted)))

n_years_back <- 4
df_prices_temp <- df_prices %>%
  dplyr::filter(ref.date > max(ref.date) - n_years_back*365)

my_garch_name <- toupper(as.character(my_garch@model$modeldesc$vmodel))

p1 <- ggplot() + 
  geom_line(data = df_prices_temp, 
            aes(x = ref.date, y = price.adjusted), color = 'black', size = 0.75)  + 
  geom_line(data = df_sim, 
            aes(x = ref_date, 
                y = sim_price, 
                group = i_sim),
            color = 'grey', 
            size = 0.25,
            alpha = 0.015) + 
  theme_bw() + 
  geom_hline(yintercept = max(df_prices_temp$price.adjusted)) + 
  labs(title = paste0('Projections of ', series_name),
       subtitle = paste0('Total of ', n_sim, ' simulations based on a ',
                         my_garch_name, 
                         ' model selected by BIC'),
       caption = 'Data from Yahoo Finance',
       x = '',
       y = 'Value') + 
  ylim(c(0.75*min(df_prices_temp$price.adjusted), 
         1.25*max(df_prices_temp$price.adjusted))) + 
  xlim(c(max(df_prices_temp$ref.date) - n_years_back*365,
         max(df_prices_temp$ref.date) + 2*365) )


# plot graphics
x11(); p1 ; ggsave(paste0('figs/fig04_', series_name, '_price_simulation.png'))

my_idx_date <- first(which(tab_prob$prob > 0.5))
df_date <- tibble(idx = c(first(which(tab_prob$prob > 0.001)),
                          first(which(tab_prob$prob > 0.5)),
                          first(which(tab_prob$prob > 0.9))),
                  ref_date = tab_prob$ref_date[idx],
                  prob = tab_prob$prob[idx],
                  my_text = paste0(format(ref_date, '%d/%m/%Y'),
                                   '\nprob = ', scales::percent(prob) ) )

df_textbox <- tibble(ref_date = df_date$ref_date[2],
                     prob = 0.25,
                     label = paste0('According to the estimated _', my_garch_name, '_ model, ', 
                                    'asset **', series_name, '** will likely reach ',
                                    'its historical peak value of ', 
                                    format(max(df_prices$price.adjusted), 
                                           big.mark = '.',
                                           decimal.mark = ','),
                                    ' at ', format(ref_date, '%d/%m/%Y'), '.') )

p2 <- ggplot(tab_prob, aes(x = ref_date, y = prob) ) + 
  geom_line(size = 2) + 
  labs(title = paste0('Probabilities of ', series_name, ' Reaching its Historical Peak'),
       subtitle = paste0('Calculations based on simulations of ',
                         my_garch_name, 
                         ' model'),
       x = '',
       y = 'Probability') + 
  scale_y_continuous(labels = scales::percent) + 
  geom_point(data = df_date,
             aes(x = ref_date, y = prob), size = 5, color = 'red') + 
  geom_text(data = df_date, aes(x = ref_date, y = prob, 
                                label = my_text), 
            nudge_x = nrow(tab_prob)*0.085,
            nudge_y = -0.05,
            color ='red', check_overlap = TRUE) + 
  geom_textbox(data = df_textbox, 
               mapping = aes(x = ref_date, 
                             y = prob, 
                             label = label),
               width = unit(0.5, "npc"),
               #fill = "cornsilk",
               hjust = 0) + 
  theme_bw()

x11(); p2 ; ggsave(paste0('figs/fig05_', series_name, '_prob_reaching_peak.png'))
