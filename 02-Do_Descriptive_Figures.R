library(tidyverse)
library(cowplot)
library(GetBCBData)

graphics.off()

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# get price data
df_ibov <- read_rds('data/RAC-GARCH-Data.rds')

# get inflation data
df_inflation <- gbcbd_get_series(id = 433, first.date = min(df_prices$ref.date), 
                                 last.date = max(df_prices$ref.date)) %>%
  mutate(inf_index  = cumprod(1+value/100))

total_ibov_ret <- last(df_ibov$price.adjusted)/first(df_ibov$price.adjusted)-1
total_inflation <- last(df_inflation$inf_index)/first(df_inflation$inf_index) - 1 
n_years <- as.numeric(max(df_ibov$ref.date) - min(df_ibov$ref.date))/365
ret_ibov_year = (1+total_ibov_ret)^(1/n_years) - 1
ret_inflation_year = (1+total_inflation)^(1/n_years) - 1

real_ret_ibov <- (1+total_ibov_ret)/(1+total_inflation) - 1
real_ret_ibov_year <- (1+ret_ibov_year)/(1+ret_inflation_year) - 1

p1 <- ggplot(df_ibov, aes(x = ref.date, y = price.adjusted)) + 
  geom_line() + 
  labs(title = paste0('Ibovespa Market Index'),
       subtitle = paste0('Total nominal return of the index is equivalent to ', 
                         my_perc(total_ibov_ret),
                         ' (', my_perc(ret_ibov_year), ' per year)\n',
                         'Total real (without inflation) return of the index is equivalent to ',
                         my_perc(real_ret_ibov), 
                         ' (', my_perc(real_ret_ibov_year), ' per year)'),
       x = '',
       y = 'Index Value',
       caption = 'Data from Yahoo Finance') + 
  theme_bw() 


n_largest <- 10

largest_tab <- df_ibov %>%
  group_by(ticker) %>%
  top_n(abs(ret), n = n_largest) %>%
  mutate(text = paste0('Ticker:', ticker))

p2 <- ggplot(df_ibov, 
             aes(x = ref.date, y = ret)) + 
  geom_line() + 
  labs(title = paste0('IBOV - Adjusted Daily Returns and Largest Absolute Returns'),
       subtitle = paste0('Red circles represent the largest ', n_largest, 
                         ' absolute price variations in the sample'),
       x = '',
       y = 'Ajusted Returns',
       caption = 'Data from Yahoo Finance') + 
  theme_bw() + 
  geom_point(data = largest_tab, aes(x = ref.date, y = ret), 
             size = 3, color = 'red'  ) +
  scale_y_continuous(labels = scales::percent) + 
  labs(size = 'Absolute Price Variation') + 
  scale_color_brewer(palette = 'BrBG')

p <- plot_grid(p1, p2, nrow = 2, labels = 'AUTO')

x11() ; p ; ggsave('figs/01_IBOV_prices_returns.png', p)

