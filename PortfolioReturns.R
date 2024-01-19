
### Portfolio Returns, Example

library(tidyquant)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(PerformanceAnalytics)

# Asset tickers
tickers = c('AAPL', 'MSFT', 'XOM', 'JNJ', 'PYPL')

# Asset weights
wts = c(0.25,0.25,0.2,0.15,0.15)

# Get data
price_data <- tq_get(tickers,
                     from = '2023-01-01',
                     to = '2023-12-31',
                     get = 'stock.prices')

# Daily returns
returns_data <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,   #Simple daily returns of every stock
               period = "daily",
               col_rename = "returns")

# Portfolio returns (weighted sum of every daily stock return):
port_returns <- returns_data %>%
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = wts,
               col_rename = 'port_returns',
               geometric = FALSE)

#Cumulative Returns:
#Second line: mutate(cr = cumprod(1+ port_ret))
port_cumulative_returns <- port_returns %>%
  mutate(c_returns = (cumprod(1+ port_returns)-1)*100)  #To add/remove variables/columns from a Data Frame: mutate()


#Plot
port_cumulative_returns %>%
  ggplot(aes(x = date, y = c_returns)) +
  geom_line(color = "#5796e7") +
  labs(x = 'Month',
       y = 'Cumulative Returns (%)',
       title = 'Portfolio Performance 2023',
       subtitle = "This portfolio is is composed as follows: AAPL (25%), MSFT (25%), XOM (20%), JNJ (15%), PYPL (15%).",
       caption = "Just an example") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90), 
        panel.grid.major.y = element_line(color = "#cfd3d6", size = 0.1, linetype = 1),
        panel.grid.major.x = element_line(color = "#cfd3d6", size = 0.1, linetype = 1),
        plot.title = element_text(family = "Helvetica", face = "bold", size = (16)),
        plot.subtitle = element_text(family = "Helvetica", face = "italic", size = (10))) +
  scale_y_continuous(breaks = seq(0,25,5)) +
  scale_x_date(date_breaks = 'month')
#               date_labels = '%Y')


#Then, what is the portfolio return?
print(port_cumulative_returns$c_returns[length(port_cumulative_returns$c_returns)])








#sec.axis = sec_axis(~ ., breaks = data_ends)
#breaks = seq(0,25,5)

#plot.background = element_rect(fill = "grey98", color = "grey98") +
#geom_text_repel(aes(label = cr), data = port_cumulative_ret_tidyquant, size = 3) +
