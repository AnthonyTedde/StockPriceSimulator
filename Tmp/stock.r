library(RandomWalk)
library(StockPriceSimulator)
library(ggplot2)
library(grid)
library(gridExtra)
## Call the path generating function from equation:
#
#
#  Evolution of a Stock over 5 years. process track by
# formula and by ito.
stock_tick <- sstock(scale = 360, seed = 1, time_to_maturity = 5,
                     sigma = 1,
                     alpha = 0.12)
## Call the path generating function from Itôs approximation
stock_tick_ito <- sstock_ito(scale = 360, seed = 1, time_to_maturity = 5,
                             sigma = 1,
                             alpha = 0.12)

stock_tick_2 <- sstock(scale = 4, seed = 1, time_to_maturity = 5,
                     sigma = 1,
                     alpha = 0.12)


## Call the path generating function from Itôs approximation
stock_tick_ito_2 <- sstock_ito(scale = 4, seed = 1, time_to_maturity = 5,
                             sigma = 1,
                             alpha = 0.12)


stock_tick_3 <- sstock(scale = 360, seed = 1, time_to_maturity = 5,
                     sigma = .2,
                     alpha = 0.12)
## Call the path generating function from Itôs approximation
stock_tick_ito_3 <- sstock_ito(scale = 360, seed = 1, time_to_maturity = 5,
                             sigma = .2,
                             alpha = 0.12)

stock_tick_4 <- sstock(scale = 4, seed = 1, time_to_maturity = 5,
                       sigma = .2,
                       alpha = 0.12)


## Call the path generating function from Itôs approximation
stock_tick_ito_4 <- sstock_ito(scale = 4, seed = 1, time_to_maturity = 5,
                               sigma = .2,
                               alpha = 0.12)



p1 <- ggplot2::ggplot(stock_tick,
                      ggplot2::aes(time_periods, stock_price_path)) +
  ggplot2::geom_line(color = 'pink') +
  ggplot2::geom_line(data = stock_tick_ito,
                     ggplot2::aes(time_periods, stock_price_path),
                     color = 'steelblue')

p2 <-  ggplot2::ggplot(stock_tick_2,
                       ggplot2::aes(time_periods, stock_price_path)) +
  ggplot2::geom_line(color = 'pink') +
  ggplot2::geom_line(data = stock_tick_ito_2,
                     ggplot2::aes(time_periods, stock_price_path),
                     color = 'steelblue')

p3 <- ggplot2::ggplot(stock_tick_3,
                      ggplot2::aes(time_periods, stock_price_path)) +
  ggplot2::geom_line(color = 'pink') +
  ggplot2::geom_line(data = stock_tick_ito_3,
                     ggplot2::aes(time_periods, stock_price_path),
                     color = 'steelblue')

p4 <-  ggplot2::ggplot(stock_tick_4,
                       ggplot2::aes(time_periods, stock_price_path)) +
  ggplot2::geom_line(color = 'pink') +
  ggplot2::geom_line(data = stock_tick_ito_4,
                     ggplot2::aes(time_periods, stock_price_path),
                     color = 'steelblue')

grid.arrange(p1, p2, p3, p4, ncol = 2)

#
# Empicial distribution of Delta S / S
#
# stock_tick = vector(length = 3000)
# for(i in 1:3000)
#   stock_tick[i] <- tail(sstock(scale = 360, seed = i, time_to_maturity = 5,
#                        sigma = .20,
#                      alpha = 0.12)$stock_price_path, 1)
#
# mean(stock_tick)
# 50 * exp(.12 * 5)
