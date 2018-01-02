#' The delta in the Black-Scholes-Merton corresponds to the first derivative of
#' the BSM function with respect to x. It practically refers to the amount of
#' stock to keep in order to hedge an call/put option
#'
#' Delta
#'
#' @inheritParams sstock
#' @param strike
#' @param riskless_rate
#'
#' @return
#'
#' @export
#'
#' @examples
delta <- function(initial_stock_price = 50,
                  time_to_maturity = 4,
                  seed = 1,
                  scale = 100,
                  sigma = 1,
                  alpha = 0,
                  strike = initial_stock_price,
                  riskless_rate = 0.03){

  # Create stock price path according to parameters:
  S <- sstock(initial_stock_price = initial_stock_price,
              time_to_maturity = time_to_maturity,
              seed = seed,
              scale = scale,
              sigma = sigma,
              alpha = alpha)

  d_plus <- StockPriceSimulator::d(stock_tick = S,
                                   sigma = sigma,
                                   strike = strike,
                                   riskless_rate = riskless_rate)
  pnorm(d_plus)
}

#' The theta in the Black-Scholes-Merton corresponds to the first derivative of
#' the BSM function with respect to time(t). It can be thought as the
#' instantaneous change in BSM price when time move forward
#'
#' Theta
#'
#' @inheritParams delta
#'
#' @return
#'
#' @export
#'
#' @examples
theta <- function(initial_stock_price = 50,
                  time_to_maturity = 4,
                  seed = 1,
                  scale = 100,
                  sigma = 1,
                  alpha = 0,
                  strike = initial_stock_price,
                  riskless_rate = 0.03){

}

#' The gamma in the Black-Scholes-Merton corresponds to the second derivative of
#' the BSM function with respect to x^2.
#'
#' gamma
#'
#' @inheritParams delta
#'
#' @return
#'
#' @export
#'
#' @examples
gamma <- function(initial_stock_price = 50,
                  time_to_maturity = 4,
                  seed = 1,
                  scale = 100,
                  sigma = 1,
                  alpha = 0,
                  strike = initial_stock_price,
                  riskless_rate = 0.03){

}

#
# Internal Use
#
#' Title
#'
#' @param stock_tick
#' @param sigma
#' @param strike
#' @param riskless_rate
#' @param sign
#'
#' @return
#' @export
#'
#' @examples
d <- function(stock_tick,
              sigma,
              strike,
              riskless_rate,
              sign = '+'){

  t <- stock_tick$time_periods
  S <- stock_tick$stock_price_path
  remaining_time <- tail(t, 1) - t

  outer_multiplier <- 1/(sigma * sqrt(remaining_time))

  # tmp variable used in eval_parse_paste function. Creation of that variable
  # remove complexity of the operation
  tmp <- sigma ^ 2 / 2
  inner_multiplier <- eval(parse(text =
                                   paste(riskless_rate,
                                         sign,
                                         tmp)))
  outer_multiplier * (log(S/strike) + inner_multiplier * remaining_time)

}
