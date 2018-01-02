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
                  riskless_rate = 0.03){}

#
# Internal Use
#
d <- function(stock_tick,
              sigma,
              strike,
              riskless_rate){

}
