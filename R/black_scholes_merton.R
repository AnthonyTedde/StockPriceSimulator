#' This function compute the Black-Scholes-Merton call and put price for [time -
#' stock price]
#'
#' Black-Scholes_Merton equation
#'
#'
#' @param stock_path data.frame containing the stock price evolution along with
#'   the appropriate time serie
#' @param delta_time
#' @param current_stock_price
#' @param k Strike price of the option. By default the option is in the money.
#'   it means that the strike is equal to the initial price of stock (at time 1)
#' @param r Riskless interest rate - 0.03 if no value provided.
#' @param sigma
#'
#' @return
#' @export
#'
#' @examples
#'
BSM <- function(stock_path = sstock(),
                k = stock_path$stock_price_path[1],
                r = 0.03,
                sigma = 1){

  # Create new and make easily usable and/or readable some variables
  #
  S <- stock_path$stock_price_path
  t <- stock_path$time_periods
  remaining_time <- tail(t, 1) - t

  # Data transformation
  #
  # From theory, d1 make reference to d+ while d2 refers to d-
  #
  args <- list(stock_tick = stock_path, sigma = sigma, strike = k, riskless_rate = r)
  d1 <- do.call(d, args)
  d2 <- do.call(d, c(args, sign = '-'))

  # Output
  #
  # In this version the time evolved with the stock price as well.
  # TODO: a fixed time BSM.
  C <- S * pnorm(d1) - k * exp(-r * remaining_time) * pnorm(d2)
  structure(data.frame(time_periods = t,
                       option_price_path = C))
}

