#' The delta in the Black-Scholes-Merton corresponds to the first derivative of
#' the BSM function with respect to x. It practically refers to the amount of
#' stock to keep in order to hedge an call/put option
#'
#' Delta
#'
#' @inheritParams sstock
#' @param strike Strike price defined for the stock option
#' @param riskless_rate Riskless rate
#'
#' @return
#'
#' @export bsm_delta delta
#'
#' @examples
bsm_delta <- delta <- function(initial_stock_price = 50,
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
#' (!) due to the zero division as t -> T, the final value when t = T is not a
#' number
#'
#' Theta
#'
#' @inheritParams delta
#'
#' @return
#'
#' @export bsm_theta theta
#'
#' @examples
bsm_theta <- theta <- function(s,
                  sigma = 1,
                  strike = initial_stock_price,
                  riskless_rate = 0.03){

  # Create stock price path according to parameters:
  S <- s

  # Simplify notation
  t <- S$time_periods
  X <- S$stock_price_path
  r <- riskless_rate
  k <- strike

  remaining_time = tail(t, 1) - t

  # Call to function d
  args <- list(stock_tick = S,
               sigma = sigma,
               strike = strike,
               riskless_rate = riskless_rate)

  d_plus <- do.call(StockPriceSimulator::d, args)

  d_min <- do.call(StockPriceSimulator::d, c(args, sign = '-'))

  # Formula Subset to remove the NaN last value
  (
  -r * k * exp(-r * remaining_time) * pnorm(d_min) -
    (sigma * X) / (2 * sqrt(remaining_time)) * dnorm(d_plus)
  )
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
#' @export bsm_gamma gamma
#'
#' @examples
bsm_gamma <- gamma <- function(s,
                  sigma = 1,
                  strike = initial_stock_price,
                  riskless_rate = 0.03){
# Simplify notation
  t <- s$time_periods
  X <- s$stock_price_path
  r <- riskless_rate
  k <- strike

  remaining_time = tail(t, 1) - t

  # Call to function d
  args <- list(stock_tick = s,
               sigma = sigma,
               strike = strike,
               riskless_rate = riskless_rate)

  d_plus <- do.call(StockPriceSimulator::d, args)

  # Formula Subset to remove the NaN last value
  (
  1/(sigma * X * sqrt(remaining_time)) * dnorm(d_plus)
  )
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



#' @export
#'
#' @examples
delta_bsm <- function(s = sstock(),
                  sigma = 1,
                  strike = initial_stock_price,
                  riskless_rate = 0.03){

  # Create stock price path according to parameters:
  S <- s

  d_plus <- StockPriceSimulator::d(stock_tick = S,
                                   sigma = sigma,
                                   strike = strike,
                                   riskless_rate = riskless_rate)
  pnorm(d_plus)
}
