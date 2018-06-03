#' This function compute the delta hedging for time series following
#' Geometric Brownian Motion. i.e. log-return are normally distributed.
#'
#' Delta hedging of Black-Scholes_Merton
#'
#'
#' @param time_to_maturity Time up to maturity in year
#' @param S Initial value of the Stock
#' @param strike Strike price of the option.
#' @param sigma Volatility rate of the simulated stock time series
#' @param alpha Drift rate of the simulated stock time series
#' @param r Riskless interest rate - 0.03 if no value provided.
#' @param scale Year divided factor
#'
#' @return
#'
#' @export
#'
#' @examples
#'
delta_hedging <- function(time_to_maturity,
                          S,
                          strike,
                          sigma = 0.2,
                          alpha = 0.05,
                          r = 0.03,
                          scale = 365){

  ## 2. Simulate 500 GRM that correspond to that date
  GBM <- map(1:500, ~sstock(initial_stock_price = S,
                            seed = .x,
                            alpha = alpha,
                            sigma = sigma,
                            time_to_maturity = time_to_maturity,
                            scale = scale))

  # Option price at Time t = 0
  o <- BSM(GBM[[1]],
           k = strike,
           r = r,
           sigma = sigma)$option_price_path[1]

  #  Computation of delta for each time series GBM
  d <- map(1:500, ~delta(initial_stock_price = S,
                         time_to_maturity = time_to_maturity,
                         seed = .x,
                         scale = scale,
                         sigma = sigma,
                         alpha = alpha,
                         strike = strike,
                         riskless_rate = r))

  # Get the delta of the delta for each time series.
  diff_delta <- map(d,
                    ~ c(.x[1], diff(.x)))

  # list of arrays used in functional pmap structure.
  l <- list(diff_delta,
            d,
            GBM)

  # Output a dataframe containing:
  #   * The remaining time till maturity
  #   * The delta of the delta. By convention the first item is the value of
  #     delta at time 0
  #   * The value of delta for each time
  #   * The value of the stock for each time following a Geom. Brownian Motion
  #   * The Total amount paid to get delta stock at time t
  #   * The Total amout paid to get delta stock at time t, but value for T
  u <- pmap(l, .f = function(dd, d, G){

    time_remaining <- (time_to_maturity * scale + 1) : 1
    amount_paid <- dd * G$stock_price_path
    amount_with_interest <- amount_paid * (1 + r / (scale)) ^ (time_remaining - 1)

    data.frame("time remaining" = time_remaining,
               "diff delta" = dd,
               "delta" = d,
               "S" = G$stock_price_path,
               "amount paid" = amount_paid,
               "with interest" = amount_with_interest)

  })


  i <- map_dbl(u,
               ~ sum(.x$with.interest)
               - .x$delta[time_to_maturity * scale + 1] * strike
               - o * (1 + r / (scale)) ^ (time_to_maturity * scale))

  structure(list("delta" = i,
                 "summury" = u))
}
