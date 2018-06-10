time_to_maturity <- 399/365
S <- 186.31
strike <- 130
sigma = 0.2
alpha = 0.40
r = 0.05
scale = 365
full <- T

  ## 2. Simulate 500 GRM that correspond to that date
GBM <- purrr::map(1:500, ~sstock(initial_stock_price = S,
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
  d <- purrr::map(1:500, ~delta(initial_stock_price = S,
                         time_to_maturity = time_to_maturity,
                         seed = .x,
                         scale = scale,
                         sigma = sigma,
                         alpha = alpha,
                         strike = strike,
                         riskless_rate = r))

  # Get the delta of the delta for each time series.
  diff_delta <- purrr::map(d,
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
  u <- purrr::pmap(l, .f = function(dd, d, G){

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


  i <- purrr::map_dbl(u,
               ~ sum(.x$with.interest)
               - .x$delta[time_to_maturity * scale + 1] * strike
               - o * (1 + r / (scale)) ^ (time_to_maturity * scale))

  hedging_cost <- purrr::map_dbl(u,
                                 ~ sum(.x$with.interest) - .x$delta[time_to_maturity * scale + 1] * strike)
  hedging_cost_sd <- sd(hedging_cost)
  hedging_perf <- hedging_cost / o

  if(full)
    structure(list("delta" = i,
                   "perf" = hedging_perf,
                   "summury" = u))
  else hedging_perf
