#' Black-Scholes_Merton equation
#'
#' @param delta_time
#' @param current_stock_price
#' @param strike
#' @param interest_rate
#' @param sigma
#'
#' @return
#' @export
#'
#' @examples
BSM <- function(delta_time,
                current_stock_price,
                strike,
                interest_rate,
                sigma){
  x <- current_stock_price
  k <- strike
  r <- interest_rate

  d_plus <- 1/(sigma * sqrt(delta_time)) * (log(x/k) + (r + sigma ^2) * delta_time)
  d_min <- 1/(sigma * sqrt(delta_time)) * (log(x/k) + (r - sigma ^2) * delta_time)

  x * pnorm(d_plus) - k * exp(-r * delta_time) * pnorm(d_min)
}
