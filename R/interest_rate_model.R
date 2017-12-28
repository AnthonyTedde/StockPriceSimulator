#' Title
#'
#' @param aplha
#' @param beta
#' @param sigma
#' @param initial_interest_rate
#' @param time_to_maturity
#' @param scale
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
#' @export
vasicek_ir <- function(initial_interest_rate = .10,
                       time_to_maturity = 4,
                       scale = 100,
                       seed = 1,
                       alpha = 0,
                       beta = 1,
                       sigma = 1
                       ){
  # vectors needed in order to compute the rate
  R0 <- initial_interest_rate
  bm <- RandomWalk::sbmotion(time_to_maturity = time_to_maturity,
                             seed = seed,
                             scale = scale)
  # Because these followings appear several time in the model:
  # e ^(-beta * t)
  # e ^(beta * t)
  e_minus_beta_t <- exp(-beta * bm$time_periods)
  e_beta_t <- exp(beta * bm$time_periods)

  differential_bm <- as.data.frame(diff(as.matrix(bm)))

  R <- e_minus_beta_t * R0 +
    (alpha / beta) * (1 - e_minus_beta_t) +
    sigma * e_minus_beta_t * c(0, cumsum(e_beta_t[-length(e_beta_t)] *
                                           differential_bm$brownian_motion_path))
  data.frame('time_periods' = bm$time_periods,
             'interest_rate_path' = R)
}


CIR_ir <- function(){

}
