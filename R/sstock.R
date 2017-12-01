#' Stock ticker simulation
#'
#' \code{sstock} outputs price path for a fully random stock evolution
#'
#'
sstock <- function(initial_stock_price = 50,
                   time_to_maturity = 4,
                   seed = 1,
                   scale = 100,
                   sigma = 1,
                   alpha = 0){

  # Create a Random Walk according to the params
  rw <- RandomWalk::srwalk(time_to_maturity = time_to_maturity,
                           seed = seed,
                           scale = scale)

  W <- rw$random_walk_path
  t <- rw$time_periods

  X <- sigma * W + ( alpha - 0.5 * sigma ^ 2 ) * t

  S <- initial_stock_price * exp(X)
  structure(data.frame(rw[, 1], S),
            names = c("time_periods", "stock_price_path")
  )
}
