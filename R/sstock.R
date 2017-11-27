sstock <- function(time_to_maturity = 4,
                   seed = 1,
                   scale = 100,
                   sigma = 1){

  # Create a Random Walk according to the params
  rw <- RandomWalk::srwalk(time_to_maturity = time_to_maturity,
                           seed = seed,
                           scale = scale)

  X <- sigma * rw[, 2] - 0.5 * sigma ^ 2 * rw[, 1]
  structure(data.frame(rw[, 1], X),
            names = c("time_periods", "stock_price_path")
  )
}
