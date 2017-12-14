#' Stock ticker simulation
#'
#' \code{sstock} outputs price path for a fully random stock evolution
#'
#' @export
sstock <- function(initial_stock_price = 50,
                   time_to_maturity = 4,
                   seed = 1,
                   scale = 100,
                   sigma = 1,
                   alpha = 0){

    ## Create a Random Walk according to the params
    bm <- RandomWalk::sbmotion(time_to_maturity = time_to_maturity,
                             seed = seed,
                             scale = scale)

    W <- bm$brownian_motion_path
    t <- bm$time_periods

    X <- sigma * W + (alpha - .5 * sigma ^2) * t

    S <- initial_stock_price * exp(X)
    structure(data.frame(t, S),
              names = c("time_periods", "stock_price_path")
              )
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param initial_stock_price
##' @param time_to_maturity
##' @param seed
##' @param scale
##' @param sigma
##' @param alpha
##' @return
##' @author
##' @export
sstock_ito <- function(initial_stock_price = 50,
                       time_to_maturity = 4,
                       seed = 1,
                       scale = 100,
                       sigma = 1,
                       alpha = 0){
    ## to delete:
    ## initial_stock_price <- 50
    ## time_to_maturity <- 4
    ## seed <- 1
    ## scale <- 100
    ## sigma <- 1
    ## alpha <- 0
    ##

    S0 <- initial_stock_price
    time_structure <- seq(0,
                          time_to_maturity,
                          length.out = (time_to_maturity * scale) + 1)

    ## Brownian Motion
    bw <- RandomWalk::sbmotion(time_to_maturity = time_to_maturity,
                               seed = seed,
                               scale = scale)
    bw_path <- bw$brownian_motion_path
    bw_time <- bw$time_periods

    ## Derivation
    ## From all derivation the exponential e^f(x) does not change.
    exponential <- exp(sigma * bw_path +
                           (alpha - 0.5 * sigma ^2) * bw_time)
    f_x <-sigma * S0 * exponential

    f_xx <- sigma ^2 * S0 * exponential

    f_t <- (alpha - 0.5 * sigma ^2) * S0 * exponential

    ## differential
    differential <- as.data.frame(diff(as.matrix(bw)))

    ## Prime Integration (Over a Brownian Motion)
    prime_x <- cumsum(f_x[-length(f_x)] * differential$brownian_motion_path)

    ## Second Integration (Over time)
    sec_x <- cumsum(f_xx[-length(f_xx)] * differential$time_periods)

    ## Prime Integration (over time)
    prime_t <- cumsum(f_t[-length(f_t)] * differential$time_periods)

    ## Equation
    data.frame('time_periods' = bw$time_periods,
               'stock_price_path' = c(S0,
                                      S0 + prime_x + 0.5 * sec_x + prime_t))

}
