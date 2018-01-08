#' Sample stock ticker simulation
#'
#' \code{sstock} outputs price path for a fully random sampled stock evolution
#'
#' @param initial_stock_price Stock price at initial step
#' @param time_to_maturity Time upon which measurement is done
#' @param seed Control randmoness
#' @param scale Divided part of a unit. For instance scale of 100 will divide
#'  4 units of time in 100 parts each, that brings to a total time of
#'  400 periods of time measurement.
#' @param sigma Volatility of the stock
#' @param alpha Mean rate of return of the stock
#'
##' @return A data.frame containing 2 variables: stock_price_path and the
##'  according time_periods.
##'
#' @author Anthony Tedde
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

    # Function that models the stock price evolution provided by S:
    # S = S0 * e^X
    # According to Ito formula for stochastic process:
    #  * alpha is the instantaneous mean rate of return
    #  * sigma is the volatility (standard deviation) of the stock.
    #
    X <- sigma * W + (alpha - .5 * sigma ^2) * t

    S <- initial_stock_price * exp(X)

    # Format the data to get a data.frame with two variables:
    #  * time_periods
    #  * stock_price_path
    #
    structure(data.frame(t, S),
              names = c("time_periods", "stock_price_path"),
              class = c(class(data.frame()),
                        'theoretical_stock_price')
              )
}

##'
##' This function approximates the path of a stock price driven by a unique
##' Brownian Motion, using the Itô-Doeblin formula for Itô-processes.
##'
##' @title Stock Price Approximation using Itô
##'
##' @inheritParams sstock
##'
##' @return A data.frame containing 2 variables: stock_price_path and the
##'  according time_periods.
##'
##' @author Anthony Tedde
##'
##' @export
sstock_ito <- function(initial_stock_price = 50,
                       time_to_maturity = 4,
                       seed = 1,
                       scale = 100,
                       sigma = 1,
                       alpha = 0){

    S0 <- initial_stock_price

    # time_structure: Sequence of time from initial time (0) up to
    # time_to_maturity sliced in equal part according to the parameter _scale_ :
    #
    time_structure <- seq(0,
                          time_to_maturity,
                          length.out = (time_to_maturity * scale) + 1)

    ## Brownian Motion generation (according to parameter)
    bw <- RandomWalk::sbmotion(time_to_maturity = time_to_maturity,
                               seed = seed,
                               scale = scale)

    W <- bw$brownian_motion_path
    t <- bw$time_periods

    ## Comupte the differential of path and time of the brownian motion:
    differential <- as.data.frame(diff(as.matrix(bw)))

    ## Derivative of f(t, x)
    ##  * f_x: First derivative of f with respect to x
    ##  * f_xx: Second derivative of f with respect to x^2
    ##  * f_t: First derivative of f with respect to t
    ##
    exponential <- exp(sigma * W +
                           (alpha - 0.5 * sigma ^2) * t)

    f_x <-sigma * S0 * exponential

    f_xx <- sigma ^2 * S0 * exponential

    f_t <- (alpha - 0.5 * sigma ^2) * S0 * exponential

    ## Prime Integration cumulative matrix (with respect to Brownian Motion)
    prime_x <- cumsum(f_x[-length(f_x)] * differential$brownian_motion_path)

    ## Second Integration cumulative matrix (with respect to Brownian Motion)
    sec_x <- cumsum(f_xx[-length(f_xx)] * differential$time_periods)

    ## Prime Integration cumulative matrix (with respect to time)
    prime_t <- cumsum(f_t[-length(f_t)] * differential$time_periods)

    ## Equation
    data.frame('time_periods' = bw$time_periods,
               'stock_price_path' = c(S0,
                                      S0 + prime_x + 0.5 * sec_x + prime_t))
}
