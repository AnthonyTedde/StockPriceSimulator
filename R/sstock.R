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
#' @export bsm_ts sstock
bsm_ts <- sstock <- function(initial_stock_price = 50,
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

##'
##' This function approximates the return of a stock price driven by a unique
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
sstock_return_ito <- function(initial_stock_price = 50,
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

    # Return a stock price return according to Ito apporximation
    alpha * differential$time_periods + sigma * differential$brownian_motion_path
}


##'
##' Stock price model with jumps.
##'
##' @title Jump Diffusion Model
##'
##' @inheritParams sstock
##'
##' @return A data.frame containing 2 variables: stock_price_path and the
##'  according time_periods.
##'
##' @author Anthony Tedde
##'
##' @export mjd_ts sstock_jump
mjd_ts <- sstock_jump <- function(initial_stock_price = 50,
                        time_to_maturity = 5,
                        seed = 1,
                        scale = 365, # Daily measurement
                        sigma = .2,
                        alpha = 0,
                        lambda =  7,
                        jumps_intensity_parameters = list(mean = 0,
                                                          sd = 0.2)
                        ){
    set.seed(seed)
    ## Simply the parameters
    mu <- jumps_intensity_parameters$mean
    delta <- jumps_intensity_parameters$sd
    lognormal_mean <- exp(mu  + 1/2 * delta ^2)
    k <- lognormal_mean - 1

    ##
    ## Generate the Compounded poisson process along with the jump intensity
    ##

    ## The first step is to define the occurences of the jumps.
    ## rpois function is used to model a poison random distribution with paramter lambda * scale ^-1
    n <- rpois(time_to_maturity * scale, lambda * scale ^ -1)
    N <- c(0, cumsum(n))

    ## Next step is to define the size of each jump depending on the according distribution
    ## y ~ iid normal(mu, delta)
    jump_intensity <- do.call(what = rlnorm, args = c(n = tail(N, 1),
                                                      jumps_intensity_parameters))
    cumulative_jump_intensity <- purrr::map_dbl(N, ~ sum(log(jump_intensity[0:.x])))

    ##################################
    ## Generate the Brownian Motion ##
    ##################################

    bm <- RandomWalk::sbmotion(time_to_maturity = time_to_maturity,
                               scale = scale,
                               seed = seed)
    W <- bm$brownian_motion_path
    t <- bm$time_periods

    ######################################################
    ## Generate the non-random part of the Levy process ##
    ######################################################

    l <- (alpha - sigma ^2 / 2 - lambda * k) * t

    #########################################
    ## Construct the whole Levy process, L ##
    #########################################

    L <- l + sigma * W + cumulative_jump_intensity

    ###############################################
    ## Construct the Stock Price Jumping Process ##
    ###############################################

    data.frame(stock_price_path = initial_stock_price * exp(L),
               time = t,
               grp = as.factor(N))

}





##' @param initial_stock_price
##'
##' @param initial_volatility
##' @param time_to_maturity
##' @param seed
##' @param scale
##' @param alpha
##' @param rho
##' @param kappa
##' @param theta
##' @param sigma
##'
##' @export hsv_ts heston
hsv_ts <- heston <- function(initial_stock_price = 50,
                   initial_volatility = 0.3,
                   time_to_maturity = 5,
                   seed = 1,
                   scale = 365,
                   alpha = 0,
                   rho = 1,
                   kappa = 2,
                   theta = 0.1,
                   sigma = .1){
  ##############################################
  ## Create the correlated BM                 ##
  ## Todo: add function to package RandomWalk ##
  ##############################################

  bms <- RandomWalk::sbmotionGenerator(time_to_maturity = time_to_maturity,
                                       scale = scale,
                                       seed = seed,
                                       n = 2)

  W1 <- bms$'1'$brownian_motion_path
  W2 <- bms$'2'$brownian_motion_path

  dB1 <- diff(W1)
  B2 <- rho * W1 + sqrt( 1 - rho ^2) * W2
  dB2 <- diff(B2)

  t <- seq(0, time_to_maturity,
           length.out = time_to_maturity * scale + 1)
  dt <- diff(t)

  ##################################
  ## Create the volatility vector ##
  ##################################
  # CIR model

  # time frame
  d2 <- apply(rbind(dt,dB2), 2, as.pairlist)


  # random_volatility: vector that takes in the volatility to be applied
  # in Heston model.
  v <- Reduce(
    function(acc, x){
      acc + kappa * (theta - acc) * x$dt + sigma * x$dB2 * sqrt(acc)
    },
    d2, initial_volatility, accumulate = T)

  ##################################
  ## Create the stock price model ##
  ##################################
  # af: accumulate factor
  # VARIANCE INDEX ???

  d1 <- apply(rbind(dt, dB1, v = v[-length(v)]), 2, as.pairlist)

  s <- Reduce(
    function(af, x){
      af + alpha * af * x$dt + sqrt(x$v) * af * x$dB1
    }, d1, initial_stock_price, accumulate = T)

  structure(data.frame(time_periods = t,
                       stock_price_path = s,
                       B1 = W1,
                       B2 = B2,
                       CIR = v))
}
