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
    rw <- RandomWalk::sbmotion(time_to_maturity = time_to_maturity,
                             seed = seed,
                             scale = scale)

    W <- rw$brownian_motion_path
    t <- rw$time_periods

    X <- sigma * W + ( alpha - 0.5 * sigma ^ 2 ) * t

    S <- initial_stock_price * exp(X)
    structure(data.frame(rw[, 1], S),
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
    time_structure <- seq(0, 4, length.out = (time_to_maturity * scale) + 1)

    ## Brownian Motion
    bw <- RandomWalk::sbmotion(time_to_maturity = time_to_maturity,
                               seed = seed,
                               scale = scale)
    
    ## Derivation
    ## With the stock price model, the S(t), S'(t) and S''(t) are all the save because:
    ## S(t) = constant * e ^ x
    f <- function(i){
        S0 * exp(sigma * RandomWalk::get_values(bw, i) + (alpha - 0.5 * sigma ^2) * i)
    }
    
    ## Prime Integration (Over a Brownian Motion)
    time <- time_structure[-length(time_structure)]
    index <- 1: length(time)
    prime <- sapply(index, FUN = function(x){
        sum(sapply(1:x, FUN = function(i){
            f(time_structure[i]) * (RandomWalk::get_values(bw, time_structure[i + 1]) - RandomWalk::get_values(bw, time_structure[i]))
        }))
    })

    ## Second Integration (Over time)
    second <- sapply(index, FUN = function(x){
        sum(sapply(1:x, FUN = function(i){
            f(time_structure[i]) * (time_structure[i + 1] - time_structure[i])
        }))
    })

    ## Equation
    S0 + prime + 0.5 * second
    prime
    
}
