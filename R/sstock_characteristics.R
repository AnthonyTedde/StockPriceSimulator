##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param initial_stock_price
##' @param initial_volatility
##' @param theta
##' @param kappa
##' @param sigma
##' @param alpha
##' @param rho
##' @param time_to_maturity
##' @param w
##' @return
##' The characteristic function of the Heston Stochastic
##' Volatility is returned
##' @author Anthony Tedde
##' @export
heston_characteristic <- function(initial_stock_price = 50,
                                  initial_volatility,
                                  theta,
                                  kappa,
                                  sigma,
                                  alpha,
                                  rho,
                                  time_to_maturity){
    v0 <- initial_volatility
    function(w){

        ## Intermediary computation. Stage 1
        # a <- complex(real = -w ^2 / 2,
        #              imaginary = -w / 2)
        a <- -w ^ 2 / 2 - 1i * w / 2

        # beta <- kappa - complex(imaginary = rho * sigma * w)
        beta <- kappa - 1i * rho * sigma * w
        gamma <- sigma ^2 / 2

        ## 2
        h <- sqrt(beta ^2 - 4 * a * gamma)

        ## 3
        rplus <- (beta + h) / sigma ^2
        rmin <- (beta - h) / sigma ^2

        ## 4
        g <- rmin / rplus

        ## 5

        d <- rmin * (1 - exp(-h * time_to_maturity))/ (1 - g * exp(-h * time_to_maturity))
        c <- kappa * (rmin * time_to_maturity - 2 / sigma ^2 * log((1 - g * exp(-h * time_to_maturity))/(1 - g)))

        ## 6
        # exp(c * theta + d * initial_volatility + complex(imaginary = w * log(initial_stock_price * exp(alpha * time_to_maturity))))

        exp(c * theta + d * initial_volatility + 1i * w * log(initial_stock_price * exp(alpha * time_to_maturity)))
    }

}

##' @export
call_heston <- function(initial_stock_price = 50,
                        initial_volatility,
                        theta,
                        kappa,
                        sigma,
                        alpha,
                        rho,
                        time_to_maturity,
                        K){

  hc <- heston_characteristic(initial_stock_price,
                              initial_volatility,
                              theta,
                              kappa,
                              sigma,
                              alpha,
                              rho,
                              time_to_maturity)
  p1 <- function(w){
    a <- exp(-1i * w * log(K)) * hc(w -1i)
    Re(a / (1i * w * hc(-1i)))
  }

  p2 <- function(w){
    a <- exp(-1i * w * log(K)) * hc(w)
    Re(a / (1i * w))
  }

  pi1 <- 1/2 + 1/pi * integrate(p1, 0, Inf)$value
  pi2 <- 1/2 + 1/pi * integrate(p2, 0, Inf)$value

  initial_stock_price * pi1 - exp(-alpha * time_to_maturity) * K * pi2
}





















