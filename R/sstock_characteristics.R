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

##' @export call_heston hsv_call
hsv_call <- call_heston <- function(initial_stock_price = 50,
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

  pi1 <- 1/2 + 1/pi * integrate(p1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
  pi2 <- 1/2 + 1/pi * integrate(p2, 0, Inf, subdivisions=2000, stop.on.error = F)$value

  initial_stock_price * pi1 - exp(-alpha * time_to_maturity) * K * pi2
}


##' @export delta_heston hsv_delta
hsv_delta <- delta_heston <- function(S = heston()$stock_price_path,
                        V = heston()$CIR,
                        theta,
                        kappa,
                        sigma,
                        alpha,
                        rho,
                        time_to_maturity,
                        K){

  remaining.time <- seq(time_to_maturity, 0, length.out = length(S))
  # To get the last price computed at the very last minute:
  remaining.time <- c(remaining.time[-length(remaining.time)], (1 / (365 * 24)))

  hc <- purrr::pmap(list(S, V, remaining.time),
                    ~ heston_characteristic(..1,
                                            ..2,
                                            theta,
                                            kappa,
                                            sigma,
                                            alpha,
                                            rho,
                                            ..3))

  return <- pmap_dbl(list(hc, S, remaining.time), .f = function(hc, s, t){
    p1 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w -1i)
      Re(a / (1i * w * hc(-1i)))
    }


    f1 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w -1i)
      Re(a / (hc(-1i) * s))
    }

    f2 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w)
      Re(a / s)
    }

    pi1 <- 1/2 + 1/pi * integrate(p1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
    fi1 <- 1 / pi * integrate(f1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
    fi2 <- 1 / pi * integrate(f2, 0, Inf, subdivisions=2000, stop.on.error = F)$value

   pi1 + s * fi1 - exp(-alpha * t) * K * fi2
    # pi1 + s * fi1 - s * fi2
  })

  return[length(return)] <- max(0, round(return[length(return)]))
  return(return)
}


##' @export hsv_gamma gamma_heston
hsv_gamma <- gamma_heston <- function(S = heston()$stock_price_path,
                         V = heston()$CIR,
                         theta,
                         kappa,
                         sigma,
                         alpha,
                         rho,
                         time_to_maturity,
                         K){

  remaining.time <- seq(time_to_maturity, 0, length.out = length(S))
  # To get the last price computed at the very last minute:
  remaining.time <- c(remaining.time[-length(remaining.time)], (1 / (365 * 24)))

  hc <- purrr::pmap(list(S, V, remaining.time),
                    ~ heston_characteristic(..1,
                                            ..2,
                                            theta,
                                            kappa,
                                            sigma,
                                            alpha,
                                            rho,
                                            ..3))

  return <- pmap_dbl(list(hc, S, remaining.time), .f = function(hc, s, t){

    f_d1pi1 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w -1i)
      Re(a / (hc(-1i) * s))
    }

    f_d2pi1 <- function(w){
      a <- exp( -1i * w * log(K)) * hc(w -1i) * (1i * w - 1)
      Re(a / (hc(-1i) * s ^2))

    }

    f_d2pi2 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w) * (1i * w - 1)
      Re(a / s ^2)
    }

    d1pi1 <- 1/pi * integrate(f_d1pi1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
    d2pi1 <- 1/pi * integrate(f_d2pi1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
    d2pi2 <- 1/pi * integrate(f_d2pi2, 0, Inf, subdivisions=2000, stop.on.error = F)$value

    2 * d1pi1 + s * d2pi1 - exp(-alpha * t) * K * d2pi2
  })

  # return[length(return)] <- max(0, round(return[length(return)]))
  return(return)
}


##' @export
hsv_theta <- function(S = heston()$stock_price_path,
                                      V = heston()$CIR,
                                      theta,
                                      kappa,
                                      sigma,
                                      alpha,
                                      rho,
                                      time_to_maturity,
                                      K){}


##' @export
merton_characteristic <- function(initial_stock_price = 50,
                                  time_to_maturity = 5,
                                  seed = 1,
                                  scale = 365, # Daily measurement
                                  sigma = .2,
                                  alpha = 0,
                                  lambda =  7,
                                  jumps_intensity_parameters = list(mean = 0,
                                                                    sd = 0.2)){
  m <- jumps_intensity_parameters$mean
  d <- jumps_intensity_parameters$sd
  s <- initial_stock_price
  t <- time_to_maturity
  k <- exp(m + d ^2  / 2) - 1
  function(w){
     a <- lambda * t * (exp( 1i * m * w - d ^ 2 * w ^ 2 / 2) - 1)
     b <- 1i * w * (log(s) + (alpha - sigma ^2 / 2 - lambda * k)* t)
     c <- sigma ^2 * w ^2 / 2 * t
     exp(a + b - c)
  }
}

##' @export mjd_call call_merton
mjd_call <- call_merton <- function(initial_stock_price = 50,
                        time_to_maturity = 5,
                        seed = 1,
                        scale = 365, # Daily measurement
                        sigma = .2,
                        alpha = 0,
                        lambda =  7,
                        jumps_intensity_parameters = list(mean = 0,
                                                          sd = 0.2),
                        K){

  mc <- merton_characteristic(initial_stock_price = initial_stock_price,
                              time_to_maturity = time_to_maturity,
                              seed = 1,
                              scale = 365, # Daily measurement
                              sigma = sigma,
                              alpha = alpha,
                              lambda =  lambda,
                              jumps_intensity_parameters = jumps_intensity_parameters)
  p1 <- function(w){
    a <- exp(-1i * w * log(K)) * mc(w -1i)
    Re(a / (1i * w * mc(-1i)))
  }

  p2 <- function(w){
    a <- exp(-1i * w * log(K)) * mc(w)
    Re(a / (1i * w))
  }

  pi1 <- 1/2 + 1/pi * integrate(p1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
  pi2 <- 1/2 + 1/pi * integrate(p2, 0, Inf, subdivisions=2000, stop.on.error = F)$value

  initial_stock_price * pi1 - exp(-alpha * time_to_maturity) * K * pi2
}


##' @export delta_merton mjd_delta
mjd_delta <- delta_merton <- function(S = sstock_jump()$stock_price_path,
                         time_to_maturity,
                         K,
                         sigma = .2,
                         alpha = 0,
                         lambda = 7,
                         jumps_intensity_parameters  = list(mean = 0,
                                                            sd = 0.2)){

  remaining.time <- seq(time_to_maturity, 0, length.out = length(S))
  # To get the last price computed at the very last minute:
  remaining.time <- c(remaining.time[-length(remaining.time)], (1 / (365 * 24)))

  hc <- purrr::pmap(list(S, remaining.time),
                    ~ merton_characteristic(..1,
                                            ..2,
                                            sigma = sigma,
                                            alpha = alpha,
                                            lambda = lambda,
                                            jumps_intensity_parameters = jumps_intensity_parameters))

  return <- pmap_dbl(list(hc, S, remaining.time), .f = function(hc, s, t){
    p1 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w -1i)
      Re(a / (1i * w * hc(-1i)))
    }


    f1 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w -1i)
      Re(a / (hc(-1i) * s))
    }

    f2 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w)
      Re(a / s)
    }

    pi1 <- 1/2 + 1/pi * integrate(p1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
    fi1 <- 1 / pi * integrate(f1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
    fi2 <- 1 / pi * integrate(f2, 0, Inf, subdivisions=2000, stop.on.error = F)$value

    pi1 + s * fi1 - exp(-alpha * t) * K * fi2
    # pi1 + s * fi1 - s * fi2
  })

  return[length(return)] <- max(0, round(return[length(return)]))
  return(return)
}

##' @export
mjd_gamma <- function(S = sstock_jump()$stock_price_path,
                      time_to_maturity,
                      K,
                      sigma = .2,
                      alpha = 0,
                      lambda = 7,
                      jumps_intensity_parameters  = list(mean = 0,
                                                         sd = 0.2)){

  remaining.time <- seq(time_to_maturity, 0, length.out = length(S))
  # To get the last price computed at the very last minute:
  remaining.time <- c(remaining.time[-length(remaining.time)], (1 / (365 * 24)))

  hc <- purrr::pmap(list(S, remaining.time),
              ~ merton_characteristic(..1,
                                      ..2,
                                      sigma = sigma,
                                      alpha = alpha,
                                      lambda = lambda,
                                      jumps_intensity_parameters = jumps_intensity_parameters))

  return <- purrr::pmap_dbl(list(hc, S, remaining.time), .f = function(hc, s, t){

    f_d1pi1 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w -1i)
      Re(a / (hc(-1i) * s))
    }

    f_d2pi1 <- function(w){
      a <- exp( -1i * w * log(K)) * hc(w -1i) * (1i * w - 1)
      Re(a / (hc(-1i) * s ^2))

    }

    f_d2pi2 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w) * (1i * w - 1)
      Re(a / s ^2)
    }

    d1pi1 <- 1/pi * integrate(f_d1pi1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
    d2pi1 <- 1/pi * integrate(f_d2pi1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
    d2pi2 <- 1/pi * integrate(f_d2pi2, 0, Inf, subdivisions=2000, stop.on.error = F)$value

    2 * d1pi1 + s * d2pi1 - exp(-alpha * t) * K * d2pi2
  })

  return(return)
}

##' @export
mjd_theta <- function(S = sstock_jump()$stock_price_path,
                      time_to_maturity,
                      K,
                      sigma = .2,
                      alpha = 0,
                      lambda = 7,
                      jumps_intensity_parameters  = list(mean = 0,
                                                         sd = 0.2)){


}
