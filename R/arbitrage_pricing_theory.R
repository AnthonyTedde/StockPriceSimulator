#' s0 <- 4
#' s1 <- c(2, 8)
#' d <- 1/2
#' u <- 2
#' r <- .25
#' k <- 5
#' # Create stock price path
#' #' Compute the distribution of a discrete stochastic process that represent
#' #' all the price a stock could take according to a specific up factor and up to
#' #' a certain period.
#' #'
#' #' @title Theoretical stock price path
#' #'
#' #' @param s0 Initial stock price
#' #' @param up Up factor which drive the price of the futur stock increment
#' #' @param dim Define the frame of observation
#' #'
#' #' @return matrix containing the whole distribution of the stock price motion
#' #' from time 0 up to time dim - 1
#' #' @export
#' #'
#' #' @examples
#' #' \code{tstock_discrete(s0 = 4, up = 3, dim = 10)}
#' tstock_discrete <- function(s0 = 4, up = 2, dim = 4){
#'   # To have recombining trees it is mandatory to force the down factor to be
#'   # 1/up
#'   down <- up ^-1
#'   outer(1:dim, 1:dim, function(i, j){
#'     ifelse(j >= i,  s0 * down ^(i - 1) * up^(j - i), NA_real_)
#'   })
#' }
#'
#' # Option price for a one period maturity option
#' # Create a stock motion
#' library(purrr)
#' k <- 5
#' r <- 0.25
#' u <- 2
#' d <- 1 / r
#' s <- tstock_discrete(dim = 4)
#'
#'
#' discount_r <- 1/(1+r)
#'
#' p_neutral <- (1 + r - d) / (u - d)
#' q_neutral <- 1 - p1
#'
#' v_last <- purrr::map_dbl(s[, ncol(s)], ~ max(. - k, 0))
#' outer(v_last, v_last, FUN = function(i, j){
#'   i  * p_neutral + j * q_neutral
#' })
#'
#'
#' x0 <- discount_r * (p_neutral * v1[2] + q_neutral * v1[1])
#'
#'
