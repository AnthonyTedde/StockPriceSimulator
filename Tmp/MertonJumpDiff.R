library(ggplot2)
library(RandomWalk)
initial_stock_price = 50
time_to_maturity = 5
seed = 1
scale = 365 # Daily measurement
sigma = .2
alpha = 0
lambda =  7
jumps_intensity_parameters = list(mean = 0,
                                  sd = 0.2)

## Simply the parameters
mu <- jumps_intensity_parameters$mean
delta <- jumps_intensity_parameters$sd
lognormal_mean <- exp(mu  + 1/2 * delta ^2)
lognormal_sd <- exp(2 * mu + delta ^2) * (exp(delta ^2) - 1)
k <- lognormal_mean - 1

###########################################################################
## Generate the Compounded poisson process along with the jump intensity ##
###########################################################################

## The first step is to define the occurences of the jumps.
## rpois function is used to model a poison random distribution with paramter lambda * scale ^-1
n <- rpois(time_to_maturity * scale, lambda * scale ^ -1)
N <- c(0, cumsum(n))

## Next step is to define the size of each jump depending on the according distribution
## y ~ iid normal(mu, delta)
jump_intensity <- do.call(what = rlnorm, args = c(n = tail(N, 1), jumps_intensity_parameters))
cumulative_jump_intensity <- purrr::map_dbl(N, ~ sum(log(jump_intensity[0:.x])))

##################################
## Generate the Brownian Motion ##
##################################

bm <- sbmotion(time_to_maturity = time_to_maturity,
               scale = scale)
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

S <- data.frame(stock_price_path = initial_stock_price * exp(L),
                time = t,
                grp = as.factor(N))

#########################
## further computation ##
#########################

## Check that ln(y(t)) has normal law with appropriate mean and variance
y <- 

ggplot(S, aes(x = time, y = stock_price_path)) +
    geom_line(aes(group = grp))

## Number of jumps
length(levels(S$grp))
                                 
