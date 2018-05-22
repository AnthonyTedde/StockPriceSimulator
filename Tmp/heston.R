############################### BEGIN OF FUNCTION ##############################

###############
## Arguments ##
###############

initial_stock_price = 50
initial_volatility = 0.3
time_to_maturity = 1
seed = 1
scale = 365 # Daily measuremen
alpha = 0
rho = -.7
kappa = 0
theta = 0
sigma = .0


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
d2 <- apply(rbind(dt,dB2),2,as.pairlist)


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

################################### END OF FUNCTION ############################

####################################
## Plot the stock price evolution ##
####################################

S <- data.frame(t = t, s = s)
library(ggplot2)
ggplot(S, aes(x = t, y = s)) +
    geom_line()



#####################################
## HESTON: Characteristic function ##
#####################################

initial_stock_price = 50
initial_volatility = .2
theta = .2
kappa = 1
sigma = .3
alpha = .5
rho = .8
time_to_maturity = 2
w = 1

## 1
a <- complex(real = -w ^2 / 2, imaginary = -w / 2)
beta <- kappa - complex(imaginary = rho * sigma * w)
gamma <- sigma ^2 / 2

## 2
h <- sqrt(beta ^2 - 4 * a * gamma)

## 3
rplus <- (beta + h) / sigma ^2
rmin <- (beta - h) / sigma ^2

## 4
g <- rmin / rplus

## 5
d <- rmin * (1 - exp(-h * time_to_maturity)) / (1 - g * exp(-h * time_to_maturity))
c <- kappa * (rmin * time_to_maturity - 2 / sigma ^2 * log((1 - g * exp(-h * time_to_maturity))/(1 - g))))

## 6
psi <- exp(c * theta + d * initial_volatility + complex(imaginary = w * log(initial_stock_price * exp(alpha * time_to_maturity))))

## TEST THE FUNCTION
heston_ch <- heston_characteristic(initial_stock_price = 50,
                                   initial_volatility = .20,
                                   theta = .20,
                                   kappa = 1,
                                   sigma = .1,
                                   alpha = .30,
                                   rho = -.4,
                                   time_to_maturity = 1)

function(w){
    ## etcetera...
}
integral(function(wheston_ch, -Inf, Inf)

########################
## HESTON: Call price ##
########################
fun <- function(
pi1 <- 1/2 + 1/pi 

## Local Variables:
## ess-r-package--project-cache: (StockPriceSimulator . "~/workspace/thesis/StockPriceSimulator/")
## End:
