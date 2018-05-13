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
