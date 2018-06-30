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
integrate(heston_ch, 0, Inf)

########################
## HESTON: Call price ##
########################
fun <- function(
pi1 <- 1/2 + 1/pi

## Local Variables:
## ess-r-package--project-cache: (StockPriceSimulator . "~/workspace/thesis/StockPriceSimulator/")
## End:



call_heston <- function(initial_stock_price = 50,
                        initial_volatility,
                        theta,
                        kappa,
                        sigma,
                        alpha,
                        rho,
                        time_to_maturity,
                        K)

  initial_stock_price = 1
initial_volatility = .16
theta = 0.16
kappa = 1
sigma = 2
alpha = 0
rho = -.8
time_to_maturity = 10
K = 2

call_heston(initial_stock_price = 1,
            initial_volatility = .16,
            theta = 0.16,
            kappa = 1,
            sigma = 2,
            alpha = .0,
            rho = -.8,
            time_to_maturity = 10,
            K = 2)


###############################################################################
# calibration
###############################################################################
load(file="option-quote.RData")

# 1.1. All the strike price available for all the maturity
avail_stikes <- map(AAPL_o, `$`, calls) %>%
  map(`$`, Strike) %>%
  unlist %>% unname %>% unique %>% sort

# 1.2 Covered maturities
maturity <- names(AAPL_o)

# 1.3 Construct the price surface
dfs_call <- map(AAPL_o, `$`, call) %>%
  map(~.x[, 1:2]) %>%
  Filter(f = Negate(is.null))

price_surface <- dfs_call[[1]]
for(i in names(dfs_call[-1])){
  price_surface <- merge(price_surface, dfs_call[[i]], by = "Strike", all = T)
}
colnames(price_surface) <- c("Strike", names(dfs_call))
# First subsetting of the price surface
price_surface <- structure(price_surface[23:57, -4])
rownames(price_surface) <- 1:nrow(price_surface)

# 1.4 Convert the maturity date based on the price surface
maturity <- names(price_surface)[-1] %>%
  as.Date(format = "%b.%d.%Y") - as.Date("2018-05-18")




