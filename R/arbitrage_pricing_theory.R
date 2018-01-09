s0 <- 4
s1 <- c(2, 8)
d <- 1/2
u <- 2
r <- .25

discount_r <- 1/(1+r)

p1 <- (1 + r - d) / (u - d)
q1 <- 1 - p1

x0 <- discount_r * (p1 * s1[2] + q1 * s1[1])

# Why k isn't involved in the calculation ?
