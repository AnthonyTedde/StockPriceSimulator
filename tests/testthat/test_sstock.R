library(testthat)
library(StockPriceSimulator)

test_that("sstock return a dataframe", {
  expect_equal(class(sstock()), class(data.frame()))
})

test_that("sstock names are correct", {
  expect_equal(names(sstock()), c('time_periods', 'stock_price_path'))
})

# The following test emerge from the theory of Stochatic Calculus for finance ii
# page 107, formula 3.4.16:
#
#     1        m-1        S(t_j+1)   2        2
# ---------- * SUM (log ------------)  = sigma
#  T2 - T1     j=0         S(t_j)
#
test_that("Equality between log return formula and volatility (formula 3.4.16)", {
  si = 9
  S <- sstock(time_to_maturity = 4,
              scale = 1000,
              sigma = si)
  T2 <- 4
  T1 <- 0
  si_squared <- sum(sapply(1:(nrow(S) - 1),
                           function(j)
                             log(S[j + 1, 2] / S[j, 2]) ^ 2)) * (T2 - T1)^-1
  expect_equal(si ^ 2, si_squared, tolerance = 1e-1)
})
