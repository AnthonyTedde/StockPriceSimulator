library(testthat)
library(StockPriceSimulator)

test_that("sstock return a dataframe", {
  expect_equal(class(sstock()), class(data.frame()))
})

test_that("sstock names are correct", {
  expect_equal(names(sstock()), c('time_periods', 'stock_price_path'))
})
