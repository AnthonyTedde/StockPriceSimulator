---
output:
  md_document:
    toc: true
    toc_depth: 3
  pdf_document:
    smart: false
  html_document:
    smart: false 
---
# StockPriceSimulator




## Introduction
This package provide a way to simulate a fully random stock ticker based on
theory provided by __"Stochastic Calculus For Finance ii", Shreve"__



## Functions provided by the package
### Key functions

 * Stock price generator for a single instance: [sstock()](sstock)
 * Stock price generator for a single instance, using the Ito's formula approximation
 [sstock\_ito()](sstock\_ito)

## Description of the functions as they was created and defined
### sstock()

#### Summary

It returns a data.frame containing the following variables:

 * time_periods
 * stock_price_path

#### Arguments

| Arguments | Default | Description |
|---|---|---|
| time_to_maturity | 4 | Final time up to the Stock Price Path goes |
| seed | 1 | It fixes initial value of the pseudo random number generation in order to get reproducible experiments. |
| scale | 100 | Define the partition of the time period. |
| sigma | 1 | |



#### Example of Usage


```r
library(StockPriceSimulator)
stock_tick <- sstock()
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)
### sstock\ito()

#### Summary

It returns a data.frame containing the following variables:

 * time_periods
 * stock_price_path
 
The computed path is based on approximation given by the It�'s formula.

#### Arguments

| Arguments | Default | Description |
|---|---|---|
| time_to_maturity | 4 | Final time up to the Stock Price Path goes |
| seed | 1 | It fixes initial value of the pseudo random number generation in order to get reproducible experiments. |
| scale | 100 | Define the partition of the time period. |
| sigma | 1 | standard deviation of the stock |
| alpha | 0 | Mean trend


#### Example of Usage


```r
library(StockPriceSimulator)
## Call the path generating function from equation:
stock_tick <- sstock(scale = 1000)
## Call the path generating function from It�'s approximation
stock_tick_ito <- sstock_ito(scale = 1000)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
