-   [StockPriceSimulator](#stockpricesimulator)
    -   [Introduction](#introduction)
    -   [Functions provided by the
        package](#functions-provided-by-the-package)
        -   [Key functions](#key-functions)
    -   [Description of the functions as they was created and
        defined](#description-of-the-functions-as-they-was-created-and-defined)
        -   [sstock()](#sstock)

StockPriceSimulator
===================

Introduction
------------

This package provide a way to simulate a fully random stock ticker based
on theory provided by \_\_“Stochastic Calculus For Finance ii”,
Shreve“\_\_

Functions provided by the package
---------------------------------

### Key functions

-   Stock price generator for a single instance: [sstock()](sstock)

Description of the functions as they was created and defined
------------------------------------------------------------

### sstock()

#### Summary

It returns a data.frame containing the following variables:

-   time\_periods
-   stock\_price\_path

#### Arguments

<table style="width:17%;">
<colgroup>
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 5%" />
</colgroup>
<thead>
<tr class="header">
<th>Arguments</th>
<th>Default</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>time_to_maturity</td>
<td>4</td>
<td>Final time up to the Stock Price Path goes</td>
</tr>
<tr class="even">
<td>seed</td>
<td>1</td>
<td>It fixes initial value of the pseudo random number generation in order to get reproducible experiments.</td>
</tr>
<tr class="odd">
<td>scale</td>
<td>100</td>
<td>Define the partition of the time period.</td>
</tr>
<tr class="even">
<td>sigma</td>
<td>1</td>
<td></td>
</tr>
</tbody>
</table>

#### Example of Usage

    library(StockPriceSimulator)
    stock_tick <- sstock()

![](README_files/figure-markdown_strict/unnamed-chunk-2-1.png)
