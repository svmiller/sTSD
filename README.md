
# `sTSD`: Simulate Time Series Diagnostics

[![](https://www.r-pkg.org/badges/version/sTSD?color=blue)](https://cran.r-project.org/package=sTSD)
[![](http://cranlogs.r-pkg.org/badges/grand-total/sTSD?color=blue)](https://cran.r-project.org/package=sTSD)
[![](http://cranlogs.r-pkg.org/badges/last-month/sTSD?color=blue)](https://cran.r-project.org/package=sTSD)
[![](http://cranlogs.r-pkg.org/badges/last-week/sTSD?color=blue)](https://cran.r-project.org/package=sTSD)

<img src="http://svmiller.com/images/sTSD-hexlogo.png" alt="My sTSD  hexlogo" align="right" width="200" style="padding: 0 15px; float: right;"/>

`{sTSD}` provides a suite of functions for the analyses of time series,
with an initial focus on diagnostic tests for unit root. Its primary aim
concerns the simulation of critical values that are almost always
approximated or interpolated by reference to tables of critical values
passed down from decades-old texts. While there is nothing necessarily
wrong with the received wisdom of critical values generated decades ago,
simulation provides its own perks. Not only is simulation broadly
informative as to what these various test statistics do and what are
their plausible values, simulation provides more flexibility for
assessing unit root by way of different thresholds or different
hypothesized distributions.

## Installation

This package is now on CRAN. You can download it as you would any other
R package.

``` r
install.packages("sTSD")
```

You can also install the development version of `{sTSD}` from Github via
the `{devtools}` package. I suppose using the `{remotes}` package would
work as well.

``` r
devtools::install_github("svmiller/sTSD")
```
