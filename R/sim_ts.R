#' Simulate a Time Series
#'
#' @description \code{sim_ts()} is mostly a helper function, to be used
#' internally in this package, but you can use it here to simulate a time series.
#'
#' @return \code{sim_ts()} returns a numeric vector of a simulated time series
#' that would follow the user's input.
#'
#' @examples
#'
#' set.seed(8675309) # don't want new numbers in documentation every time...
#'
#' sim_ts(25)
#'
#' sim_ts(25, b0 = 1)
#'
#' sim_ts(25, b0 = 1, bt = .05)
#'
#' @author Steven V. Miller
#'
#' @param n a numeric vector for the length of the series
#' @param b0 a numeric vector for a potential drift in the series. Defaults to 0
#' @param bt a numeric vector for a potential trend in the series. Defaults to 0.
#' @param rho a numeric vector for the simple autoregressive parameter. Defaults to 1.
#' @param white_noise = logical, defaults to FALSE. If FALSE, generates a random
#' walk. If TRUE, series is white noise.
#' @param rsd the standard deviation for a normal distribution to be simulated. Defaults to 1.
#' @export



sim_ts <- function(n, b0 = 0, bt = 0, rho = 1, white_noise = FALSE,  rsd = 1) {

  if (white_noise == FALSE) {

    y <- cumsum(rnorm(n = n, mean = 0, sd = rsd) + b0)

  } else {

    y <- rnorm(n = n) + b0

  }

  t <- 1:n
  x <- (t*bt) + (rho*y)

  return(x)

}
