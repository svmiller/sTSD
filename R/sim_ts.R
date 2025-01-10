#' Simulate a Time Series
#'
#' @description \code{sim_ts()} is mostly a helper function, to be used
#' internally in this package, but you can use it here to simulate a time series.
#'
#' @details This function makes ample use of the "attributes" element in the
#' list produced by the unit root simulations.
#'
#' @return \code{sim_series()} returns a numeric vector of a simulated time series
#' that would follow the user's input.
#'
#' @examples
#'
#' set.seed(8675309) # don't want new numbers in documentation every time...
#'
#' sim_ts(25)
#'
#' sim_ts(25, 1)
#'
#' sim_ts(25, 1, 1)
#'
#' @author Steven V. Miller
#'
#' @param n a numeric vector for the length of the series
#' @param b0 a numeric vector for a potential drift in the series. Defaults to 0
#' @param bt a numeric vector for a potential trend in the series. Defaults to 0.
#' @param white_noise = logical, defaults to FALSE. If FALSE, generates a random
#' walk. If TRUE, series is white noise.
#' @export


sim_ts <- function(n, b0 = 0, bt = 0, white_noise = FALSE) {

  if (white_noise == FALSE) {

    y <- cumsum(rnorm(n = n))

  } else {

    y <- rnorm(n = n)

  }

  t <- 1:n
  x <- b0 + t*bt + y

  return(x)

}
