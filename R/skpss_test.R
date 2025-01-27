#' Simulate a KPSS Test to Assess Unit Root in a Time Series
#'
#' @description \code{skpss_test()} provides a simulation approach to assessing
#' unit root in a time series by way of KPSS test, first proposed by Kwiatkowski
#' et al. (1992). It takes a vector and extracts the residuals from two models to
#' assess stationarity around a level or trend, producing a KPSS test statistic
#' (eta). Rather than interpolate or approximate a *p*-value, it simulates
#' some user-specified number of KPSS of either a known, stationary time series
#' (default) or a known, non-stationary time series matching the length of the
#' time series the user provides. This allows the user to make assessments of
#' non-stationarity or stationarity by way of simulation rather than
#' approximation from received critical values by way of various books/tables.
#'
#' @details
#'
#' Recall that this procedure defaults from almost every other unit root test
#' by making stationarity a null hypothesis. Non-stationarity is the alternative
#' hypothesis.
#'
#' As of writing, the default lags are those suggested by Schwert (1989) to
#' apply to the Bartlett kernel generating the KPSS test statistic (eta).
#'
#' \pkg{aTSA} has a particularly interesting approach to this test that draws on
#' on insights seemingly proposed by Hobijn et al. (2004). Future updates may
#' include those, but this function, as is, performs calculations of eta
#' identical to what \pkg{tseries} or \pkg{urca} would produce. Right now,
#' I don't have the time or energy to get into the weeds of what Hobijn et al.
#' (2004) are doing or what \pkg{aTSA} is implementing, but it seems pretty cool.
#'
#' This function removes missing values from the vector before calculating test
#' statistics.
#'
#' @return
#'
#' \code{skpss_test()} returns a list of length 3. The first element
#' in the list is a matrix of eta statistics calculated by the test. The first
#' of those is the level statistic and the second of those is the trend
#' statistic. The second element is a data frame of the simulated eta statistics,
#' where the type of simulation (level, trend) is communicated in the `cat` column.
#' The third element contains some attributes about the procedure for
#' post-processing.
#'
#' @author Steven V. Miller
#'
#' @param x a vector
#' @param lag_short logical, defaults to \code{TRUE}. If \code{TRUE}, the
#' "short-term" lag is used for the KPSS test. If \code{FALSE}, the
#' "long-term" lag is used. These lags are those suggested by Schwert (1989).
#' @param n_sims the number of simulations for calculating an interval or
#' distribution of test statistics for assessing stationarity or
#' non-stationarity. Defaults to 1,000.
#' @param sim_hyp can be either "stationary" or "nonstationary". If
#' "stationary" (default), the function runs KPSS tests on simulated stationary
#' (pure white noise) data. This allows the user to assess
#' compatibility/plausibility of the test statistic against a distribution of
#' test statistics that are known to be pure white noise (in expectation). If
#' "nonstationary", the simulations are conducted on two different random walks.
#' The "trend" test includes a level drawn from a Rademacher distribution with
#' a time trend of that level, divided by 10.
#'
#' @references
#'
#' Hobijn, Bart, Philip Hans Franses, and Marius Ooms. 2004. "Generalizations of
#' the KPSS-test for Stationarity". *Statistica Neerlandica* 58(4): 483--502.
#'
#' Kwiatkowski, Denis, Peter C.B. Phillips, Peter Schmidt, and Yongcheol Shin.
#' 1992. "Testing the Null Hypothesis of Stationarity Against the Alternative
#' of a Unit Root: How Sure Are We that Economic Time Series Have a Unit Root?"
#' *Journal of Econometrics* 54(1-3): 159--78.
#'
#' @examples
#'
#' x <- USDSEK$close[1:500] # note: one missing obs here; becomes series of 499
#'
#' skpss_test(x, n_sims = 25) # make it quick...
#'
#'
#' @importFrom stats embed
#' @importFrom stats lm
#' @importFrom stats resid
#' @importFrom stats rnorm
#' @export
#'


skpss_test <- function(x, lag_short = TRUE, n_sims = 1000, sim_hyp = "stationary") {


  if(!sim_hyp %in% c("stationary", "nonstationary")) {
    stop("The 'sim_hyp' argument must be 'stationary' or 'nonstationary'.")
  }

  x <- x[is.finite(x)] # force the time series to be complete.

  n <- length(x)      # length of series
  tvar <- 1:length(x) # time var

  # We'll do both the level and the trend. Should be easy.

  resLev <- x - mean(x)
  # FYI: the above is equivalent to resid(lm(x ~ 1))
  resTre <- resid(lm(x ~ tvar))

  # I could just copy-paste, do it twice, but let's try it this way...

  if(lag_short == TRUE) {
    q <- floor(4*(n/100)^0.25)
  } else {
    q <-  floor(12*(n/100)^0.25)
  }

  calc_kpss <- function(resids, q) {

    n_resids <- length(resids)

    S <- cumsum(resids)
    nom <- sum(S^2)/n_resids^2
    s2 <- sum(resids^2)/n_resids

    index <- 1:q
    rips <- sapply(index, function(x) t(resids[-c(1:x)]) %*% resids[-c((n_resids-x+1):n_resids)])

    bart <- 1 - (index/(q + 1))
    denom <- s2 + 2/n_resids * (t(bart) %*% rips)

    eta <- as.vector(nom/denom)
    eta
    return(eta)

  }

  Stats <- rbind(calc_kpss(resLev, q),
                 calc_kpss(resTre, q))



  # * Simulations ----
  Sims <- data.frame()

  if(sim_hyp == "stationary") { # * If sim_hyp == "stationary ----

    for (i in 1:n_sims) {

      fake_tvar <- 1:n
      fake_x <- sim_ts(n, b0 = 0, bt = 0, white_noise = TRUE)

      fake_resLev <- fake_x - mean(fake_x)
      fake_resTre <- resid(lm(fake_x ~ fake_tvar))



      fakeStats <- rbind(calc_kpss(fake_resLev, q),
                         calc_kpss(fake_resTre, q))

      fakeStats <- data.frame(fakeStats)
      names(fakeStats) <- c("eta")
      fakeStats$sim <- i
      fakeStats$cat <- c("Level", "Trend")

      Sims <- rbind(Sims, fakeStats)


    }
  } else { # * else if sim_hyp == "non-stationary" ----
    for (i in 1:n_sims) {

      fake_tvar <- 1:n

      fake_lev <- sim_ts(n, b0 = 0, bt = 0, white_noise = FALSE)

      fake_b0 <- 2 * stats::rbinom(n = 1, size = 1, prob = 0.5) - 1
      fake_bt <- fake_b0/10

      fake_tre <- sim_ts(n, b0 = fake_b0, bt = fake_bt, white_noise = FALSE)

      fake_resLev <- fake_lev - mean(fake_lev)
      fake_resTre <- resid(lm(fake_tre ~ fake_tvar))

      fakeStats <- rbind(calc_kpss(fake_resLev, q),
                         calc_kpss(fake_resTre, q))



      fakeStats <- data.frame(fakeStats)
      names(fakeStats) <- c("eta")
      fakeStats$sim <- i
      fakeStats$cat <- c("Level", "Trend")

      Sims <- rbind(Sims, fakeStats)




    }

  }

  attatt <- data.frame(lags = q,
                       sim_hyp = sim_hyp,
                       n_sims = n_sims,
                       n = length(x),
                       test = "kpss")

  output <- list("stats" = Stats,
                 "sims" = Sims,
                 "attributes" = attatt)

  class(output) <- c("skpss_test")
  return(output)


}


