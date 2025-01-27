#' Simulate a (Augmented) Dickey-Fuller Test to Assess Unit Root in a Time Series
#'
#' @description \code{sadf_test()} provides a simulation approach to assessing
#' unit root in a time series by way of the (Augmented) Dickey-Fuller test. It
#' takes a vector and performs three (Augmented) Dickey-Fuller tests (no drift,
#' no trend; drift, no trend; drift and trend) and calculates tau statistics as
#' one normally would. Rather than interpolate or approximate a  *p*-value, it
#' simulates some user-specified number of (Augmented) Dickey-Fuller tests of
#' either a known, non-stationary time series or a known, white-noise time series
#' matching the length of the time series the user provides. This allows the
#' user to make assessments of non-stationarity or stationarity by way of
#' simulation rather than approximation from received critical values by way of
#' books or tables some years out of date.
#'
#' @details
#'
#' The Dickey-Fuller and its "augmented" corollary are curious statistical
#' procedures, even if the underlying concept is straightforward. I have seen
#' various implementations of these procedures use slightly different
#' terminology to describe its procedure, though this particular implementation
#' will impose nomenclature in which the classic Dickey-Fuller procedure that
#' assumes just the AR(1) process is one in which `n_lags` is 0. The
#' addition of lags (of first differences) is what ultimately makes the
#' Dickey-Fuller procedure to be "augmented."
#'
#' The function employs the default suggested by Schwert (1989) for the number
#' of lagged first differences to include in this procedure. Schwert (1989)
#' recommends taking the length of the series and dividing it by 100 before
#' raising that number to the power of 1/4. Thereafter, multiply it by 12 and
#' round down the number to the nearest integer. There are other suggested
#' defaults you can consider. \code{adf.test} in \pkg{aTSA} takes the length of
#' the series, divides it by 100 and raises it to the power of 2/9. It
#' multiplies that by 4 and floors the result. \code{adf.test} in \pkg{tseries}
#' subtracts 1 from the length of the series before raising it to the power of
#' 1/3 (flooring that result as well). The Examples section will show you how
#' you can do this.
#'
#' This function specifies three different types of tests: 1) no drift, no trend,
#' 2) drift, no trend, and 3) drift and trend. In the language of the `lm()`
#' function, the first is `lm(y ~ ly - 1)` where `y` is the value of `y` and
#' `ly` is its first-order lag. The second test is `lm(y ~ ly)`, intuitively
#' suggesting the *y*-intercept in this equation is the "drift". The third would
#' be `lm(y ~ ly + t)` with `t` being a simple integer that increases by 1 for
#' each observation (i.e. a time-trend).
#'
#' None of this is meant to discourage the use of Fuller (1976) or its various
#' reproductions for the sake of diagnosing stationarity or non-stationary, and
#' I will confess their expertise on these matters outpaces mine. Consider the
#' justification for this function to be largely philosophical and/or
#' experimental. Why not simulate it? It's not like time or computing power are
#' huge issues anymore.
#'
#' This is always awkwardly stated, but it's a good reminder that the classic
#' Dickey-Fuller statistics are mostly intended to come back negative. That's
#' not always the case, to be clear, but it is the intended case. You assess the
#' statistic by "how negative" it is. Stationary time series will produce test
#' statistics more negative ("smaller") than those produced by non-stationary
#' time series. In a way, this makes the hypotheses implicitly one-tailed (to
#' use that language).
#'
#' This function removes missing values from the vector before calculating test
#' statistics.
#'
#' @return \code{sadf_test()} returns a list of length 3. The first element
#' in the list is a matrix of tau statistics calculated by the test. The second
#' element is a data frame of the simulated tau statistics of either a known
#' white-noise time series or three different non-stationary time series
#' (pure random walk, random walk with drift, random walk with drift and trend).
#' The third element contains some attributes about the procedure for
#' post-processing.
#'
#' @author Steven V. Miller
#'
#' @param x a vector
#' @param n_lags defaults to NULL, but must be 0 or a positive integer. This
#' argument determines the number of lagged first differences to include in the
#' estimation procedure. Recall that the test statistic (tau) is still the
#' t-statistic for the *level* value of the vector at t-1, whether the constant
#' (drift) and time trend is included or not. If this value is 0, the procedure
#' is the classic Dickey-Fuller test. If this value is greater than 0, this is
#' the "augmented" Dickey-Fuller test, so-called because it is "augmented" by
#' the number of lagged first differences to assess higher-order AR processes.
#' If no argument is specified, the default lag is Schwert's suggested lower
#' bound. The \code{lag_suggests} data provides more information about these
#' suggested lags.
#' @param n_sims the number of simulations for calculating an interval or
#' distribution of test statistics for assessing stationarity or
#' non-stationarity. Defaults to 1,000.
#' @param sim_hyp can be either "stationary" or "nonstationary". If
#' "stationary", the function runs (A)DF tests on simulated stationary
#' (pure white noise) data. This allows the user to assess
#' compatibility/plausibility of the test statistic against a distribution of
#' test statistics that are known to be pure white noise (in expectation). If
#' "nonstationary" (default), the function generates three different data sets of
#' a pure random walk, a random walk with a drift, and a random walk with a
#' drift and trend. It then runs (A)DF tests on all those. This allows the user
#' to assess the compatibility/plausibility of their test statistics with data
#' that are known to be nonstationary in some form.
#'
#' @references
#'
#' Schwert, G. William. 1989. "Tests for Unit Roots: A Monte Carlo Investigation."
#' *Journal of Business & Economic Statistics* 7(2): 147--159.
#'
#' @examples
#'
#' y <- na.omit(USDSEK[1:500,])$close # there is one missing value here. n = 499.
#'
#'
#' sadf_test(y, n_sims = 25) # Doing 25, just to make it quick
#'
#'
#' @importFrom stats embed
#' @importFrom stats lm
#' @importFrom stats resid
#' @importFrom stats rnorm
#' @importFrom stats arima.sim
#' @export
#'


sadf_test <- function(x, n_lags = NULL, n_sims = 1000, sim_hyp = "nonstationary") {

  if (!is.null(n_lags) && (n_lags %% 1 != 0 || n_lags < 0)) {
    stop("n_lags must be a positive integer.")

  }


  if(!sim_hyp %in% c("stationary", "nonstationary")) {
    stop("The 'sim_hyp' argument must be 'stationary' or 'nonstationary'.")
  }

  x <- x[is.finite(x)] # force the time series to be complete.

  n_lags <- ifelse(is.null(n_lags),
                   floor(4*(length(x)/100)^(.25)), # gonna go with Schwert's lower bound (1989)
                   n_lags)

  # Let's try it again at the top...
  # This calculates the DF/ADF stat on the actual data. ----
  nlp1 <- n_lags + 1

  diff_x <- diff(x)
  n_diff_x <- length(diff_x)

  m <- embed(diff_x, nlp1)
  d_x_t <- m[,1]
  l1_x <- x[nlp1:n_diff_x]
  time <- 1:length(d_x_t)

  if(n_lags > 0) {
    adf_diff_lags <- m[, 2:nlp1]
    M1 <- lm(d_x_t ~ l1_x - 1 + adf_diff_lags)    # no drift, no trend
    M2 <- lm(d_x_t ~ l1_x + adf_diff_lags)        # drift, no trend
    M3 <- lm(d_x_t ~ l1_x + time + adf_diff_lags) # drift and trend
  } else { # classic DF, where n_lags == 0
    M1 <- lm(d_x_t ~ l1_x - 1)    # no drift, no trend
    M2 <- lm(d_x_t ~ l1_x )       # drift, no trend
    M3 <- lm(d_x_t ~ l1_x + time) # drift and trend
  }


  Stats <- rbind(summary(M1)$coefficients[1, 3],
                 summary(M2)$coefficients[2, 3 ],
                 summary(M3)$coefficients[2, 3 ])

  # Start the Sims -----
  Sims <- data.frame()
  if(sim_hyp == "stationary") { # * if sim_hyp is stationary... ----
    for (i in 1:n_sims) {

      if(n_lags > 0) {

        fM1 <- sim_df_mod(x, ts_type = "ndnt", df_lags = n_lags, wn = TRUE)
        fM2 <- sim_df_mod(x, ts_type = "dnt", df_lags = n_lags, wn = TRUE)
        fM3 <- sim_df_mod(x, ts_type = "dt", df_lags = n_lags, wn = TRUE)

      } else {

        fM1 <- sim_df_mod(x, ts_type = "ndnt", classic_df = TRUE, wn = TRUE)
        fM2 <- sim_df_mod(x, ts_type = "dnt", classic_df = TRUE, wn = TRUE)
        fM3 <- sim_df_mod(x, ts_type = "dt", classic_df = TRUE, wn = TRUE)


      }

      fakeStats <- rbind(summary(fM1)$coefficients[1, 3],
                         summary(fM2)$coefficients[2, 3 ],
                         summary(fM3)$coefficients[2, 3 ])

      fakeStats <- data.frame(fakeStats)
      names(fakeStats) <- c("tau")
      fakeStats$sim <- i
      fakeStats$cat <- c("No Drift, No Trend", "Drift, No Trend",
                         "Drift and Trend")

      Sims <- rbind(Sims, fakeStats)

    }


  } else { # else sim_hyp is non-stationary ----

    for (i in 1:n_sims) {

      if(n_lags > 0) {

        fM1 <- sim_df_mod(x, ts_type = "ndnt", df_lags = n_lags, wn = FALSE)
        fM2 <- sim_df_mod(x, ts_type = "dnt", df_lags = n_lags, wn = FALSE)
        fM3 <- sim_df_mod(x, ts_type = "dt", df_lags = n_lags, wn = FALSE)

      } else {

        fM1 <- sim_df_mod(x, ts_type = "ndnt", classic_df = TRUE, wn = FALSE)
        fM2 <- sim_df_mod(x, ts_type = "dnt", classic_df = TRUE, wn = FALSE)
        fM3 <- sim_df_mod(x, ts_type = "dt", classic_df = TRUE, wn = FALSE)


      }

      fakeStats <- rbind(summary(fM1)$coefficients[1, 3],
                         summary(fM2)$coefficients[2, 3 ],
                         summary(fM3)$coefficients[2, 3 ])

      fakeStats <- data.frame(fakeStats)
      names(fakeStats) <- c("tau")
      fakeStats$sim <- i
      fakeStats$cat <- c("No Drift, No Trend", "Drift, No Trend",
                         "Drift and Trend")

      Sims <- rbind(Sims, fakeStats)

    }

  }

  attatt <- data.frame(lags = n_lags,
                       sim_hyp = sim_hyp,
                       n_sims = n_sims,
                       n = length(x),
                       test = "adf")

  output <- list("stats" = Stats,
                 "sims" = Sims,
                 "attributes" = attatt)

  class(output) <- c("sadf_test")
  return(output)

} # end function sadf_test()




