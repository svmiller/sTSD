#' Simulate a Phillips-Perron Test to Assess Unit Root in a Time Series
#'
#' @description \code{spp_test()} provides a simulation approach to assessing
#' unit root in a time series by way of the Phillips-Perron test. It takes a
#' vector and performs three Phillips-Perron tests (no drift, no trend; drift, no
#' trend; drift and trend) and calculates both rho and tau statistics as one
#' normally would. Rather than interpolate or approximate a *p*-value, it
#' simulates some user-specified number of Phillips-Perron tests of a known,
#' white-noise time series matching the length of the time series the user
#' provides. This allows the user to make assessments of non-stationarity or
#' stationarity by way of simulation rather than approximation from received
#' critical values by way of books or tables some years out of date.
#'
#' @details Some knowledge of Augmented Dickey-Fuller and the Phillips-Perron
#' procedure is assumed here. Generally, the Phillips-Perron test purports to
#' build on the Augmented Dickey-Fuller procedure through two primary means. The
#' first is relaxing the need to specify or assume lag structures ad hoc or ex
#' ante. Only a short-term lag or long-term lag are necessary. The second is
#' that its robust to various forms of heteroskedasticity in the error term.
#'
#' The short-term and long-term lags follow the convention introduced in the
#' Phillips-Perron test. The short-term lag uses the default number of
#' Newey-West lags, defined as the floor of 4*(n/100)^.25 where `n` is the length
#' of the time series. The long-term lag substitutes 4 for 12 in this equation.
#'
#' This function specifies three different types of tests: 1) no drift, no trend,
#' 2) drift, no trend, and 3) drift and trend. In the language of the `lm()`
#' function, the first is `lm(y ~ ly - 1)` where `y` is the value of `y` and
#' `ly` is its first-order lag. The second test is `lm(y ~ ly)`, intuitively
#' suggesting the *y*-intercept in this equation is the "drift". The third would
#' be `lm(y ~ ly + t)` with `t` being a simple integer that increases by 1 for
#' each observation (i.e. a time-trend).
#'
#' There are two types of statistics in the Phillips-Perron test: rho and tau.
#' Of the two, tau is the more intuitive statistic and compares favorably to its
#' corollary statistic in the Augmented Dickey-Fuller test. It's why you'll
#' typically see tau reported as the statistic of interest in other
#' implementations. rho has its utility for more advanced diagnostics, though.
#' Both are calculated in this function, though tau is the default statistic.
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
#' @return \code{spp_test()} returns a list of length 3. The first element
#' in the list is a matrix of rho statistics and tau statistics calculated by
#' the Phillips-Perron test. The second element is a data frame of the simulated
#' rho and tau statistics of either a known white-noise time series or three
#' different non-stationary time series (pure random walk, random walk with
#' drift, random walk with drift and trend). The third element is some
#' attributes about the procedure for post-processing.
#'
#' @author Steven V. Miller
#'
#' @param x a vector
#' @param lag_short logical, defaults to \code{TRUE}. If \code{TRUE}, the
#' "short-term" lag is used for the Phillips-Perron test. If \code{FALSE}, the
#' "long-term" lag is used.
#' @param n_sims the number of simulations for calculating an interval or
#' distribution of test statistics of a white-noise time series. Defaults to
#' 1,000.
#' @param sim_hyp can be either "stationary" or "nonstationary". If
#' "stationary" (the default), the function runs Phillips-Perron tests on
#' simulated stationary (pure white noise) data. This allows the user to assess
#' compatibility/plausibility of the test statistic against a distribution of
#' test statistics that are known to be pure white noise (in expectation). If
#' "nonstationary", the function generates three different data sets of a pure
#' random walk, a random walk with a drift, and a random walk with a drift and
#' trend. It then runs Phillips-Perron tests on all those. This allows the user
#' to assess the compatibility/plausibility of their test statistics with data
#' that are known to be nonstationary in some form.
#'
#' @examples
#'
#' a <- rnorm(25) # white noise
#' b <- cumsum(a) # random walk
#'
#' spp_test(a, n_sims = 25)
#' spp_test(b, n_sims = 25)
#'
#' @importFrom stats embed
#' @importFrom stats lm
#' @importFrom stats resid
#' @importFrom stats rnorm
#' @importFrom stats arima.sim
#' @export

spp_test <- function(x, lag_short = TRUE, n_sims = 1000,
                     sim_hyp = "nonstationary") {

  # if (!pp_stat %in% c("tau", "rho") | length(pp_stat) > 1) {
  #   stop("The only 'pp_stat' arguments that make sense in this context is 'tau' or 'rho'. Pick one of the two.")
  # }

  if(!sim_hyp %in% c("stationary", "nonstationary")) {
    stop("The 'sim_hyp' argument must be 'stationary' or 'nonstationary'.")
  }

  x <- x[is.finite(x)] # force the time series to be complete.


  m <- embed(x, 2)
  dat <- data.frame(y = m[, 1], ly = m[, 2])
  dat$t <- 1:length(dat$y)

  n <- length(dat$y)

  M1 <- lm(y ~ ly - 1, dat) # no drift, no trend
  M2 <- lm(y ~ ly, dat) # drift, no trend
  M3 <- lm(y ~ ly + t, dat) # drift and trend


  if(lag_short == TRUE) {
    q <- floor(4*(n/100)^0.25)
  } else {
    q <-  floor(12*(n/100)^0.25)
  }

  calc_pp <- function(mod, m) {
    index <- ifelse(m > 1, 2, 1)
    resids <- resid(mod)
    est_rho <- summary(mod)$coefficients[index,1]
    est_sig <- summary(mod)$coefficients[index,2]
    s2 <- sum(resids^2)/(n - m)
    gamma <- numeric(q + 1)
    for (i in 1:(q + 1)) {
      u <- embed(resids, i)
      gamma[i] = sum(u[,1]*u[,i])/n
    }
    lambda2 <- gamma[1] + 2*sum((1 - 1:q/(q + 1))*gamma[-1])
    z_rho <- n*(est_rho - 1) - n^2*est_sig^2/s2*(lambda2 - gamma[1])/2
    z_tau <- sqrt(gamma[1]/lambda2)*(est_rho - 1)/est_sig -
      (lambda2 - gamma[1])*n*est_sig/(2*sqrt(lambda2*s2))
    return(c(z_rho, z_tau))
  }


  Stats <- rbind(calc_pp(M1,1),
                 calc_pp(M2,2),
                 calc_pp(M3,3))


  Sims <- data.frame()
  if(sim_hyp == "stationary") {
    for (i in 1:n_sims) {
      fake_x <- rnorm(length(x))

      fm <- embed(fake_x, 2)
      fdat <- data.frame(y = fm[, 1], ly = fm[, 2])
      fdat$t <- 1:length(fdat$y)

      fn <- length(fdat$y)

      fM1 <- lm(y ~ ly - 1, fdat) # no drift, no trend
      fM2 <- lm(y ~ ly, fdat) # drift, no trend
      fM3 <- lm(y ~ ly + t, fdat) # drift and trend

      fakeStats <- rbind(calc_pp(fM1, 1),
                         calc_pp(fM2, 2),
                         calc_pp(fM3, 3))

      fakeStats <- data.frame(fakeStats)
      names(fakeStats) <- c("z_rho", "z_tau")
      fakeStats$sim <- i
      fakeStats$cat <- c("No Drift, No Trend", "Drift, No Trend",
                         "Drift and Trend")

      Sims <- rbind(Sims, fakeStats)


    }
  } else { # Assuming we're going to simulate non-stationary data.
    for (i in 1:n_sims) {

      fake_x <- rnorm(length(x))
      time <- 1:length(x)

      nd_nt <- cumsum(fake_x)
      d_nt <- cumsum(2 + fake_x)
      # d_t <- cumsum(rnorm(length(x), mean = time,
      #                     sd = sqrt(time)))

      #d_t <- 1/sqrt(time)*cumsum(fake_x)
      d_t <- as.vector(arima.sim(n = length(x), model = list(ar = .99)))

      # okie doke, this could get tedious.
      # Let's do it for the first one...

      fm_nd_nt <- embed(nd_nt, 2)
      fdat_nd_nt <- data.frame(y = fm_nd_nt[, 1], ly = fm_nd_nt[, 2])
      fdat_nd_nt$t <- 1:length(fdat_nd_nt$y)

      fn_nd_nt <- length(fdat_nd_nt$y)

      fM1 <- lm(y ~ ly - 1, fdat_nd_nt) # no drift, no trend


      # The second one...

      fm_d_nt <- embed(d_nt, 2)
      fdat_d_nt <- data.frame(y = fm_d_nt[, 1], ly = fm_d_nt[, 2])
      fdat_d_nt$t <- 1:length(fdat_d_nt$y)

      fn_d_nt <- length(fdat_d_nt$y)

      fM2 <- lm(y ~ ly, fdat_nd_nt) # drift, no trend


      # The third one...

      fm_d_t <- embed(d_t, 2)
      fdat_d_t <- data.frame(y = fm_d_t[, 1], ly = fm_d_t[, 2])
      fdat_d_t$t <- 1:length(fdat_d_t$y)

      fn_d_t <- length(fdat_d_t$y)

      fM3 <- lm(y ~ ly + t, fdat_d_t) # drift and trend

      fakeStats <- rbind(calc_pp(fM1, 1),
                         calc_pp(fM2, 2),
                         calc_pp(fM3, 3))

      fakeStats <- data.frame(fakeStats)
      names(fakeStats) <- c("z_rho", "z_tau")
      fakeStats$sim <- i
      fakeStats$cat <- c("No Drift, No Trend", "Drift, No Trend", "Drift and Trend")

      Sims <- rbind(Sims, fakeStats)




    }

  }

  attatt <- data.frame(lags = q,
                       sim_hyp = sim_hyp,
                       n_sims = n_sims,
                       n = length(x))

  output <- list("stats" = Stats,
                 "sims" = Sims,
                 "attributes" = attatt)

  class(output) <- c("spp_test")
  return(output)


}

