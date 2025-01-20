#' Identify Optimal Lag Order Selection for (Augmented) Dickey-Fuller Tests
#'
#' @description \code{adf_lag_select()} runs a series of (Augmented) Dickey-Fuller
#' tests and returns information that may (or may not) be useful in identify
#' a potential lag order for unit root tests.
#'
#' @details
#'
#' This function removes missing values from the vector before calculating test
#' statistics.
#'
#' The lower bound lag order suggested by Schwert (1989) and the default suggested
#' by Said and Dickey (1984) do not meaningfully separate from each other until
#' the length of the series reaches 127. Under those conditions, if the `note`
#' column returned by this function for a finite series does not identify the
#' Said and Dickey (1984) default, but identifies the Schwert (1989) lower
#' bound, interpret the latter as the former.
#'
#' @return \code{adf_lag_select()} returns a list of length 3. The first element
#' in the list is a data frame of a series of (Augmented) Dickey-Fuller tests
#' for no-drift, no-trend. The second is a data frame of a series of (Augmented)
#' Dickey-Fuller tests for drift, no trend. The third is a data frame of a series
#' of (Augmented) Dickey-Fuller tests for a drift and trend. Each data frame has
#' the following columns communicating the following information.
#'
#' 1. The lag order
#' 2. The (A)DF statistic for the lag order.
#' 3. The Akaike information criterion for the model.
#' 4. Schwartz' (Bayesian) criteron for the model.
#' 5. The absolute value of the last lagged first difference in the model.
#' 6. The "modified" Akaike information criterion for the model.
#' 7. The "modified" Schwarz' (Bayesian) criterion for the model.
#' 8. A note indicating if the lag was suggested by Schwert (1989) or Said and Dickey (1984)
#'
#' @author Steven V. Miller
#'
#' @param x a vector
#' @param min_lag the minimum lag order to use. Defaults to 0.
#' @param max_lag the maximum lag order to use. Defaults to Schwert's (1989) upper lag.
#'
#' @examples
#'
#' x <- head(tbills$tb3m, 500)
#' adf_lag_select(x)
#'
#' @importFrom stats AIC
#' @importFrom stats BIC
#' @importFrom stats nobs
#' @importFrom stats var
#' @export

adf_lag_select <- function(x, min_lag = 0,
                       max_lag = NULL) {

  x <- x[is.finite(x)] # force the time series to be complete.


  max_lag <- ifelse(is.null(max_lag),
                   floor(12*(length(x)/100)^(.25)), # gonna go with Schwert (1989)
                   max_lag)

  schwert_ub <- floor(12*(length(x)/100)^(.25))
  schwert_lb <- floor(4*(length(x)/100)^(.25))
  sd84 <- floor((length(x)-1)^(1/3))

  # tibble(x = seq(1:500)) -> A
  # A %>% mutate(schwert_ub = floor(12*(x/100)^(.25)),
  #              schwert_lb = floor(4*(x/100)^(.25)),
  #              sd84 = floor((x-1)^(1/3))) %>%
  #   filter(x >= 120)

  # In the above, Said and Dickey's default doesn't meaningfully separate from
  # Schwert's (1989) lower bound until the length of the time series reaches 127.

  if (!is.null(max_lag) && (max_lag %% 1 != 0 || max_lag < 0)) {
    stop("max_lag must be a positive integer.")

  }


  if (!is.null(min_lag) && (min_lag %% 1 != 0 || min_lag < 0)) {
    stop("min_lag must be a positive integer.")

  }

  if (!(max_lag > min_lag)) {
    stop("max_lag should be greater than min_lag, otherwise this procedure doesn't make much sense.")
  }


  NDNT <- DNT <- DT <- NULL

  diff_x <- diff(x)
  n_diff_x <- length(diff_x)



  for (i in min_lag:max_lag) { # classic DF, where n_lags == 0

    if(i == 0) {
      m <- embed(diff_x, i+1)
      d_x_t <- m[,1]
      l1_x <- x[i+1:n_diff_x]
      time <- 1:length(d_x_t)

      M1 <- lm(d_x_t ~ l1_x - 1)    # no drift, no trend
      M2 <- lm(d_x_t ~ l1_x )       # drift, no trend
      M3 <- lm(d_x_t ~ l1_x + time) # drift and trend

    } else {

      j <- i + 1

      m <- embed(diff_x, j)
      d_x_t <- m[,1]
      l1_x <- x[j:n_diff_x]
      time <- 1:length(d_x_t)

      adf_diff_lags <- m[, 2:j]
      M1 <- lm(d_x_t ~ l1_x - 1 + adf_diff_lags)    # no drift, no trend
      M2 <- lm(d_x_t ~ l1_x + adf_diff_lags)        # drift, no trend
      M3 <- lm(d_x_t ~ l1_x + time + adf_diff_lags) # drift and trend

    }

    Stats <- rbind(summary(M1)$coefficients[1, 3],
                   summary(M2)$coefficients[2, 3 ],
                   summary(M3)$coefficients[2, 3 ])

    mAIC <- rbind(AIC(M1), AIC(M2), AIC(M3))
    mBIC <- rbind(BIC(M1), BIC(M2), BIC(M3))

    last_adf_lag <- rbind(
      summary(M1)$coefficients[nrow(summary(M1)$coefficients), 3],
      summary(M2)$coefficients[nrow(summary(M2)$coefficients), 3],
      summary(M3)$coefficients[nrow(summary(M3)$coefficients), 3]
    )

    mMAIC <- rbind(log(var(resid(M1)-1)) + (i)*(2/nobs(M1)),
                  log(var(resid(M2)-1)) + i*(2/nobs(M2)),
                  log(var(resid(M3)-1)) + i*(2/nobs(M3)))

    mMBIC <- rbind(log(var(resid(M1)-1)) + i*(log(nobs(M1))/nobs(M1)),
                   log(var(resid(M2)-1)) + i*(log(nobs(M2))/nobs(M2)),
                   log(var(resid(M3)-1)) + i*(log(nobs(M3))/nobs(M3)))


    NDNT <- rbind(NDNT, c(i, Stats[1], mAIC[1], mBIC[1], abs(last_adf_lag[1]),
                          mMAIC[1], mMBIC[1]))
    DNT <- rbind(DNT, c(i, Stats[2], mAIC[2], mBIC[2], abs(last_adf_lag[2]),
                        mMAIC[2], mMBIC[2]))
    DT <- rbind(DT, c(i, Stats[3], mAIC[3], mBIC[3], abs(last_adf_lag[3]), mMAIC[2], mMBIC[2]))

  }

  NDNT <- as.data.frame(NDNT)
  DNT <- as.data.frame(DNT)
  DT <- as.data.frame(DT)

  colnames(NDNT) <- colnames(DNT) <- colnames(DT) <- c("lag","tau","AIC", "BIC", "abs_llfd", "mAIC", "mBIC")
  #result

  NDNT$abs_llfd <- with(NDNT, ifelse(lag == 0, NA, abs_llfd))
  DNT$abs_llfd <- with(DNT, ifelse(lag == 0, NA, abs_llfd))
  DT$abs_llfd <- with(DT, ifelse(lag == 0, NA, abs_llfd))

  # NDNT$note <- with(NDNT, ifelse(lag == sd84, "Said and Dickey (1984) default",
  #                                      ifelse(lag == schwert_ub, "Schwert (1989) upper bound",
  #                                             ifelse(lag == schwert_lb, "Schwert (1989) lower bound", NA))))
  #
  # DNT$note <- with(DNT, ifelse(lag == sd84, "Said and Dickey (1984) default",
  #                                      ifelse(lag == schwert_ub, "Schwert (1989) upper bound",
  #                                             ifelse(lag == schwert_lb, "Schwert (1989) lower bound", NA))))
  #
  # DT$note <- with(DT, ifelse(lag == sd84, "Said and Dickey (1984) default",
  #                                      ifelse(lag == schwert_ub, "Schwert (1989) upper bound",
  #                                             ifelse(lag == schwert_lb, "Schwert (1989) lower bound", NA))))

  NDNT$note <- with(NDNT, ifelse(lag == schwert_lb, "Schwert (1989) lower bound",
                                       ifelse(lag == schwert_ub, "Schwert (1989) upper bound",
                                              ifelse(lag == sd84, "Said and Dickey (1984) default", NA))))

  DNT$note <- with(DNT, ifelse(lag == schwert_lb, "Schwert (1989) lower bound",
                                       ifelse(lag == schwert_ub, "Schwert (1989) upper bound",
                                              ifelse(lag == sd84, "Said and Dickey (1984) default", NA))))

  DT$note <- with(DT, ifelse(lag == schwert_lb, "Schwert (1989) lower bound",
                                       ifelse(lag == schwert_ub, "Schwert (1989) upper bound",
                                              ifelse(lag == sd84, "Said and Dickey (1984) default", NA))))


  return_me <- list(NDNT, DNT, DT)


  return(return_me)
}

