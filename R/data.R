#' The USD/SEK Exchange Rate
#'
#' A data frame on the USD/SEK exchange rate (i.e. how many Swedish crowns does
#' one dollar get you).
#'
#' @format A data frame with 3905 observations on the following 2 variables.
#' \describe{
#' \item{\code{date}}{a date}
#' \item{\code{close}}{the exchange rate at the close of trading}
#' }
#'
#' @details Data come by way of \pkg{quantmod}.

"USDSEK"

#' Quarterly disposable income and personal consumption expenditures in the United States
#'
#' A data frame on personal consumption expenditures and disposable personal income in the United States.
#'
#' @format A data frame with 299 observations on the following 4 variables.
#' \describe{
#' \item{\code{date}}{a date}
#' \item{\code{pce}}{personal consumption expenditures, seasonally adjusted, in billions}
#' \item{\code{dpi}}{disposable personal income, seasonally adjusted, in billions}
#' \item{\code{pira}}{personal income receipts on assets (personal dividend income), in billions}
#' \item{\code{cpiu}}{consumer price index for all urban consumers (all items in U.S. city average)}
#' }
#'
#' @details Data come by way of \pkg{fredr} call. Data are quarterly. Personal
#' consumption expenditure. disposable personal income, and personal dividend
#' income are not inflation-adjusted. The data on the consumer price index allow
#' for such inflation adjustment to "real" dollars based on researcher discretion.

"USDICE"

#' Daily maturity rates for U.S. Treasury Bills
#'
#' A data frame on daily (when applicable/available) U.S. Treasury Bill rates.
#'  These are the yield received for investing in a government-issued treasury
#'  security that has a maturity of a given period of time (three months, six
#'  months, or a year).
#'
#' @format A data frame with 17,741 observations on the following 4 variables.
#' \describe{
#' \item{\code{date}}{a date}
#' \item{\code{tb3m}}{the three-month treasury bill rate}
#' \item{\code{tb6m}}{the six-month treasury bill rate}
#' \item{\code{tb1y}}{the one-year treasury bill rate}
#' }
#'
#' @details Data come by way of \pkg{fredr} call. The one-year (52-week) treasury
#' bill rate was discontinued in 2001 and re-introduced in 2008. Be mindful of
#' that gap in the series.

"tbills"

#' Dynamic Foreign Policy Behavior (COPDAB)
#'
#' A data frame on monthly dyadic foreign policy behavior from 1948 to 1978 for
#'  select dyads, using COPDAB data. The data offer the opportunity for a basic
#'  replication of Lebo and Moore (2003).
#'
#' @format A data frame with 372 observations on the following 12 variables.
#' \describe{
#' \item{\code{ym}}{a year-month indicator, in the format of YYMM}
#' \item{\code{eg2is}}{an estimate of the dyadic foreign policy behavior of Egypt to Israel}
#' \item{\code{is2eg}}{an estimate of the dyadic foreign policy behavior of Israel to Egypt}
#' \item{\code{us2ussr}}{an estimate of the dyadic foreign policy behavior of the U.S. to the Soviet Union}
#' \item{\code{ussr2us}}{an estimate of the dyadic foreign policy behavior of the Soviet Union to the U.S.}
#' \item{\code{us2fra}}{an estimate of the dyadic foreign policy behavior of the U.S. to France}
#' \item{\code{fra2us}}{an estimate of the dyadic foreign policy behavior of France to the U.S.}
#' \item{\code{us2is}}{an estimate of the dyadic foreign policy behavior of the U.S. to Israel}
#' \item{\code{is2us}}{an estimate of the dyadic foreign policy behavior of Israel to the U.S.}
#' \item{\code{suez}}{a dummy variable indicating if the observation corresponds with the Suez Crisis}
#' \item{\code{sixday}}{a dummy variable indicating if the observation corresponds with the Six-Day War}
#' \item{\code{yomk}}{a dummy variable indicating if the observation corresponds with the Yom Kippur War}
#' }
#'
#' @details Lebo and Moore (2003, 22-24) will offer more context about how these
#' variables are coded. Important details for replication from scratch are assuredly
#' lost to history, but the authors are clear about what they're doing and the
#' procedure they used to weight fundamentally ordinal data to create some kind
#' of continuous estimate. Context clues offer more information as well.
#'
#' @references
#'
#' Lebo, Matthew J. and Will H. Moore. 2003. "Dynamic Foreign Policy Behavior."
#' *Journal of Conflict Resolution* 47(1): 13-32.
#'
"exCopdab"

#' Quarterly Money Demand in the United States
#'
#' A data frame of quarterly indicators useful for modeling the demand for money
#'  in the United States. Data go from the first quarter of 1960 to the third
#'  quarter of 2024.
#'
#' @format A data frame with 259 observations on the following 6 variables.
#' \describe{
#' \item{\code{date}}{a date}
#' \item{\code{m1}}{so-called 'narrow' money (M1) in supply, in billions, not seasonally adjsuted}
#' \item{\code{m2}}{monetary supply (M2), in billions, not seasonally adjusted}
#' \item{\code{gnpdef}}{an implicit price deflator for gross national product (index, 2017 = 100)}
#' \item{\code{ffer}}{the federal funds effective rate}
#' \item{\code{rgnp}}{real gross national product (in 2017 dollars)}
#' \item{\code{pcepi}}{the chain-type price index (index, 2017 == 100)}
#' }
#'
#' @details Data come by way of \pkg{fredr} call. Be mindful of changes in the
#' definition of the money supply, especially as they manifest in May 2020.
#' Subject domain expertise goes a long way here. The "M2" indicator is the "M1"
#' indicator with small-time deposits that are "close substitutes" for M1.
#'

"money_demand"
