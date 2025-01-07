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
#' \item{\code{cpiu}}{consumer price index for all urban consumers (all items in U.S. city average)}
#' }
#'
#' @details Data come by way of \pkg{fredr} call. Data are quarterly. Personal
#' consumption expenditure and disposable personal income are not inflation-adjusted,
#' though the data on the consumer price index allow for such inflation adjustment
#' to "real" dollars based on researcher discretion.

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
