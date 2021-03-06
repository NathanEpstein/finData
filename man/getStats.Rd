% Generated by roxygen2 (4.1.0.9001): do not edit by hand
% Please edit documentation in R/main.R
\name{getStats}
\alias{getStats}
\title{easily get financial asset performance statistics}
\usage{
getStats(tickers, start, end)
}
\arguments{
\item{vector}{string formatted stock tickers}

\item{string}{data collection start date}

\item{string}{data collection end date}
}
\value{
list with matrices containing annualized return, annualized volatility, and maximum drawdown for each asset
}
\description{
easily get financial asset performance statistics
}
\examples{
getStats(c("FB","GS"),"2014-12-01","2015-01-01")
}

