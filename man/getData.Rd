% Generated by roxygen2 (4.1.0.9001): do not edit by hand
% Please edit documentation in R/main.R
\name{getData}
\alias{getData}
\title{easily get financial price data}
\usage{
getData(tickers, start, end)
}
\arguments{
\item{vector}{string formatted stock tickers}

\item{string}{data collection start date}

\item{string}{data collection end date}
}
\value{
list of matrices with price/trading data
}
\description{
easily get financial price data
}
\examples{
getData(c("FB","GS"),"2014-12-01","2015-01-01")
}

