% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factors.R
\name{factorSMB}
\alias{factorSMB}
\title{Factor: Small minus Big}
\usage{
factorSMB(me, beme, ret, by, weight = 1, nyse = TRUE)
}
\arguments{
\item{me}{the market equity as of last June}

\item{beme}{the book to market ratio}

\item{ret}{the return at time T+1}

\item{by}{the time index}

\item{weight}{the weight for value-weighted portfolios}

\item{nyse}{logical vector representing whether the stock is listed in NYSE. It is used for the computation of breakpoints.}
}
\value{
Factor returns for each \code{by}-time
}
\description{
Factor: Small minus Big
}
