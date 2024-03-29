% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fm.R
\name{fm}
\alias{fm}
\title{Fama-Macbeth regression}
\usage{
fm(x, formula, by, weights = NULL, nmin = 0)
}
\arguments{
\item{x}{a \code{data.table}}

\item{formula}{an object of class \code{"\link[stats]{formula}"} (or one that can be coerced to that class): a symbolic description of the model to be fitted. See \code{\link[stats]{lm}}.}

\item{by}{character vector specifying the column that contains the time index for cross-sectional regressions.}

\item{weights}{an optional vector of weights to be used in the fitting process. See \code{\link[stats]{lm}}.}

\item{nmin}{the minimum number of observations for each cross-sectional regression.}
}
\value{
\code{data.table} of time-varying regression coefficients
}
\description{
Fama-Macbeth regression
}
