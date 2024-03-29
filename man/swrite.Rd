% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.R
\name{swrite}
\alias{swrite}
\title{Safe CSV writer}
\usage{
swrite(x, file, key = NULL, na = "", create = FALSE, ...)
}
\arguments{
\item{x}{the data to write. It gets internally coerced to \code{data.table} preserving col names but not row names.}

\item{file}{the output file name.}

\item{key}{one or more column names to check duplicates.}

\item{na}{the string to use for missing values in the data.}

\item{create}{if \code{TRUE}, creates the directory if it does not exist.}

\item{...}{additional arguments passed to \code{\link[data.table]{fwrite}}.}
}
\value{
The file name.
}
\description{
Sorts by key and checks duplicates. Does not overwrite existing file.
}
