#' Winsorize a vector
#'
#' @param x a numeric vector.
#' @param pmin the lowest percentile.
#' @param pmax the upper percentile.
#'
#' @returns
#' winsorized \code{numeric} vector
#' 
#' @export
#'
winsorize <- function(x, pmin = 0.01, pmax = 0.99) {
  y <- sort(na.omit(x))
  n <- length(y)

  ibot <- floor(pmin * n) + 1
  itop <- ceiling(pmax * n)

  xbot <- y[ibot]
  xtop <- y[itop]

  ifelse(x <= xbot, xbot, ifelse(x >= xtop, xtop, x))
}
