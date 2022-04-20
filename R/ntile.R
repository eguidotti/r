#' Compute the n-tiles of x based on the breakpoints of y
#'
#' @param x a numeric vector.
#' @param n integer (e.g., \code{10} for deciles) or vector of probabilities (e.g. \code{c(0, 0.3, 0.7, 1)} to split in quantiles 0-30, 30-70, 70-100)
#' @param y a numeric vector to compute the breakpoints.
#'
#' @returns
#' \code{integer} vector
#'
#' @export
#'
ntile <- function(x, n, y = x){
  if(any(n < 1)) probs <- n
  else probs <- seq(0, 1, length.out = n+1)
  breaks <- quantile(y, probs = probs, na.rm = TRUE)
  as.integer(cut(x, breaks = breaks, include.lowest = TRUE))
}
