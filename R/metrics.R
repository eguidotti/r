#' Mean absolute percentage error
#' 
#' @param x a numeric vector of predicted values.
#' @param y a numeric vector of true values.
#' @param na.rm a logical evaluating to \code{TRUE} or \code{FALSE} indicating whether \code{NA} values should be stripped before the computation proceeds.
#'
#' @returns
#' \code{numeric} value
#'
#' @export
#' 
mape <- function(x, y, na.rm = FALSE){
  mean(abs((x-y)/y), na.rm = na.rm)
}

#' Root mean square error
#' 
#' @param x a numeric vector of predicted values.
#' @param y a numeric vector of true values.
#' @param na.rm a logical evaluating to \code{TRUE} or \code{FALSE} indicating whether \code{NA} values should be stripped before the computation proceeds.
#'
#' @returns
#' \code{numeric} value
#'
#' @export
#' 
rmse <- function(x, y, na.rm = FALSE){
  sqrt(mean((x-y)^2, na.rm = na.rm))
}
