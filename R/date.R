#' Convert YYYYMM to Date
#' 
#' @param x \code{integer} vector.
#' @param day \code{character} specifying the day of the month.
#' @param ... additional arguments passed to \link[base]{as.Date}.
#' 
#' @returns 
#' vector of \code{Date}s
#' 
#' @export
#' 
as.Date.integer <- function(x, day = "01", ...){
  as.Date(paste0(x, day), format = "%Y%m%d", ...)  
}
  