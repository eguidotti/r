#' Prints an xtable in bold by 
#' 
#' If 'which' is given it should be a logical matrix specifying bold cells.
#' Otherwise: in each column or row with numeric data, the maximum or minimum
#' value is set bold; 'max' can have entries for each column/row, NA means skip.
#' 
#' @param x an object of class \link[xtable]{xtable}.
#' @param which logical matrix specifying bold cells.
#' @param each axis along which to compute the max/min. Possible values for \code{each} are "row" or "column".
#' @param max \code{TRUE} for max and \code{FALSE} for min.
#' @param NA.string string to be used for missing values in table entries.
#' @param type type of table to produce. Possible values for type are "latex" or "html".
#' @param sanitize.text.function argument passed to \link[xtable]{print.xtable}.
#' @param sanitize.rownames.function argument passed to \link[xtable]{print.xtable}.
#' @param sanitize.colnames.function argument passed to \link[xtable]{print.xtable}.
#' @param ... additional arguments passed to \link[xtable]{print.xtable}.
#' 
#' @examples
#' library(xtable)
#' x <- tail(iris[,1:4])
#' printbold(xtable(x))
#' printbold(xtable(x), each = "column", max = c(F, NA, NA, T, NA))
#' printbold(xtable(x), each = "row", max = FALSE)
#' printbold(xtable(x), x >= 6.5, type = "html")
#' 
#' @note
#' The code is adopted from \url{https://gist.github.com/floybix/452201}
#' 
#' @export
#' 
printbold <- function(
    x, 
    which = NULL, 
    each = c("column", "row"), 
    max = TRUE,
    NA.string = "", 
    type = c("latex", "html"),
    sanitize.text.function = force,
    sanitize.rownames.function = NULL,
    sanitize.colnames.function = NULL, 
    ...
){
  
  stopifnot(inherits(x, "xtable"))
  each <- match.arg(each)
  type <- match.arg(type)
  digits <- rep(digits(x), length = ncol(x)+1)
  
  if (!is.null(which)) {
    stopifnot(nrow(which) == nrow(x))
    stopifnot(ncol(which) == ncol(x))
    boldmatrix <- which
  } else {
    boldmatrix <- matrix(FALSE, ncol = ncol(x), nrow = nrow(x))
    ## round values before calculating max/min to avoid trivial diffs
    for (i in 1:ncol(x)) {
      if (!is.numeric(x[,i])) next
      x[,i] <- round(x[,i], digits = digits[i+1])
    }
    if (each == "column") {
      max <- rep(max, length = ncol(x))
      for (i in 1:ncol(x)) {
        xi <- x[,i]
        if (!is.numeric(xi)) next
        if (is.na(max[i])) next
        imax <- max(xi, na.rm = TRUE)
        if (!max[i])
          imax <- min(xi, na.rm = TRUE)
        boldmatrix[xi == imax, i] <- TRUE
      }
    } else if (each == "row") {
      max <- rep(max, length = nrow(x))
      for (i in 1:nrow(x)) {
        xi <- x[i,]
        ok <- sapply(xi, is.numeric)
        if (!any(ok)) next
        if (is.na(max[i])) next
        imax <- max(unlist(xi[ok]), na.rm = TRUE)
        if (!max[i])
          imax <- min(unlist(xi[ok]), na.rm = TRUE)
        whichmax <- sapply(xi, identical, imax)
        boldmatrix[i, whichmax] <- TRUE
      }
    }
  }
  
  ## need to convert to character
  ## only support per-column formats, not cell formats
  display <- rep(display(x), length = ncol(x)+1)
  for (i in 1:ncol(x)) {
    if (!is.numeric(x[,i])) next
    ina <- is.na(x[,i])
    x[,i] <- formatC(x[,i], digits = digits[i+1],
                     format = display[i+1])
    x[ina, i] <- NA.string
    display(x)[i+1] <- "s"
    ## embolden
    yes <- boldmatrix[,i]
    if (type == "latex") {
      x[yes,i] <- paste("\\textbf{", x[yes,i], "}", sep = "")
    } else {
      x[yes,i] <- paste("<strong>", x[yes,i], "</strong>", sep = "")
    }
  }
  
  print(x, ..., type = type, NA.string = NA.string,
        sanitize.text.function = sanitize.text.function,
        sanitize.rownames.function = sanitize.rownames.function,
        sanitize.colnames.function = sanitize.colnames.function)
  
}
