#' Convert a data frame to a LaTeX table
#' 
#' Converts a data frame to LaTeX or HTML.
#' If \code{bold.which} is given it should be a logical matrix specifying bold cells.
#' Otherwise: in each column or row with numeric data, the maximum or minimum
#' value is set bold; \code{bold.max} can have entries for each column/row, \code{NA} means skip.
#' 
#' @param x a data frame.
#' @param file using this argument, the resulting table is written to a file rather than to the R prompt. The file name can be specified as a character string.
#' @param caption argument passed to \link[xtable]{xtable}.
#' @param label argument passed to \link[xtable]{xtable}.
#' @param align argument passed to \link[xtable]{xtable}.
#' @param digits argument passed to \link[xtable]{xtable}.
#' @param display argument passed to \link[xtable]{xtable}.
#' @param auto argument passed to \link[xtable]{xtable}.
#' @param bold.which logical matrix specifying bold cells.
#' @param bold.each axis along which to compute the max/min. Possible values for \code{each} are "row" or "column".
#' @param bold.max whether to boldify maximum values (\code{TRUE}) or minimum values (\code{FALSE}).
#' @param NA.string string to be used for missing values in table entries.
#' @param type type of table to produce. Possible values for type are "latex" or "html".
#' @param sanitize.text.function argument passed to \link[xtable]{print.xtable}.
#' @param sanitize.rownames.function argument passed to \link[xtable]{print.xtable}.
#' @param sanitize.colnames.function argument passed to \link[xtable]{print.xtable}.
#' @param ... additional arguments passed to \link[xtable]{print.xtable}.
#' 
#' @examples
#' x <- tail(iris[,1:4])
#' textab(x)
#' textab(x, bold.each = "column", bold.max = c(FALSE, NA, NA, TRUE))
#' textab(x, bold.each = "row", bold.max = FALSE)
#' textab(x, bold.which = x >= 6.5)
#' 
#' @note
#' The code is adopted from \url{https://gist.github.com/floybix/452201}
#' 
#' @import xtable
#' 
#' @export
#' 
textab <- function(
    x, 
    file = "",
    type = c("latex", "html"),
    caption = NULL,
    label = NULL,
    align = NULL,
    digits = NULL,
    display = NULL,
    auto = FALSE,
    bold.which = NULL, 
    bold.each = c("column", "row"), 
    bold.max = NA,
    NA.string = "", 
    sanitize.text.function = force,
    sanitize.rownames.function = NULL,
    sanitize.colnames.function = NULL, 
    ...
){

  x <- xtable(
    x, 
    caption = caption,
    label = label,
    align = align,
    digits = digits,
    display = display,
    auto = auto
  )  

  max <- bold.max
  each <- match.arg(bold.each)
  type <- match.arg(type)
  digits <- rep(digits(x), length = ncol(x)+1)
  
  if (!is.null(bold.which)) {
    stopifnot(nrow(bold.which) == nrow(x))
    stopifnot(ncol(bold.which) == ncol(x))
    boldmatrix <- bold.which
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
