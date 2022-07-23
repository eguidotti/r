#' Fama-Macbeth regression
#' 
#' @param x a \code{data.table}
#' @param formula an object of class \code{"\link[stats]{formula}"} (or one that can be coerced to that class): a symbolic description of the model to be fitted. See \code{\link[stats]{lm}}.
#' @param by character vector specifying the column that contains the time index for cross-sectional regressions.
#' @param weights an optional vector of weights to be used in the fitting process. See \code{\link[stats]{lm}}. 
#' @param nmin the minimum number of observations for each cross-sectional regression.
#' 
#' @returns 
#' \code{data.table} of time-varying regression coefficients
#' 
#' @export
#' 
fm <- function(x, formula, by, weights = NULL, nmin = 0){
  
  if(!is.null(weights))
    weights <- as.name(weights)
  
  reg <- function(formula, data){
    
    mod <- try(silent = TRUE, eval(bquote(
      lm(formula, 
         data = data, 
         weights = .(weights), 
         na.action = na.omit)
    )))
    
    if(class(mod)=="try-error" || nobs(mod) < nmin) 
      return(NULL)
    
    return(as.list(c("NOBS" = nobs(mod), mod$coefficients)))
    
  }
  
  return(x[, reg(formula, data = .SD), by = by])
  
}

#' Convert Fama-Macbeth regression to TexReg object
#' 
#' @param x the output of \link{fm}.
#' 
#' @returns 
#' output of \code{\link[texreg]{createTexreg}}
#' 
#' @export
#' 
fm2texreg <- function(x){
  
  x.coef <- x[,-(1:2)]
  coef <- apply(x.coef, 2, mean)
  se <- apply(x.coef, 2, sd) / sqrt(nrow(x.coef))
  
  texreg::createTexreg(
    coef.names = colnames(x.coef),
    coef = coef, 
    se = se, 
    pvalues = 2 * pnorm(abs(coef/se), lower.tail = FALSE), 
    gof.names = c("N. Periods", "Min Obs.", "Median Obs.", "Max Obs."), 
    gof = c(nrow(x.coef), min(x$NOBS), median(x$NOBS), max(x$NOBS)), 
    gof.decimal = c(FALSE, FALSE, FALSE, FALSE)
  )
  
}

#' Convert Fama-Macbeth regression to ggplot object
#' 
#' @param x the output of \link{fm}.
#' @param variable the name of the column to plot.
#' @param n number of periods for rolling averages.
#' @param ci confidence intervals to plot.
#' @param fill color of the confidence intervals.
#' @param alpha transparency of confidence intervals.
#' @param gg a \code{\link[ggplot2]{ggplot}} object to initialize the plot.
#' @param ... additional arguments passed to \code{\link[ggplot2]{geom_line}} aesthetic mappings.
#' 
#' @returns 
#' \code{\link[ggplot2]{ggplot}} object
#' 
#' @import ggplot2
#' 
#' @export
#' 
fm2ggplot <- function(x, variable, n = 1, ci = 0.95, fill = "grey", alpha = 0.1, gg = ggplot(), ...){
  
  cols <- c(1, which(colnames(x) == variable))
  dt <- x[order(x[,1]), ..cols]
  
  x <- dt[[1]]
  y <- dt[[2]]
  
  y.min <- y.max <- y.mean <- frollmean(y, n = n, na.rm = TRUE)
  if(n > 1){
    
    y.num <- frollsum(!is.na(y), n = n)
    y.std <- frollapply(y, n = n, sd, na.rm = TRUE) / sqrt(y.num)
    y.min <- y.mean + qnorm(p = (1-ci) / 2) * y.std
    y.max <- y.mean - qnorm(p = (1-ci) / 2) * y.std
    
    nas <- 1:n-1
    x <- x[-nas]
    y.min <- y.min[-nas]
    y.max <- y.max[-nas]
    y.mean <- y.mean[-nas]
    
  }
  
  gg +
    geom_line(aes(x = x, y = y.mean, ...)) + 
    geom_ribbon(aes(x = x, ymin = y.min, ymax = y.max), fill = fill, alpha = alpha)
  
}
